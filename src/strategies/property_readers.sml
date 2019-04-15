(*
The following functions handle how properties are generated from the "property"
table. For example, they key "operators" then lists a collection of operators.
To read this, we use the "readCollection" function and will prepend each of the
operators with the string "op-". Compare this with basic labels, which simply
return the one thing that is there, in a list, ready to prepend to. Bools are
simplest, either returning an empty list (false), or a list containing the empty
string (true) to generate either the key, or nothing, as a property.
A concrete example: From the table
    operators        +, -, *, \sqrt
    sentential       true
    logic-power      2
we would generate the properties
    op-+, op--, op-*, op-\sqrt, sentential, logic-power-2
*)

import "strategies.property_importance";
import "strategies.property_tables";

structure PropertyReader =
struct

(*
To properly handle all the different tokens, it can be useful to give them
'types'. We support a simple subset of the Standard ML type language: ground
types, variables, pairs, functions, and unary type constructors. Variables are
denoted by a preceding quote (e.g., 'a).
For example,
    ('a list * int) tree -> (('a * int) -> int) -> int
would parse to
    Function(
        Constructor(
            "tree",
            Pair(
                Constructor("list", Var "a"),
                Ground "int")),
        Function(
            Function(
                Pair(Var "a", Ground "int"),
                Ground "int"),
            Ground "int"))
Note that * and -> have equal precedence and are right associative, so
    'a * int -> int
parses differently to
    ('a * int) -> int
The first is equivalent to 'a * (int -> int). This is a deviation from the SML
standard, but is much easier to parse.
*)
datatype Type = Ground of string
              | Var of string
              | Pair of Type * Type
              | Function of Type * Type
              | Constructor of string * Type;

fun typeToString (Ground s) = s
  | typeToString (Var s) = "'" ^ s
  | typeToString (Pair (t,u)) = "(" ^ (typeToString t) ^ " * " ^ (typeToString u) ^ ")"
  | typeToString (Function (t, (Function (u,v)))) =
    (typeToString t) ^ " -> (" ^ (typeToString (Function (u,v))) ^ ")"
  | typeToString (Function (t,u)) = (typeToString t) ^ " -> " ^ (typeToString u)
  | typeToString (Constructor (s,t)) = (typeToString t) ^ " " ^ s;

fun typeDebugString (Ground s) = "Ground \"" ^ s ^ "\""
  | typeDebugString (Var s) = "Var \"" ^ s ^ "\""
  | typeDebugString (Pair (t,u)) = "Pair(" ^ (typeDebugString t) ^ ", "
                                 ^ (typeDebugString u) ^ ")"
  | typeDebugString (Function (t,u)) = "Function(" ^ (typeDebugString t) ^ ", "
                                     ^ (typeDebugString u) ^ ")"
  | typeDebugString (Constructor (s,t)) = "Constructor(\"" ^ s ^ "\", " ^ (typeDebugString t) ^ ")";

fun vartype str =
    let
        fun readExactly [] _ = true
          | readExactly _ [] = false
          | readExactly (x::xs) (c::cs) =
            if x = c then readExactly xs cs
            else false;
        fun collectUntil b [] xs = (String.implode (List.rev xs), [])
          | collectUntil b (c::cs) xs =
            if (b c) then (String.implode (List.rev xs), c::cs)
            else collectUntil b cs (c::xs);
        fun isInvalid c = not (Char.isAlpha c);
        fun typeTokens [] out = List.rev out
          | typeTokens (c::cs) out =
            if c = #"'" then let val (tok, cs') = collectUntil (isInvalid) cs []
                             in typeTokens cs' ((Var tok)::out) end
            else if c = #"*" then typeTokens cs ((Ground "*")::out)
            else if readExactly [#"-", #">"] (c::cs) then
                typeTokens (List.drop (cs, 1)) ((Ground "->")::out)
            else if Char.isSpace c then typeTokens cs out
            else if c = #"(" then typeTokens cs ((Ground "(")::out)
            else if c = #")" then typeTokens cs ((Ground ")")::out)
            else let val (tok, cs') = collectUntil (isInvalid) (c::cs) []
                 in typeTokens cs' ((Ground tok)::out) end;

        fun toCloseParen [] _ = raise Match
          | toCloseParen ((Ground "(")::ys) xs =
            let
                val (innerToks, rest) = toCloseParen ys [];
                val innerType = readType innerToks
            in
                toCloseParen rest (innerType::xs)
            end
          | toCloseParen ((Ground ")")::ys) xs = (List.rev xs, ys)
          | toCloseParen (c::cs) xs = toCloseParen cs (c::xs)
        and readType [] = raise Match
          | readType [x] = x
          | readType ((Ground "(")::cs) =
            let val (ins, outs) = toCloseParen cs []
            in readType ((readType ins)::outs) end
          | readType (c::(Ground "*")::cs) =
            let val left = c;
                val right = readType cs;
            in Pair (left, right) end
          | readType (c::(Ground "->")::cs) =
            let val left = c;
                val right = readType cs;
            in Function (left, right) end
          | readType (x::(Ground y)::cs) = readType ((Constructor (y, x))::cs)
          | readType stuff =
            let
                val _ = Logging.write((listToString typeToString stuff) ^ "\n");
            in raise Match end;

        val chars = String.explode str;
        val tokens = typeTokens chars [];
        val finalType = readType tokens;
    in
        finalType
    end;

fun bool str = if ((String.implode (map Char.toLower (String.explode str))) = "true")
               then [""] else [];

fun label str = [str];

fun collection str = if str = "NONE" then []
                     else map stringTrim (String.tokens (fn c => c = #",") str);

fun dimension str =
    let
        fun parseDimProps s = if s = "{}" then []
                              else let
                                  fun dropEnds [] = []
                                    | dropEnds [x] = []
                                    | dropEnds [x, y] = []
                                    | dropEnds (x::xs) = List.rev (List.tl (List.rev xs));
                                  val s' = String.implode (dropEnds (String.explode s));
                              in
                                  map stringTrim (String.tokens (fn c => c = #";") s')
                              end;
        fun createPairs dimval =
            let
                val parts = map stringTrim (String.tokens (fn c => c = #":") dimval);
            in
                case parts of
                    [x, y] => (x, parseDimProps y)
                  | _ => raise PropertyTables.TableError
                               ("Unable to read dimensions from " ^ dimval)
            end;
        val dimensions = collection str;
        val dimensionsWithValues = map createPairs dimensions;
        val dimensionsSplitOut = map (fn (x, y) =>
                                         map (fn z => x ^ "-" ^ z) y)
                                     dimensionsWithValues;
        val dimensionsNoLabels = map (fn (x, y) =>
                                         map (fn z:string => z) y)
                                     dimensionsWithValues;
    in
        List.foldr (fn (a, b) => a @ b) [] (dimensionsSplitOut @ dimensionsNoLabels)
    end;
end;

let
    open Importance;
    fun stripImportance vals = map (fn (l, (f, p, i)) => (l, (f, p))) vals;

    val RSProperties = [
        ("mode",
         (PropertyReader.collection, "mode-")),
        ("grammar-imports",
         (PropertyReader.collection, "import-")),
        ("grammatical-complexity",
         (PropertyReader.label,  "grammatical-complexity-")),
        ("rigorous",
         (PropertyReader.bool, "rigorous")),
        ("knowledge-manipulation-system",
         (PropertyReader.bool, "knowledge-manipulation-system")),
        ("facts",
         (PropertyReader.collection, "fact-")),
        ("fact-imports",
         (PropertyReader.collection, "import-")),
        ("tactics",
         (PropertyReader.collection, "tactic-")),
        ("logic-power",
         (PropertyReader.label, "logic-power-")),
        ("standard-accessibility-manipulations",
         (PropertyReader.collection, "accessible-manipulation-")),
        ("accessible-facts",
         (PropertyReader.bool, "accessible-facts")),
        ("accessible-tactics",
         (PropertyReader.bool, "accessible-tactics")),
        ("accessible-grammatical-constructors",
         (PropertyReader.bool, "accessible-grammatical-constructors")),
        ("editable-external-memory",
         (PropertyReader.bool, "editable-external-memory")),
        ("physical-dimension-use",
         (PropertyReader.dimension, "dimension-use-")),
        ("grammatical-dimensionality",
         (PropertyReader.label, "grammatical-dimensionality-")),
        ("grammatical-granularity",
         (PropertyReader.label, "grammatical-granularity-")),
        ("mean-branching-factor",
         (PropertyReader.label, "mean-branching-factor-")),
        ("mean-solution-depth",
         (PropertyReader.label, "mean-solution-depth-")),
        ("pr-distinct-state-change",
         (PropertyReader.label, "pr-distinct-state-change-")),
        ("pr-valid-state-change",
         (PropertyReader.label, "pr-valid-state-change-")),
       ("types",
         (PropertyReader.collection, "type-")),
       ("tokens",
         (PropertyReader.collection, "token-")),
       ("operators",
         (PropertyReader.collection, "token-")),
       ("relations",
         (PropertyReader.collection, "token-")),
       ("patterns",
         (PropertyReader.collection, "pattern-"))
    ];
    val QProperties = [
        ("error-allowed",
         (PropertyReader.label, "error-allowed-", High)),  (*previously rigorous*)
        ("answer-type",
         (PropertyReader.collection, "type-", High)), (*previously question-value-type*)
        ("instrumental-tokens",
         (PropertyReader.collection, "token-", Medium)),
        ("instrumental-type",
         (PropertyReader.collection, "type-", Medium)),
        ("instrumental-patterns",
         (PropertyReader.collection, "pattern-", Medium)),
        ("instrumental-facts",
         (PropertyReader.collection, "fact-", Medium)),
        ("instrumental-tactics",
         (PropertyReader.collection, "tactic-", Medium)),
        ("relevant-tokens",
         (PropertyReader.collection, "token-", Low)),
        ("relevant-related-tokens",
         (PropertyReader.collection, "token-", Low)),
        ("num-statements",
         (PropertyReader.label, "num-statements-", Zero)),
        ("num-tokens",
         (PropertyReader.label, "num-tokens-", Zero)),
        ("num-distinct-tokens",
         (PropertyReader.label, "num-distinct-tokens-", Zero)),
        ("noise-tokens",
         (PropertyReader.collection, "token-", Noise)),
        ("noise-related-tokens",
         (PropertyReader.collection, "token-", Noise))
    ];
    val QandRSProperties = [
    ];
in
    PropertyTables.setQGenerators
        (QProperties @ QandRSProperties);
    PropertyTables.setRSGenerators
        (RSProperties @ (stripImportance QandRSProperties))
end;
