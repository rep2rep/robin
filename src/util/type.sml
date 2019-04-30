import "util.logging";

signature TYPE =
sig
  type T
  val outputArity : T -> int;
  val inputArity : T -> int;
  val order : T -> int;
  val match : T -> T -> bool;
  val compare : T * T -> order;
  val toString : T -> string;
  val toDebugString : T -> string;
  val fromString : string -> T;
end;

structure Type : TYPE =
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
datatype T = Ground of string
           | Var of string
           | Pair of T * T
           | Function of T * T
           | Constructor of string * T;

fun dimensionality (Pair (s,t)) = dimensionality s + dimensionality t
  | dimensionality _ =  1

(* how many arguments do you need to plug in before you get a non-functional value? *)
fun inputArity (Function (s,t)) = dimensionality s + inputArity t
  | inputArity (Pair (s,t)) = inputArity s + inputArity t
  | inputArity _ = 0

(* how many values do you get once you've plugged in as many arguments as you can?*)
fun outputArity (Pair (s,t)) = outputArity s + outputArity t
  | outputArity (Function (s,t)) = outputArity t
  | outputArity _ = 1

(* order is the depth of nested Function constructors *)
fun order (Pair (s,t)) = Int.max(order s, order t)
  | order (Function (s,t)) = 1 + Int.max(order s, order t)
  | order _ = 0

fun occurs x (Ground _) = false
  | occurs x (Var y) = (x = y)
  | occurs x (Pair (s,t)) = (occurs x s orelse occurs x t)
  | occurs x (Function (s,t)) = (occurs x s orelse occurs x t)
  | occurs x (Constructor (_,t)) = occurs x t;

fun match (Ground s) (Ground s') = (s = s')
(*  | match (Var x) (Var y) = true*)
  | match (Var x) t = true (*not (occurs x t)*)
  | match t (Var x) = true (*not (occurs x t)*)
  | match (Pair(s,t)) (Pair(s',t')) = (match s s') andalso (match t t')
  | match (Function(s,t)) (Function(s',t')) = (match s s') andalso (match t t')
  | match (Constructor(s,t)) (Constructor(s',t')) = (s = s') andalso (match t t')
  | match _ _ = false;

(*A lexicographic order for types, to use in dictionaries*)
fun compare (Ground s, Ground s') = String.compare (s,s')
  | compare (Ground _, _) = LESS
  | compare (_, Ground _) = GREATER
  | compare (Var s, Var s') = String.compare (s,s')
  | compare (Var _, _) = LESS
  | compare (_, Var _) = GREATER
  | compare (Pair (t,u), Pair (t',u')) = let val c = compare (t,t')
                                         in if c = EQUAL then compare (u,u') else c
                                         end
  | compare (Pair _, _) = LESS
  | compare (_, Pair _) = GREATER
  | compare (Function (t,u), Function (t',u')) = let val c = compare (t,t')
                                                 in if c = EQUAL then compare (u,u') else c
                                                 end
  | compare (Function _, _) = LESS
  | compare (_, Function _) = GREATER
  | compare (Constructor (s,t), Constructor (s',t')) = let val c = String.compare (s,s')
                                                       in if c = EQUAL then compare (t,t') else c
                                                       end
  ;

fun toString (Ground s) = s
  | toString (Var s) = "'" ^ s
  | toString (Pair (t,u)) = "(" ^ (toString t) ^ " * " ^ (toString u) ^ ")"
  | toString (Function (t, (Function (u,v)))) =
    (toString t) ^ " -> (" ^ (toString (Function (u,v))) ^ ")"
  | toString (Function (t,u)) = (toString t) ^ " -> " ^ (toString u)
  | toString (Constructor (s,t)) = (toString t) ^ " " ^ s;

fun toDebugString (Ground s) = "Ground \"" ^ s ^ "\""
  | toDebugString (Var s) = "Var \"" ^ s ^ "\""
  | toDebugString (Pair (t,u)) = "Pair(" ^ (toDebugString t) ^ ", "
                                 ^ (toDebugString u) ^ ")"
  | toDebugString (Function (t,u)) = "Function(" ^ (toDebugString t) ^ ", "
                                     ^ (toDebugString u) ^ ")"
  | toDebugString (Constructor (s,t)) = "Constructor(\"" ^ s ^ "\", " ^ (toDebugString t) ^ ")";

fun fromString str =
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
                val _ = Logging.write((listToString toString stuff) ^ "\n");
            in raise Match end;

        val chars = String.explode str;
        val tokens = typeTokens chars [];
        val finalType = readType tokens;
    in
        finalType
    end;

  end;
