import "util.logging";

signature TYPE =
sig
    exception ParseError;
    type T;
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
Note that constructors bind tighter than pairs, and pairs bind tighter than
function application. Thus the correct parse of
    'a list * int -> real
is
    Function(
        Pair(
            Constructor ("list", Var "a"),
            Ground "int"),
        Ground "real")
*)

exception ParseError;

datatype T = Ground of string
           | Var of string
           | Pair of T * T
           | Function of T * T
           | Constructor of string * T;

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
                             in typeTokens cs' (("'" ^ tok)::out) end
            else if c = #"*" then typeTokens cs ("*"::out)
            else if readExactly [#"-", #">"] (c::cs) then
                typeTokens (List.drop (cs, 1)) ("->"::out)
            else if Char.isSpace c then typeTokens cs out
            else if c = #"(" then typeTokens cs ("("::out)
            else if c = #")" then typeTokens cs (")"::out)
            else let val (tok, cs') = collectUntil (isInvalid) (c::cs) []
                 in typeTokens cs' (tok::out) end;

        fun expect s [] = raise ParseError
          | expect s (x::xs) = if s = x then (Ground x, xs) else raise ParseError

        fun toCloseParen [] _ = raise ParseError
          | toCloseParen ("("::ys) xs =
            let
                val (innerToks, resta) = toCloseParen ys [];
            in
                toCloseParen resta (innerToks @ xs)
            end
          | toCloseParen (")"::ys) xs = (List.rev xs, ys)
          | toCloseParen (c::cs) xs = toCloseParen cs (c::xs)
        and readGround [] = raise ParseError
          | readGround (x::xs) = if x = "(" then raise ParseError
                                 else if x = ")" then raise ParseError
                                 else if x = "*" then raise ParseError
                                 else if x = "->" then raise ParseError
                                 else if (String.isPrefix "'" x) then raise ParseError
                                 else (Ground x, xs)
        and readVar [] = raise ParseError
          | readVar (x::xs) = if (String.isPrefix "'" x)
                              then (Var (String.extract(x, 1, NONE)), xs)
                              else raise ParseError
        and readConstructor [] = raise ParseError
          | readConstructor xs =
            let
                val (inner, resta) = readCType xs;
                val (label, restb) = readGround resta;
            in
                case label of
                    Ground l => (Constructor (l, inner), restb)
                  | _ => raise ParseError
            end
        and readPair [] = raise ParseError
          | readPair xs =
            let
                val (left, resta) = readBType xs;
                val (star, restb) = expect "*" resta;
                val (right, restc) = readAType restb; (* Pairs can nest right, so allow all Atypes (functions need parens) *)
            in
                (Pair (left, right), restc)
            end
        and readFun [] = raise ParseError
          | readFun xs =
            let
                val (left, resta) = readAType xs;
                val (arrow, restb) = expect "->" resta;
                val (right, restc) = readType restb; (* Functions can nest right, so allow all types *)
            in
                (Function (left, right), restc)
            end
        and readAType [] = raise ParseError
          | readAType xs = readPair xs
                           handle ParseError => readBType xs
        and readBType [] = raise ParseError
          | readBType xs = readConstructor xs
                           handle ParseError => readCType xs
        and readCType [] = raise ParseError
          | readCType xs = readVar xs
                           handle ParseError => readGround xs
                                                handle ParseError =>
                                                       let
                                                           val (lparen, resta) = expect "(" xs;
                                                           val (inner, restb) = toCloseParen resta [];
                                                           val (innerType, restc) = readType inner;
                                                       in
                                                           case restc of
                                                               [] => (innerType, restb)
                                                             | _ => raise ParseError
                                                       end
        and readType [] = raise ParseError
          | readType xs = readFun xs
                          handle ParseError => readAType xs;

        val chars = String.explode str;
        val tokens = typeTokens chars [];
        val (finalType, rest) = readType tokens;
    in
        case rest of
            [] => finalType
          | _ => (Logging.error("Extra characters at end of type!"); raise ParseError)
    end;

  end;
