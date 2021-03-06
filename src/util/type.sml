import "util.logging";
import "util.stream";
import "util.parser";

signature TYPE =
sig
    exception ParseError;
    datatype t = Ground of string
               | Var of string
               | Pair of t * t
               | Function of t * t
               | Constructor of string * t;
    val getInOutTypes : t -> (t list * t);
    val outputArity : t -> int;
    val inputArity : t -> int;
    val order : t -> int;
    exception TUNIFY;
    val unify : (t * t) list -> (t * t) list;
    val generalise : t -> t * (string * string) list;
    val match : t * t -> bool;
    val compare : t * t -> order;
    val toString : t -> string;
    val toDebugString : t -> string;
    val fromString : string -> t;
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

datatype t = Ground of string
           | Var of string
           | Pair of t * t
           | Function of t * t
           | Constructor of string * t;

exception Extend;

val newVariable' = ref (let fun inc [] = raise Fail "Variable gen in Type broken!"
                              | inc [#"z"] = SOME [#"a", #"a"]
                              | inc (x::xs) = if x = #"z" then SOME (#"a"::(Option.valOf(inc xs)))
                                              else SOME (Char.succ(x)::xs);
                        in Stream.map (String.implode o List.rev) (Stream.unfold inc [#"a"]) end);

fun getNewVariable () = let val (v, s) = Stream.step (!newVariable');
                            val _ = newVariable' := s;
                        in v end;

fun allVarsIn f =
    let fun getVars' ans (Ground a) = ans
          | getVars' ans (Var a) = (a::ans)
          | getVars' ans (Pair (x, y)) = getVars' (getVars' ans x) y
          | getVars' ans (Function (x, y)) = getVars' (getVars' ans x) y
          | getVars' ans (Constructor (s, x)) = getVars' ans x
    in getVars' [] f end;

fun getNewVariableExcept invalids = let val v = getNewVariable ();
                                    in if List.exists (equals v) invalids
                                       then getNewVariableExcept invalids
                                       else v end;


fun pairToList (Pair (x,y)) = (pairToList x) @ (pairToList y)
  | pairToList x = [x]

fun getInOutTypes (Ground x) = ([], Ground x)
  | getInOutTypes (Var x) = ([], Var x)
  | getInOutTypes (Pair (x,y)) = let val (xT,xt) = getInOutTypes x val (yT,yt) = getInOutTypes y in (xT @ yT, Pair (xt,yt)) end
  | getInOutTypes (Function (x,y)) = let val (yT,yt) = getInOutTypes y; in ((pairToList x) @ yT, yt) end
  | getInOutTypes (Constructor (s,x)) = ([], Constructor (s,x));

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

fun replaceVar (Var x, t') t =
     (case t of
          Ground s => Ground s
        | Var y => if x = y then t' else Var y
        | Pair (s,t) => Pair (replaceVar (Var x, t') s, replaceVar (Var x, t') t)
        | Function (s,t) => Function (replaceVar (Var x, t') s, replaceVar (Var x, t') t)
        | Constructor (s,t) => Constructor (s, replaceVar (Var x, t') t))
  | replaceVar _ _ = raise Match;

exception TUNIFY;
fun unify [] = []
  | unify (p :: C) =
    case p of
      (Ground a, Ground b) => if a = b then unify C else raise TUNIFY
    | (Var x, Var y) => if x = y then unify C else p :: (unify (map (mappair (replaceVar p)) C))
    | (Var x, t) => if occurs x t then raise TUNIFY else p :: (unify (map (mappair (replaceVar p)) C))
    | (t, Var x) => if occurs x t then raise TUNIFY else p :: (unify (map (mappair (replaceVar (Var x, t))) C))
    | (Pair (s,t), Pair (s',t')) => unify ((s,s') :: (t,t') :: C)
    | (Function (s,t), Function (s',t')) => unify ((s,s') :: (t,t') :: C)
    | (Constructor (s,t), Constructor (s',t')) => if s = s' then unify ((t,t') :: C) else raise TUNIFY
    | _ => raise TUNIFY;

fun generalise f =
    let
        val invalidVars = allVarsIn f;
        fun getOrGenerate' old [] s = let val v = getNewVariableExcept invalidVars;
                                      in (v, (s, v)::old) end
          | getOrGenerate' old ((s, v)::vs) s' = if s = s' then (v, (s, v)::(old@vs))
                                                 else getOrGenerate' ((s,v)::old) vs s';
        fun getOrGenerate gs s = getOrGenerate' [] gs s;
        fun generalise' gens (Ground a) = let val (v, gens') = getOrGenerate gens a
                                          in (Var v, gens') end
          | generalise' gens (Var v) = (Var v, gens)
          | generalise' gens (Pair (x, y)) = let val (g1, gens') = generalise' gens x;
                                                 val (g2, gens'') = generalise' gens' y;
                                             in (Pair (g1, g2), gens'') end
          | generalise' gens (Function (x, y)) = let val (g1, gens') = generalise' gens x;
                                                     val (g2, gens'') = generalise' gens' y;
                                                 in (Function (g1, g2), gens'') end
          | generalise' gens (Constructor (s, x)) = let val (g, gens') = generalise' gens x;
                                                    in (Constructor (s,g), gens') end;
    in generalise' [] f end;

fun refreshVariables invalidVars f =
    let fun getOrGenerate' old [] s = let val v = getNewVariableExcept invalidVars;
                                      in (v, (s, v)::old) end
          | getOrGenerate' old ((s, v)::vs) s' = if s = s' then (v, (s, v)::(old@vs))
                                                 else getOrGenerate' ((s,v)::old) vs s';
        fun getOrGenerate gs s = getOrGenerate' [] gs s;
        fun newNames names (Ground s) = (Ground s, names)
          | newNames names (Var a) = let val (v, names') = getOrGenerate names a
                                     in (Var v, names') end
          | newNames names (Pair (s,t)) = let val (p1, names') = newNames names s;
                                              val (p2, names'') = newNames names' t;
                                          in (Pair (p1, p2), names'') end
          | newNames names (Function (s,t)) = let val (f1, names') = newNames names s;
                                                  val (f2, names'') = newNames names' t;
                                              in (Function (f1, f2), names'') end
          | newNames names (Constructor (s,t)) = let val (c, names') = newNames names t
                                                 in (Constructor (s, c), names') end;
    in
        #1 (newNames [] f)
    end;

fun match (x,y) =
  let val (_,found) = (unify [(x, refreshVariables (allVarsIn (Pair (x, y))) y)],true) handle TUNIFY => ([],false)
  in found
  end;

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
        fun isInvalid c = not (Char.isAlpha c orelse c = #"_");
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

        fun parseGround () =
            let
                fun ground c = if c = "(" then NONE
                               else if c = ")" then NONE
                               else if c = "*" then NONE
                               else if c = "->" then NONE
                               else if String.isPrefix "'" c then NONE
                               else SOME (Ground c);
            in
                Parser.accept ground
            end
        and parseVar () =
            let
                fun var c = if String.isPrefix "'" c
                            then SOME (Var (String.extract (c, 1, NONE)))
                            else NONE;
            in
                Parser.accept var
            end
        and parseConstructor () =
            let
                val parseInner = parseCType ();
                val parseOuter = parseGround ();
            in
                (parseInner)
                    >=> (fn c => (parseOuter
                                      >=> (fn (Ground g) =>
                                              Parser.produce (Constructor (g, c))
                                          | _ => raise Parser.ParseError)))
            end
        and parsePair () =
            let
                val readLeft = parseBType ();
                val readRight = parseAType ();
                val readStar = Parser.expect "*" (Ground "NULL");
            in
                (readLeft)
                    >=> (fn l => (readStar
                                      >>> (readRight)
                                      >=> (fn r => Parser.produce (Pair (l, r)))))
            end
        and parseFunction () =
            let
                val readLeft = parseAType ();
                val readRight = parseType ();
                val readArrow = Parser.expect "->" (Ground "NULL");
            in
                (readLeft)
                    >=> (fn l => (readArrow
                                      >>> (readRight)
                                      >=> (fn r => Parser.produce (Function (l, r)))))
            end
        and parseAType () = Parser.either (parsePair) (parseBType)
        and parseBType () = Parser.either (parseConstructor) (parseCType)
        and parseCType () = Parser.either (parseVar) (parseDType)
        and parseDType () =
            let
                val openParen = Parser.expect "(" (Ground "NULL");
                val closeParen = Parser.expect ")" (Ground "NULL");
                val parseSubType =
                    (openParen)
                        >>> (parseType ())
                        >=> (fn t => (closeParen
                                          >>> (Parser.produce t)));
            in
                Parser.either (parseGround) (fn () => parseSubType)
            end
        and parseType () = Parser.either (parseFunction) (parseAType);

        fun parse tokens = Parser.run (parseType ()) tokens;

        val chars = String.explode str;
        val tokens = typeTokens chars [];

    in
        parse tokens
    end;

end;
