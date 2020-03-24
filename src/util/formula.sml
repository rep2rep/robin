(*
formula.sml

Propositional formulae consist of conjunction, disjunction,
and negation forming trees of atoms. They can be read, printed,
normalised, mapped, and folded.
To provide more generality, we allow you to specify the tokens
that you use for negation, conjunction, and disjunction.
*)

import "util.logging";
import "util.parser";
import "util.listset";

signature FORMULA =
sig

    exception ParseError;
    exception NormalisationError;

    datatype 'a formula = Atom of 'a
                        | Neg of 'a formula
                        | Conj of 'a formula * 'a formula
                        | Disj of 'a formula * 'a formula;

    val normalise : 'a formula -> 'a formula;
    val clauseToLists : 'a formula -> ('a list * 'a list);
    (* Convert a clause to a pair of lists,
       the first with the positive atoms,
       the second with the negated atoms. *)

    val equal : ('a * 'a -> bool) -> ('a formula * 'a formula) -> bool;

    val map : ('a -> 'b) -> 'a formula -> 'b formula;
    val fold : ('a -> 'b) -> ('b -> 'b) -> ('b * 'b -> 'b) -> ('b * 'b -> 'b) -> 'a formula -> 'b;

    val clauses : 'a formula -> 'a formula list;
    val implies : ('a * 'a -> bool) -> 'a formula -> 'a formula -> bool;

    val toString : ('a -> string) -> 'a formula -> string;
    val fromString : (string -> 'a) -> string -> 'a formula;

end;

functor Formula(Format :
                sig
                    val neg : string;
                    val conj : string;
                    val disj : string;
                end) : FORMULA =
struct

exception ParseError;
exception NormalisationError;

datatype 'a formula = Atom of 'a
                    | Neg of 'a formula
                    | Conj of 'a formula * 'a formula
                    | Disj of 'a formula * 'a formula;

fun normalise (Atom a) = Atom a
  | normalise (Neg a) =
    let val a' = normalise a;
    in case a' of
            Atom t => Neg (Atom t)
          | Neg t => t
          | Conj (t, u) => normalise (Disj (Neg t, Neg u))
          | Disj (t, u) => normalise (Conj (Neg t, Neg u))
    end
  | normalise (Conj (a, b)) =
    let val a' = normalise a;
        val b' = normalise b;
    in case (a', b') of
            (Disj (t, u), v) => normalise (Disj (Conj (t, v),
                                                 Conj (u, v)))
          | (t, Disj(u, v)) => normalise (Disj (Conj (t, u),
                                                Conj (t, v)))
          | (u, v) => Conj (u, v)
    end
  | normalise (Disj (a, b)) =
    let val a' = normalise a;
        val b' = normalise b;
    in Disj (a', b') end;

fun clauseToLists f =
    let val f' = normalise f;
        fun clausify (Atom a) = ([a], [])
          | clausify (Neg (Atom a)) = ([], [a])
          | clausify (Conj (t, u)) =
            let val (p1, n1) = clausify t;
                val (p2, n2) = clausify u;
            in (p1 @ p2, n1 @ n2) end
          | clausify _ = raise NormalisationError;
    in clausify f' end;

fun fold a _ _ _ (Atom x) = a x
  | fold a n c d (Neg x) = n(fold a n c d x)
  | fold a n c d (Conj (x, y)) =
    let
        val l = fold a n c d x;
        val r = fold a n c d y;
    in
        c (l, r)
    end
  | fold a n c d (Disj (x, y)) =
    let
        val l = fold a n c d x;
        val r = fold a n c d y;
    in
        d (l, r)
    end;

fun map f (Atom a) = Atom (f a)
  | map f (Neg a) = Neg (map f a)
  | map f (Conj (a, b)) = Conj (map f a, map f b)
  | map f (Disj (a, b)) = Disj (map f a, map f b);

fun equal eq (x, y) =
    let
        fun equal' (Atom a) (Atom b) = eq(a, b)
          | equal' (Neg a) (Neg b) = equal' a b
          | equal' (Conj (a, b)) (Conj (c, d)) =
            (equal' a c andalso equal' b d) orelse
            (equal' a d andalso equal' b c)
          | equal' (Disj (a, b)) (Disj (c, d)) =
            (equal' a c andalso equal' b d) orelse
            (equal' a d andalso equal' b c)
          | equal' _ _ = false;
    in
        equal' (normalise x) (normalise y)
    end;

fun clauses (Atom a) = [(Atom a)]
  | clauses (Neg a) = [(Neg a)]
  | clauses (Conj(Disj a, b)) = clauses (normalise (Conj(Disj a, b)))
  | clauses (Conj(a, Disj b)) = clauses (normalise (Conj(a, Disj b)))
  | clauses (Conj(a, b)) = [Conj(a, b)]
  | clauses (Disj(a, b)) = (clauses a) @ (clauses b);

(* implies : ('a * 'a -> b) -> 'a formula -> 'a formula -> bool
`implies` defines a relationship on satisfiability. `implies a b` returns true
if when in all cases a is satisfied, then b would also be satisfied. For example,
x AND y implies x, while x implies x OR z. We also state that x implies x.
The provided function is the atom-level implication checker: we need to make
sure that the things the formula is talking about imply each other.
 *)
fun implies imp x y =
    (* General idea: x's clauses must be a 'subset' of y's clauses
                     where the subset relation is defined by being
                     a 'covering conjunction' (has all the terms and maybe extras).
     *)
    let
        fun unitsOfClause (Atom x) = [Atom x]
          | unitsOfClause (Neg x) = [Neg x]
          | unitsOfClause (Conj(a, b)) = (unitsOfClause a) @ (unitsOfClause b)
          | unitsOfClause (Disj _) = raise Match;
        fun coveringConj (x, y) =
            let
                val x' = unitsOfClause x;
                val y' = unitsOfClause y;
                fun eqF (Atom m, Atom n) = imp (m, n)
                  | eqF (Neg m, Neg n) = eqF (m, n)
                  | eqF _ = false;
            in
                ListSet.subset eqF y' x'
            end;
    in
        ListSet.subset coveringConj (clauses x) (clauses y)
    end;

fun toString f (Atom a) = f a
  | toString f (Neg a) = Format.neg ^ " " ^ (toString f a)
  | toString f (Conj (x as (Disj _), y as (Disj _))) = "("
                                                       ^ (toString f x)
                                                       ^ ") "
                                                       ^ Format.conj
                                                       ^ " ("
                                                       ^ (toString f y)
                                                       ^ ")"
  | toString f (Conj (x as(Disj _), y)) = "("
                                          ^ (toString f x)
                                          ^ ") "
                                          ^ Format.conj
                                          ^ " "
                                          ^ (toString f y)
  | toString f (Conj (x, y as (Disj _))) = (toString f x)
                                           ^ " "
                                           ^ Format.conj
                                           ^ " ("
                                           ^ (toString f y)
                                           ^ ")"
  | toString f (Conj (a, b)) = (toString f a)
                               ^ " "
                               ^ Format.conj
                               ^ " "
                               ^ (toString f b)
  | toString f (Disj (x as (Conj _), y as (Conj _))) = "("
                                                       ^ (toString f x)
                                                       ^ ") "
                                                       ^ Format.disj
                                                       ^ " ("
                                                       ^ (toString f y)
                                                       ^ ")"
  | toString f (Disj (x as (Conj _), y)) = "("
                                           ^ (toString f x)
                                           ^ ") "
                                           ^ Format.disj
                                           ^ " "
                                           ^ (toString f y)
  | toString f (Disj (x, y as (Conj _))) = (toString f x)
                                           ^ " "
                                           ^ Format.disj
                                           ^ " ("
                                           ^ (toString f y)
                                           ^ ")"
  | toString f (Disj (a, b)) = (toString f a)
                               ^ " "
                               ^ Format.disj
                               ^ " "
                               ^ (toString f b);

fun fromString builder s =
    let
        fun tokenize string =
            let
                fun revJoinAll xs = List.map (String.implode o List.rev) xs;
                fun dropEmpty xs = List.filter (fn s => s <> "") xs;
                fun stripAllSpaces xs = List.map (Parser.stripSpaces) xs;
                fun cluster [] xs = cluster [[]] xs
                  | cluster cs [] = List.rev cs
                  | cluster (c::cs) (x::xs) =
                    case x of
                        #"(" => cluster ([]::[#"("]::c::cs) xs
                      | #")" => cluster ([]::[#")"]::c::cs) xs
                      | s => if Char.isSpace s
                             then cluster ([]::[s]::c::cs) xs
                             else cluster ((s::c)::cs) xs;
                fun fixSpaces ans [] = List.rev ans
                  | fixSpaces ans [t] = fixSpaces (t::ans) []
                  | fixSpaces ans (t::t'::ts) =
                    if (Parser.stripSpaces t) = "("
                       orelse t = Format.neg
                    then
                        fixSpaces (t::ans) (t'::ts)
                    else if t' = Format.conj
                            orelse t' = Format.disj
                            orelse t' = Format.neg
                            orelse t' = ")"
                    then
                        fixSpaces (t'::t::ans) ts
                    else
                        fixSpaces ans ((t ^ t')::ts);
                val chars = String.explode string;
            in
                dropEmpty
                    (stripAllSpaces
                         (fixSpaces []
                                    (revJoinAll
                                         (cluster [[]] chars))))
            end;

        fun parseAtom () =
            let
                fun atom c = if c = "(" then NONE
                             else if c = ")" then NONE
                             else if c = Format.conj then NONE
                             else if c = Format.disj then NONE
                             else if c = Format.neg then NONE
                             else SOME (Atom c);
            in
                Parser.accept atom
            end
        and parseNeg () =
            let
                val readNegSign = Parser.expect Format.neg (Atom "NULL");
                val readNegArg = parseCForm ();
            in
                (readNegSign)
                    >>> (readNegArg)
                    >=> (fn b => Parser.produce (Neg b))
            end
        and parseConj () =
            let
                val readConjSign = Parser.expect Format.conj (Atom "NULL");
                val readLeftArg = parseBForm ();
                val readRightArg = parseAForm ();
            in
                (readLeftArg)
                    >=> (fn l => (readConjSign
                                      >>> (readRightArg)
                                      >=> (fn r => Parser.produce (Conj (l, r)))))
            end
        and parseDisj () =
            let
                val readDisjSign = Parser.expect Format.disj (Atom "NULL");
                val readLeftArg = parseAForm ();
                val readRightArg = parseFormula ();
            in
                (readLeftArg)
                    >=> (fn l => (readDisjSign
                                      >>> (readRightArg)
                                      >=> (fn r => Parser.produce (Disj (l, r)))))
            end
        and parseFormula () = Parser.either (parseDisj) (parseAForm)
        and parseAForm () = Parser.either (parseConj) (parseBForm)
        and parseBForm () = Parser.either (parseNeg) (parseCForm)
        and parseCForm () =
            let
                val openParen = Parser.expect "(" (Atom "NULL");
                val closeParen = Parser.expect ")" (Atom "NULL");
                val parseSubFormula =
                    (openParen)
                        >>> (parseFormula ())
                        >=> (fn f => (closeParen
                                          >>> (Parser.produce f)));
            in
                Parser.either (parseAtom) (fn () => parseSubFormula)
            end;

        fun parse tokens = Parser.run (parseFormula ()) tokens;

    in
        ((map builder) o normalise o parse o tokenize) s
    end
    handle Parser.ParseError => (Logging.error
                                     ("Failed to parse formula \"" ^ s ^ "\"\n");
                                 raise ParseError);

end;
