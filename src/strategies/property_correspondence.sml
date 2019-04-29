import "util.set";
import "util.dictionary";

import "strategies.property";
import "strategies.property_importance";

signature CORRESPONDENCE =
sig

    structure S: SET;
    structure D: DICTIONARY;

    exception ParseError

    datatype 'a corrformula = Atom of 'a
                            | Neg of 'a corrformula
                            | Conj of 'a corrformula * 'a corrformula
                            | Disj of 'a corrformula * 'a corrformula;
    type correspondence = Property.property corrformula * Property.property corrformula * real;

    val equal : correspondence -> correspondence -> bool;
    val match : S.t S.set -> S.t S.set -> correspondence -> bool;
    val matchingIntersectionLeft : S.t S.set -> S.t S.set -> S.t S.set;
    val sameProperties : correspondence -> correspondence -> bool;
    val matchingProperties : correspondence -> correspondence -> bool;
    val strength : correspondence -> real;

    val toString : correspondence -> string;
    val fromString : (string -> Property.property) -> string -> correspondence;
end;


structure Correspondence : CORRESPONDENCE =
struct

structure S = PropertySet;

structure D = PropertyDictionary;

exception ParseError;

datatype 'a corrformula = Atom of 'a
                        | Neg of 'a corrformula
                        | Conj of 'a corrformula * 'a corrformula
                        | Disj of 'a corrformula * 'a corrformula;
type correspondence = Property.property corrformula * Property.property corrformula * real;

fun formulamap f (Atom a) = Atom (f a)
  | formulamap f (Neg a) = Neg (formulamap f a)
  | formulamap f (Conj (a, b)) = Conj (formulamap f a, formulamap f b)
  | formulamap f (Disj (a, b)) = Disj (formulamap f a, formulamap f b);

fun emptyIntn a b = S.isEmpty (S.intersection a b);

fun strength (_, _, s) = s;

fun filterMatches p ps = S.filter (fn v => Property.propertyMatch (p,v)) ps;

fun propertyMatches p ps = not (S.isEmpty (filterMatches p ps));

fun allPropertiesMatch ps ps' =
    let val contained = S.map (fn p => propertyMatches p ps') ps;
    in all contained end;

fun somePropertiesMatch ps ps' =
    let val contained = S.map (fn p => propertyMatches p ps') ps;
    in any contained end;

fun joinSets [] = S.empty ()
  | joinSets (h::t) = S.union h (joinSets t)

(* finds matches between two property sets,
but only returns the matches of the left (ps) *)
fun matchingIntersectionLeft ps ps' =
    let val x = S.map (fn p => filterMatches p ps) ps'
    in foldr (fn (a,b) => S.union a b) (S.empty ()) x
    end;


fun match qs rs ((qp, qn), (rp, rn), _) =
    (allPropertiesMatch qp qs) andalso
    (allPropertiesMatch rp rs) andalso
    (not (somePropertiesMatch qn qs)) andalso
    (not (somePropertiesMatch rn rs));

fun biMatch (ps,ps') = allPropertiesMatch ps ps' andalso allPropertiesMatch ps' ps

fun matchingProperties ((qp, qn), (rp, rn), _) ((qp', qn'), (rp', rn'), _) =
    biMatch (qp, qp') andalso
    biMatch (qn, qn') andalso
    biMatch (rp, rp') andalso
    biMatch (rn, rn');

fun sameProperties ((qp, qn), (rp, rn), _) ((qp', qn'), (rp', rn'), _) =
    S.equal (qp, qp') andalso
    S.equal (qn, qn') andalso
    S.equal (rp, rp') andalso
    S.equal (rn, rn');

fun equal c c' = sameProperties c c' andalso Real.== ((strength c), (strength c'));

fun toString ((qp, qn), (rp, rn), s) =
    "((" ^
    (S.toString qp) ^
    ", " ^
    (S.toString qn) ^
    "), (" ^
    (S.toString rp) ^
    ", " ^
    (S.toString rn) ^
    ")) -> " ^
    (Real.toString s);

fun normalise (Atom a) = Atom a
  | normalise (Neg a) =
    let
        val a' = normalise a;
    in
        case a' of
            Atom t => Neg (Atom t)
          | Neg t => t
          | Conj (t, u) => normalise (Disj (Neg t, Neg u))
          | Disj (t, u) => normalise (Conj (Neg t, Neg u))
    end
  | normalise (Conj (a, b)) =
    let
        val a' = normalise a;
        val b' = normalise b;
    in
        case (a', b') of
            (Disj (t, u), v) => normalise (Disj (Conj (t, v),
                                                 Conj (u, v)))
          | (t, Disj(u, v)) => normalise (Disj (Conj (t, u),
                                                Conj (t, v)))
          | (u, v) => Conj (u, v)
    end
  | normalise (Disj (a, b)) =
    let
        val a' = normalise a;
        val b' = normalise b;
    in
        Disj (a', b')
    end;

fun fromString propMaker s =
    let
        val temp = (Atom (propMaker s), Atom (propMaker s), 1.0);
        val parts = String.tokens (fn c => c = #",") s;
        val [leftString, rightString, valString] = parts
                                                   handle Match => raise Match;

        fun tokenize string =
            let
                fun cluster [] xs = cluster [[]] xs
                  | cluster cs [] = cs
                  | cluster (c::cs) (x::xs) =
                    case x of
                        #"(" => cluster ([]::[#"("]::c::cs) xs
                      | #")" => cluster ([]::[#")"]::c::cs) xs
                      | s => if Char.isSpace s
                             then cluster ([]::c::cs) xs
                             else cluster ((s::c)::cs) xs;
            in
                List.rev (map (String.implode o List.rev)
                              (List.filter
                                   (fn cs => not (List.null cs))
                                   (cluster [[]] (String.explode string))))
            end;

        fun toCloseParen xs =
            let
                fun toCloseParen' [] _ _ = raise ParseError
                  | toCloseParen' (")"::cs) vs 0 = (List.rev vs, cs)
                  | toCloseParen' (")"::cs) vs n =  toCloseParen' cs (")"::vs) (n-1)
                  | toCloseParen' ("("::cs) vs n = toCloseParen' cs ("("::vs) (n+1)
                  | toCloseParen' (c::cs) vs n = toCloseParen' cs (c::vs) n;
            in
                toCloseParen' xs [] 0
            end;

        fun expect s [] = raise ParseError
          | expect s (x::xs) = if x = s
                               then (s, xs)
                               else raise ParseError;

        fun parseAtom [] = raise ParseError
          | parseAtom (x::xs) = if x = "("
                                    then raise ParseError
                                else if x = ")"
                                    then raise ParseError
                                else if x = "AND"
                                    then raise ParseError
                                else if x = "OR"
                                    then raise ParseError
                                else if x = "NOT"
                                    then raise ParseError
                                else (Atom x, xs)
        and parseNeg [] = raise ParseError
          | parseNeg xs =
            let
                val (neg, resta) = expect "NOT" xs;
                val (atom, restb) = parseCForm resta;
            in
                (Neg atom, restb)
            end
        and parseConj [] = raise ParseError
          | parseConj xs =
            let
                val (left, resta) = parseBForm xs;
                val (andtok, restb) = expect "AND" resta;
                val (right, restc) = parseAForm restb;
            in
                (Conj (left, right), restc)
            end
        and parseDisj [] = raise ParseError
          | parseDisj xs =
            let
                val (left, resta) = parseAForm xs;
                val (ortok, restb) = expect "OR" resta;
                val (right, restc) = parseFormula restb;
            in
                (Disj (left, right), restc)
            end
        and parseFormula xs = parseDisj xs
                              handle ParseError => parseAForm xs
        and parseAForm xs = parseConj xs
                            handle ParseError => parseBForm xs
        and parseBForm xs = parseNeg xs
                            handle ParseError => parseCForm xs
        and parseCForm xs = parseAtom xs
                            handle ParseError =>
                                   let
                                       val (lparn, resta) = expect "(" xs;
                                       val (toks, restb) = toCloseParen resta;
                                   in
                                       case restb of
                                           [] => parseFormula toks
                                         | _ => raise ParseError
                                   end;

        fun parse tokens =
            let
                val (tree, tokens) = parseFormula tokens;
            in
                if (List.null tokens) then tree
                else raise Match
            end;

        val read = (normalise o parse o tokenize);
    in
        (read leftString, read rightString, Real.fromString valString)
    end;

end;
