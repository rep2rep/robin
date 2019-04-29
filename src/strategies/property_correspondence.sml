import "util.set";
import "util.dictionary";

import "strategies.property";
import "strategies.property_importance";

signature CORRESPONDENCE =
sig

    structure S: SET;
    structure D: DICTIONARY;

    datatype 'a corrformula = Atom of 'a
                            | Neg of 'a corrformula
                            | Conj of 'a corrformula * 'a corrformula
                            | Disj of 'a corrformula * 'a corrformula;
    type correspondence = Property.property corrformula * Property.property corrformula * real;

    val equal : correspondence -> correspondence -> bool;
    val match : S.t S.set -> S.t S.set -> correspondence -> bool;
    val sameProperties : correspondence -> correspondence -> bool;
    val strength : correspondence -> real;

    val toString : correspondence -> string;
    val fromString : (string -> Property.property) -> string -> correspondence;
end;


structure Correspondence : CORRESPONDENCE =
struct

structure S = PropertySet;

structure D = PropertyDictionary;

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

fun match qs rs ((qp, qn), (rp, rn), _) =
    (S.subset qp qs) andalso
    (S.subset rp rs) andalso
    (emptyIntn qn qs) andalso
    (emptyIntn rn rs);

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
    in
    end
  | normalise (Conj (a, b)) =
    let
    in
    end
  | normalise (Disj (a, b)) =
    let
    in
    end;

fun fromString propMaker s = (Atom (propMaker s), Atom (propMaker s), 1.0);

end;
