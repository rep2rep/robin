import "util.set";
import "util.dictionary";

import "strategies.property";
import "strategies.property_importance";

signature CORRESPONDENCE =
sig

    structure S: SET;
    structure D: DICTIONARY;

    type correspondence = (S.t S.set * S.t S.set) * (S.t S.set * S.t S.set) * real;
    type importance = Importance.importance;

    val equal : correspondence -> correspondence -> bool;
    val match : S.t S.set -> S.t S.set -> correspondence -> bool;
    val matchingIntersectionLeft : S.t S.set -> S.t S.set -> S.t S.set;
    val sameProperties : correspondence -> correspondence -> bool;
    val matchingProperties : correspondence -> correspondence -> bool;
    val strength : correspondence -> real;

    val toString : correspondence -> string;
end;


structure Correspondence : CORRESPONDENCE =
struct

structure S = PropertySet;

structure D = PropertyDictionary;

type correspondence = (S.t S.set * S.t S.set) * (S.t S.set * S.t S.set) * real;
type importance = Importance.importance;

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

end;
