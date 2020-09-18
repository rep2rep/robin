import "util.set";
import "util.dictionary";
import "util.formula";
import "util.parser";
import "util.algorithms";

import "strategies.properties.property";
import "strategies.properties.importance";

signature CORRESPONDENCE =
sig

    structure F : FORMULA;

    exception ParseError;

    type propertyset = PropertySet.t PropertySet.set;
    type 'a corrformula;
    type correspondence = Property.property corrformula * Property.property corrformula * real;

    val flip : real -> correspondence -> correspondence;

    val equal : correspondence -> correspondence -> bool;
    val stronger : correspondence -> correspondence -> bool;
    val sameProperties : correspondence -> correspondence -> bool;
    val matchingProperties : correspondence -> correspondence -> bool;
    val match : propertyset -> propertyset -> correspondence -> bool;
    val matchExists : propertyset -> propertyset -> correspondence list -> bool;

    val leftMatches : propertyset -> correspondence -> propertyset;
    val rightMatches : propertyset -> correspondence -> propertyset;

    val identity : Property.property -> correspondence;

    val liftImportances : QPropertySet.t QPropertySet.set ->
                          correspondence -> Importance.importance list;

    val strength : correspondence -> real;
    val toString : correspondence -> string;
    val fromString : string -> correspondence;

end;


structure Correspondence : CORRESPONDENCE =
struct

structure F = Formula(struct
                       val neg = "NOT";
                       val conj = "AND";
                       val disj = "OR";
                       end);

exception ParseError;

type propertyset = PropertySet.t PropertySet.set;
type 'a corrformula = 'a F.formula;
type correspondence = Property.property corrformula * Property.property corrformula * real;

fun strength (_, _, s) = s;
fun flip r (a,b,s) = (b,a,s*r);

fun matchTree setMatch ps p =
    let
        fun a x = setMatch x ps;
        fun n x = not x;
        fun c (x, y) = x andalso y;
        fun d (x, y) = x orelse y;
    in
        F.fold a n c d p
    end;

fun propertyMatchTree ps p = matchTree (PropertySet.isMatchedIn) ps p;

(* TODO: Show that this returns an empty list when matchTree ps p
         would return false, and a nonempty list when matchTree ps p
         would return true. Better would be to show that the properties
         returned are exactly those that occur in the formula that are
         not also negated in the formula, but that might be quite tricky. *)
fun collectMatches fromList match findMatch ps p =
    let
        fun removeOne [] a zs = zs
          | removeOne (x::xs) a zs = if match(x, a)
                                     then (removeOne xs a zs)
                                     else (removeOne xs a (x::zs));
        fun removeAll xs zs = List.foldr
                                  (fn (x, zs) => removeOne zs x [])
                                  zs xs;
        fun collectMatchesHelper ps t =
            let
                fun a x = case (findMatch x ps) of
                              SOME x => ([x], [])
                            | NONE => ([], []);
                fun n (x, y) = (y, x);
                fun c ((x, y), (x', y')) = (x@x', y@y');
                fun d ((x, y), (x', y')) = (x@x', y@y');
            in
                F.fold a n c d t
            end;
        val (keep, remove) = collectMatchesHelper ps p;
    in
        fromList (removeAll remove keep)
    end;

fun propertyCollectMatches ps p =
    collectMatches
        (PropertySet.fromList) (Property.match)
        (fn x => fn xs => if PropertySet.isMatchedIn x xs then SOME x else NONE)
        ps p;

fun leftMatches ps (p, _, _) = propertyCollectMatches ps p;

fun rightMatches ps (_, p, _) = propertyCollectMatches ps p;

fun match qs rs (q, r, _) =
    (propertyMatchTree qs q) andalso (propertyMatchTree rs r);

fun matchExists qs rs cs = List.exists (match qs rs) cs;

fun matchingProperties (q, r, _) (q', r', _) =
    let
        fun eq (p1, p2) = Property.match(p1, p2);
    in
        F.equal eq (q, q') andalso F.equal eq (r, r')
    end;

fun sameProperties (q, r, _) (q', r', _) =
    let
        fun eq (p1, p2) = Property.compare(p1, p2) = EQUAL;
    in
        F.equal eq (q, q') andalso F.equal eq (r, r')
    end;

fun equal c c' = sameProperties c c' andalso Real.== ((strength c), (strength c'));

(* One correspondence is stronger than another
   if the strength has increased, and where...
       <a, b AND c> is stronger than <a, b>
       <a, b>       is stronger than <a AND c, b>
       <a, b>       is stronger than <a, b OR c>
       <a OR c, b>  is stronger than <a, b>
   (that is, q' -> q and r -> r')
 *)
fun stronger (q, r, s) (q', r', s') =
    F.implies Property.match q' q andalso
    F.implies Property.match r r' andalso
    s >= s';

fun identity p = (F.Atom p, F.Atom p, 1.0);

fun liftImportances qs (l, _, _) =
    let
        fun qListMatch (ps, qs) = QPropertySet.equal (ps, qs);
        fun qFind p qs =
            let
                val qs' = QPropertySet.filterMatches p qs;
            in
                if QPropertySet.isEmpty qs' then NONE
                else SOME qs'
            end;
        fun qFromList xs = QPropertySet.unionAll xs;
        val matchedQProps = collectMatches
                                qFromList
                                qListMatch
                                qFind
                                qs l;
        val allImportances = QPropertySet.map
                                 (fn q => QProperty.logGravity q handle Property.NoAttribute _ => QProperty.importanceOf q)
                                 matchedQProps;
    in
        allImportances
    end;

fun toString (q, r, s) =
    let
        val treeToString = F.toString Property.toString;
    in
        "(" ^ (treeToString q)
        ^ ", " ^ (treeToString r)
        ^ ", " ^ (Real.toString s)
        ^ ")"
    end;

fun fromString s =
    let
        val parts = Parser.splitStrip "," (Parser.removeParens s);
        val (leftString, rightString, valString) =
            case parts of
                [l, r, v] => (l, r, v)
              | _ => raise Match;

        fun realFromString r = case (Real.fromString r) of
                                   SOME x => x
                                 | NONE => raise ParseError;

        val read = F.fromString Property.fromString;
    in
        (read leftString, read rightString, realFromString valString)
    end
    handle F.ParseError => raise ParseError
         | Kind.KindError => raise ParseError;

end;
