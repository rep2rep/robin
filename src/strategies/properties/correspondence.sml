import "util.set";
import "util.dictionary";
import "util.formula";
import "util.parser";

import "strategies.properties.property";
import "strategies.properties.importance";

signature CORRESPONDENCE =
sig

    structure F : FORMULA;

    exception ParseError;

    type propertyset = PropertySet.t PropertySet.set;
    type 'a corrformula;
    type correspondence = Property.property corrformula * Property.property corrformula * real;

    val equal : correspondence -> correspondence -> bool;
    val stronger : correspondence -> correspondence -> bool;
    val sameProperties : correspondence -> correspondence -> bool;
    val matchingProperties : correspondence -> correspondence -> bool;
    val match : propertyset -> propertyset -> correspondence -> bool;
    val matchExists : propertyset -> propertyset -> correspondence list -> bool;

    val leftMatches : propertyset -> correspondence -> propertyset;
    val rightMatches : propertyset -> correspondence -> propertyset;

    val identity : Property.property -> correspondence;

    val strength : correspondence -> real;
    val toString : correspondence -> string;
    val fromString : string -> correspondence;

end;


structure Correspondence : CORRESPONDENCE =
struct

structure S = PropertySet;

structure D = PropertyDictionary;

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

fun matchTree ps p =
    let
        fun a x = PropertySet.isMatchedIn x ps;
        fun n x = not x;
        fun c (x, y) = x andalso y;
        fun d (x, y) = x orelse y;
    in
        F.fold a n c d p
    end;

(* TODO: Show that this returns an empty list when matchTree ps p
         would return false, and a nonempty list when matchTree ps p
         would return true. Better would be to show that the properties
         returned are exactly those that occur in the formula that are
         not also negated in the formula, but that might be quite tricky. *)
fun collectMatches ps p =
    let
        fun removeOne [] a zs = zs
          | removeOne (x::xs) a zs = if Property.match(x, a)
                                     then (removeOne xs a zs)
                                     else (removeOne xs a (x::zs));
        fun removeAll xs zs = List.foldr
                                  (fn (x, zs) => removeOne zs x [])
                                  zs xs;
        fun collectMatches' ps t =
            let
                fun a x = if (PropertySet.isMatchedIn x ps) then ([x], []) else ([], []);
                fun n (x, y) = (y, x);
                fun c ((x, y), (x', y')) = (x@x', y@y');
                fun d ((x, y), (x', y')) =
                    let
                        val option1 = removeAll y x;
                        val option2 = removeAll y' x';
                    in
                        if List.null option1 then (x', y') else (x, y)
                    end;
            in
                F.fold a n c d t
            end;
        val (keep, remove) = collectMatches' ps p;
    in
        S.fromList (removeAll remove keep)
    end;

fun leftMatches ps (p, _, _) = collectMatches ps p;

fun rightMatches ps (_, p, _) = collectMatches ps p;

fun match qs rs (q, r, _) = (matchTree qs q) andalso (matchTree rs r);

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
