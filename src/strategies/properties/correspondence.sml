import "util.set";
import "util.dictionary";
import "util.formula";
import "util.parser";

import "strategies.properties.property";
import "strategies.properties.importance";

signature CORRESPONDENCE =
sig

    exception ParseError

    structure F : FORMULA;
    type 'a corrformula;
    type correspondence = Property.property corrformula * Property.property corrformula * real;

    val flip : real -> correspondence -> correspondence;

    val equal : correspondence -> correspondence -> bool;
    val sameProperties : correspondence -> correspondence -> bool;
    val matchingProperties : correspondence -> correspondence -> bool;
    val match : PropertySet.t PropertySet.set -> PropertySet.t PropertySet.set
                -> correspondence -> bool;

    val leftMatches : PropertySet.t PropertySet.set -> correspondence
                      -> PropertySet.t PropertySet.set;
    val rightMatches : PropertySet.t PropertySet.set -> correspondence
                       -> PropertySet.t PropertySet.set;

    val liftImportances : QPropertySet.t QPropertySet.set -> correspondence
                          -> Importance.importance list;

    val identity : Property.property -> correspondence;

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
    (propertyMatchTree qs q) andalso (propertyMatchTree rs r)

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

signature CORRESPONDENCELIST =
sig

    val allMatches :
        Correspondence.correspondence list
        -> QPropertySet.t QPropertySet.set
        -> PropertySet.t PropertySet.set
        -> Correspondence.correspondence list;
    val typeCorrespondences :
        (Correspondence.correspondence * Importance.importance) list
        -> QPropertySet.t QPropertySet.set
        -> (Correspondence.correspondence * Importance.importance) list;
    val mrmc :
        (Correspondence.correspondence * Importance.importance) list
        -> QPropertySet.t QPropertySet.set
        -> PropertySet.t PropertySet.set
        -> (Correspondence.correspondence * Importance.importance) list list;

end;

structure CorrespondenceList : CORRESPONDENCELIST =
struct

structure F = Correspondence.F;

fun allMatches corrs qProps rProps =
    let
        val qProps' = QPropertySet.withoutImportances qProps;
        fun alreadyCorr cs c = List.exists (Correspondence.matchingProperties c) cs;
        val baseCorrs = List.filter (Correspondence.match qProps' rProps) corrs;
        val identities = PropertySet.map
                             Correspondence.identity
                             (PropertySet.collectLeftMatches qProps' rProps);
        val newIdentities = List.filter (fn c => not (alreadyCorr baseCorrs c))
                                        identities;
    in
        newIdentities @ baseCorrs
    end;


exception skipProp;
fun typeCorrespondences corrs qProps =
    let fun tCorrs q =
            let val p = QProperty.withoutImportance q
                val t = Property.getTypeOfValue p handle Property.NoAttribute _ => raise skipProp
                val singletonT = PropertySet.fromList [Property.fromKindValueAttributes (Kind.Type, Property.Type t, [])]
                val singletonP = PropertySet.fromList [p]
                val g = QProperty.logGravity q handle Property.NoAttribute _ => 0.0
                fun mkCorrs [] = []
                  | mkCorrs (((x,y,s),i)::L) =
                    if PropertySet.isEmpty (Correspondence.leftMatches singletonT (x,y,s))
                    then (if PropertySet.isEmpty (Correspondence.leftMatches singletonP (x,y,s)) then mkCorrs L else raise skipProp)
                    else ((F.Atom p, F.Atom (Property.fromKindValueAttributes (Kind.Dummy, Property.Label ("\"" ^ F.toString Property.toString y ^ "\""), [])),s), i*g) :: mkCorrs L
            in mkCorrs corrs
            end handle skipProp => [] (* This is the hackiest thing ever, but it should take care of cases when there is already a correspondence for the property, so we don't need to add it from its type *)
    in List.concat (QPropertySet.map tCorrs qProps)
    end;

type subset = (Property.property list * Correspondence.correspondence);
type state = (subset list * subset list);
type U = (Property.property list);

fun mrmc corrs qProps rProps =
    let
        (* fun subsetToString (s, c) = Correspondence.toString c; *)
        fun subsetToString (s, c) = List.toString Property.toString s;
        fun showState ((s, t): state) : unit =
            print (List.toString (subsetToString) s ^ "\n");

        fun sort xs = xs;

        fun takeBest xs = xs;

        (* Split a correspondence into (QPropertySet * correspondence) pairs
           The correspondence in the pair ensures we know where the QProperties
           came from for later recombination. *)
        fun split (q, r, s) =
            let val conjs = Correspondence.F.clauses q;
            in map (fn c => (fst c, (q, r, s))) conjs end;

        (* Count the distance from a total covering *)
        fun score (universe : U) (subsets : subset list) : int =
            let val qpeq = Property.match;
                val unionAll = ListSet.unionAll qpeq;
                val difference = ListSet.difference qpeq;
                val length = List.length;
                val newU = unionAll (map fst subsets);
                val dupU = List.flatmap fst subsets;
                val missing = difference universe newU;
                val redundant = difference dupU newU;
            in (length missing) + (length redundant) end;

        (* Generic backtracking algorithm *)
        fun backtrack (done : state -> bool)
                      (next : state -> state list)
                      (start : state) : state list =
            let fun loop state =
                    if done state then [state]
                    else List.flatmap loop (next state);
            in loop start end;

        (* Find the best covers of the universe from the subsets *)
        fun findCovers (universe : U) (subsets : subset list) : subset list list =
            let fun sortByScore subs =
                    let fun scoreCmp (s, t) =
                            Int.compare (score universe s, score universe t);
                    in List.mergesort (scoreCmp o fst) subs end;
                fun done (subs, _) =
                    let val s = score universe subs
                    in if s = 0 then (
                           showState (subs, []);
                           true)
                       else false end;
                fun next (subs, rest) =
                    (* Add in a new subset, ordered by "best" gain *)
                    sortByScore (map (fn (s, rest') => (s::subs, rest'))
                                     (List.inout rest));
            in map fst (backtrack done next ([], subsets)) end;

        val corrsAsSubs = List.flatmap split (map fst corrs);
        val _ = print (List.toString subsetToString corrsAsSubs ^ "\n");
        val universe = ListSet.unionAll Property.match (map fst corrsAsSubs);

        (* val covers : subset list list = findCovers universe corrsAsSubs; *)
        (* val _ = print (List.toString (List.toString subsetToString) covers ^ "\n"); *)

    in [corrs] end;

end;
