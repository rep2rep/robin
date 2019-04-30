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
    val sameProperties : correspondence -> correspondence -> bool;
    val matchingProperties : correspondence -> correspondence -> bool;
    val match : S.t S.set -> S.t S.set -> correspondence -> bool;

    val leftMatches : S.t S.set -> correspondence -> S.t S.set;
    val rightMatches : S.t S.set -> correspondence -> S.t S.set;

    val identity : Property.property -> correspondence;

    val strength : correspondence -> real;
    val toString : correspondence -> string;
    val fromString : string -> correspondence;

    val matchingIntersectionLeft : S.t S.set -> S.t S.set -> S.t S.set;
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

fun isoTrees eq x y =
    let
        fun isoTrees' (Atom a) (Atom b) = eq(a, b)
          | isoTrees' (Neg a) (Neg b) = isoTrees' a b
          | isoTrees' (Conj (a, b)) (Conj (c, d)) =
            (isoTrees' a c andalso isoTrees' b d) orelse
            (isoTrees' a d andalso isoTrees' b c)
          | isoTrees' (Disj (a, b)) (Disj (c, d)) =
            (isoTrees' a c andalso isoTrees' b d) orelse
            (isoTrees' a d andalso isoTrees' b c)
          | isoTrees' _ _ = false;
    in
        isoTrees' (normalise x) (normalise y)
    end;

fun strength (_, _, s) = s;

fun filterMatches p ps = S.filter (fn v => Property.match (p,v)) ps;

(* finds matches between two property sets,
but only returns the matches of the left (ps) *)
fun matchingIntersectionLeft ps ps' =
    let val x = S.map (fn p => filterMatches p ps) ps'
    in foldr (fn (a,b) => S.union a b) (S.empty ()) x
    end;

fun isMatchedIn p ps = not (S.isEmpty (filterMatches p ps));

fun matchTree ps (Atom p) = isMatchedIn p ps
  | matchTree ps (Neg a) = not (matchTree ps a)
  | matchTree ps (Conj (a, b)) = (matchTree ps a) andalso (matchTree ps b)
  | matchTree ps (Disj (a, b)) = (matchTree ps a) orelse (matchTree ps b);

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
        fun collectMatches' ps (Atom p) = if (isMatchedIn p ps)
                                          then ([p], [])
                                          else ([], [])
          | collectMatches' ps (Neg a) =
            let
                val (keep, remove) = collectMatches' ps a;
            in
                (remove, keep)
            end
          | collectMatches' ps (Conj (a, b)) =
            let
                val (keep, remove) = collectMatches' ps a;
                val (keep', remove') = collectMatches' ps b;
            in
                (keep@keep', remove@remove')
            end
          | collectMatches' ps (Disj (a, b)) =
            let
                val (keep, remove) = collectMatches' ps a;
                val (keep', remove') = collectMatches' ps b;
                val option1 = removeAll remove keep;
                val option2 = removeAll remove' keep';
            in
                if List.null option1 then (keep', remove') else (keep, remove)
            end;
        val (keep, remove) = collectMatches' ps p;
    in
        S.fromList (removeAll remove keep)
    end;

fun leftMatches ps (p, _, _) = collectMatches ps p;

fun rightMatches ps (_, p, _) = collectMatches ps p;

fun match qs rs (q, r, _) = (matchTree qs q) andalso (matchTree rs r)

fun matchingProperties (q, r, _) (q', r', _) =
    let
        fun eq (p1, p2) = Property.match(p1, p2);
    in
        isoTrees eq q q' andalso isoTrees eq r r'
    end;

fun sameProperties (q, r, _) (q', r', _) =
    let
        fun eq (p1, p2) = Property.compare(p1, p2) = EQUAL;
    in
        isoTrees eq q q' andalso isoTrees eq r r'
    end;

fun equal c c' = sameProperties c c' andalso Real.== ((strength c), (strength c'));

fun identity p = (Atom p, Atom p, 1.0);

fun toString (q, r, s) =
    let
        fun treeToString (Atom a) = Property.toString a
          | treeToString (Neg a) = "NOT " ^ (treeToString a)
          | treeToString (Conj (x as (Disj _), y as (Disj _))) =
            "("
            ^ (treeToString x)
            ^ ") AND ("
            ^ (treeToString y)
            ^ ")"
          | treeToString (Conj (x as(Disj _), y)) = "("
                                                     ^ (treeToString x)
                                                     ^ ") AND "
                                                     ^ (treeToString y)
          | treeToString (Conj (x, y as (Disj _))) = (treeToString x)
                                                     ^ " AND ("
                                                     ^ (treeToString y)
                                                     ^ ")"
          | treeToString (Conj (a, b)) = (treeToString a)
                                         ^ " AND "
                                         ^ (treeToString b)
          | treeToString (Disj (x as (Conj _), y as (Conj _))) =
            "("
            ^ (treeToString x)
            ^ ") OR ("
            ^ (treeToString y)
            ^ ")"
          | treeToString (Disj (x as (Conj _), y)) = "("
                                                     ^ (treeToString x)
                                                     ^ ") OR "
                                                     ^ (treeToString y)
          | treeToString (Disj (x, y as (Conj _))) = (treeToString x)
                                                     ^ " OR ("
                                                     ^ (treeToString y)
                                                     ^ ")"
          | treeToString (Disj (a, b)) = (treeToString a)
                                         ^ " OR "
                                         ^ (treeToString b);
    in
        "(" ^ (treeToString q)
        ^ ", " ^ (treeToString r)
        ^ ", " ^ (Real.toString s)
        ^ ")"
    end;

fun fromString s =
    let
        fun removeParens s =
            let
                fun charIs c = fn x => x = c
                fun flipAndDropIf p [] = raise Match
                  | flipAndDropIf p xs = case List.rev xs of
                                             (y::ys) => if p y then ys
                                                        else raise Match
                                           | [] => [];
                val chars = String.explode s;
                val dropped = flipAndDropIf
                                  (charIs #"(") (flipAndDropIf
                                                     (charIs #")") chars)
                              handle Match => chars;
                val s' = String.implode dropped;
            in
                s'
            end;
        val parts = String.tokens (fn c => c = #",") (removeParens s);
        val (leftString, rightString, valString) =
            case parts of
                [l, r, v] => (l, r, v)
              | _ => raise Match;

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

        fun realFromString r = case (Real.fromString r) of
                                   SOME x => x
                                 | NONE => raise ParseError;

        val read = (formulamap Property.fromString) o normalise o parse o tokenize;
    in
        (read leftString, read rightString, realFromString valString)
    end;

end;
