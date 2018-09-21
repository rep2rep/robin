use "base.sml";
use (BASE^"util/robinlib.sml");
use (BASE^"util/set.sml");
use (BASE^"util/dictionary.sml");

structure StringSet = Set(struct
                           type t = string;
                           val compare = String.compare;
                           end);
exception KeyError;


fun contains [] _ = false
  | contains (x::xs) y = (x=y) orelse contains xs y;

fun getValue [] _ = raise KeyError
  | getValue ((k, v)::xs) k' = if (k=k') then v else getValue xs k';

fun setToString fmt items =
    let
        val stringItems = StringSet.map fmt items;
        val withCommas = RobinLib.intersperse ", " stringItems;
        val joined = foldr (fn (x, y) => x ^ y) "" withCommas;
    in
        "{" ^ joined ^ "}"
    end;

fun printSet fmt items = print ((setToString fmt items) ^ "\n");

fun listToString fmt items =
    let
        val stringItems = map fmt items;
        val withCommas = RobinLib.intersperse ", " stringItems;
        val joined = foldr (fn (x, y) => x ^ y) "" withCommas;
    in
        "[" ^ joined ^ "]"
    end;

fun printList fmt items = print ((listToString fmt items) ^ "\n");

(* Read in some faked data *)

val cmp = fn a => fn b => String.compare(a, b);
val set' = StringSet.fromList;
val insert' = StringSet.insert;
val subset' = StringSet.subset;

val correspondingTable = [
    ((set' ["a"], set' ["F"]), 1.0),
    ((set' ["b"], set' ["H"]), 1.0)
];
val propertyTableQ = [
    ("q", set' ["a", "c", "d"])
];
val propertyTableRep = [
    ("One",   set' ["A", "B", "C", "D", "E"]),
    ("Two",   set' ["B", "D", "F", "G", "H"]),
    ("Three", set' ["A", "G", "H", "J", "K"]),
    ("Four",  set' ["H", "K", "M", "N", "O"])
];

fun propertiesRep rep = getValue propertyTableRep rep;

fun propertiesQ q = getValue propertyTableQ q;

(*
propInfluence : (question * representation * float) -> (question * representation * float)
For the given question and representation, adjust the score based on
their properties.
*)
fun propInfluence (q, r, s) =
    let
        val qProps = propertiesQ q;
        val rProps = propertiesRep r;
        val propertyPairs = List.filter
                                (fn ((a, B), _) => (subset' a qProps)
                                                   andalso (subset' B rProps))
                                correspondingTable;
        val correspondences = map (fn (_, c) => c) propertyPairs;

        val mix = fn (c, s) => (c + s);
        val s' = List.foldl mix s correspondences;
    in (q, r, s')
    end;

fun userInfluence (q, r, s) = (q, r, s);

fun taskInfluence (q, r, s) = (q, r, s);

(*
topKRepresentations : question -> int -> (representation * real) list
Determine the best k representations from some known set to attempt to
solve the given question. The limit k can take on the special value -1,
in which case every valid representation is returned.
*)
fun topKRepresentations question k =
    let
        val (questionString, questionRep) = question;
        val relevanceScore = (taskInfluence o userInfluence o propInfluence);
        val representations = map (fn (r, _) => r) propertyTableRep;
        val influencedRepresentations = List.map
                                            (fn rep => relevanceScore (questionString, rep, 0.0))
                                            representations;

        val sort = RobinLib.mergesort (fn ((a, b, c), (x, y, z)) =>
                                          if c < z then LESS
                                          else if c > z then GREATER
                                          else EQUAL);
        val getValid = List.filter (fn (_, _, s) => s > 0.0);
        val topK = fn xs => if k = ~1 then xs
                            else if (List.length xs) <= k then xs
                            else List.take (xs, k);
        val getRepWithScore = fn (_, r, s) => (r, s);
    in
        map getRepWithScore (topK (getValid (List.rev (sort (influencedRepresentations)))))
    end;
