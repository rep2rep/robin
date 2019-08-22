import "util.dictionary";

signature MULTISET =
sig
    type t;
    type 'a multiset;
    exception NegativeCount of (t * int);

    val empty : unit -> t multiset;
    val fromList : t list -> t multiset;
    val fromPairList : (t * int) list -> t multiset;
    val toList : t multiset -> t list;
    val toPairList : t multiset -> (t * int) list;
    val toString : t multiset -> string;

    val insert : t multiset -> t -> unit;
    val remove : t multiset -> t -> unit;
    val removeAll : t multiset -> t -> unit;

    val union : t multiset -> t multiset -> t multiset;
    val intersection : t multiset -> t multiset -> t multiset;

    val unionAll : t multiset list -> t multiset;
    val intersectionAll : t multiset list -> t multiset;

    val map : (t -> 'a) -> t multiset -> 'a list;
    val endomap : (t -> t) -> t multiset -> t multiset;
    val filter : (t * int -> bool) -> t multiset -> t multiset;
    val foldl : (t * 'a -> 'a) -> 'a -> t multiset -> 'a;
    val foldr : (t * 'a -> 'a) -> 'a -> t multiset -> 'a;

    val size : t multiset -> int;
    val countUnique : t multiset -> int;

    val equal : t multiset * t multiset -> bool;
    val isEmpty : t multiset -> bool;
    val contains : t multiset -> t -> bool;
    val subset : t multiset -> t multiset -> bool;

    val getFirst : t multiset -> t;
    val countOf : t -> t multiset -> int;
end;

functor Multiset(O :
                 sig
                     type t;
                     val compare : t * t -> order;
                     val fmt : t -> string;
                 end
                ) :> MULTISET where type t = O.t =
struct

type t = O.t;
structure D = Dictionary(struct
                          type k = t;
                          val compare = O.compare;
                          val fmt = O.fmt;
                          end);
type 'a multiset = ('a, int) D.dict;

exception NegativeCount of (t * int);

fun fromCountPairs' ans [] = List.rev ans
  | fromCountPairs' ans ((x, 0)::xs) = fromCountPairs' ans xs
  | fromCountPairs' ans ((x, i)::xs) = if i > 0 then fromCountPairs' (x::ans) ((x, i-1)::xs) else raise NegativeCount (x,i);
fun fromCountPairs xs = fromCountPairs' [] xs;

fun toCountPairs' ans [] = List.rev ans
  | toCountPairs' ((x, i)::xs) (y::ys) =
    if O.compare(x, y) = EQUAL
    then toCountPairs' ((x, i+1)::xs) ys
    else toCountPairs' ((y, 1)::(x, i)::xs) ys
  | toCountPairs' [] (y::ys) = toCountPairs' [(y, 1)] ys;
fun toCountPairs xs = toCountPairs' [] (List.mergesort O.compare xs);


val empty = D.empty;
val fromPairList = D.fromPairList;
val toPairList = D.toPairList;
val fromList = fromPairList o toCountPairs;
val toList = fromCountPairs o toPairList;

fun toString items =
    let
        val printThreshold = 100;
        val stringItems = D.map (fn (k, i) => ("(" ^
                                               (O.fmt k) ^
                                               "," ^
                                               (Int.toString i) ^
                                               ")"))
                                items;
        val tooLong = (D.size items) > printThreshold;
        val mostItems = if tooLong
                        then (List.take (stringItems, printThreshold))
                        else stringItems;
        val joined = String.concatWith ", " mostItems;
    in
        "{" ^ joined ^ (if tooLong then "..." else "") ^ "}"
    end;

exception CountReachedZero;
fun insert xs x =
    let val _ = D.update xs x (fn i => i + 1) in () end
    handle D.KeyError => D.insert xs (x, 1);
fun remove xs x =
    let val _ = D.update xs x (fn i => if i <= 1 then raise CountReachedZero
                                       else i - 1) in () end
    handle CountReachedZero => D.remove xs x;
fun removeAll xs x = D.remove xs x;

fun sum xs ys = D.unionWith (fn (_, i, j) => i + j) xs ys;
fun union xs ys = D.unionWith (fn (_, i, j) => Int.max(i, j)) xs ys;
fun intersection xs ys = D.intersectionWith (fn (_, i, j) => Int.min (i, j)) xs ys;

fun sumAll xs = D.unionAllWith (fn (_, i, j) => i + j) xs;
fun unionAll xs = D.unionAllWith (fn (_, i, j) => Int.max(i, j)) xs;
fun intersectionAll xs = D.intersectionAllWith (fn (_, i, j) => Int.min (i, j)) xs;

fun map f xs = List.map f (toList xs);
fun endomap f xs = fromPairList (List.map (fn (x, i) => (f x, i)) (toPairList xs));
fun filter f xs = D.filter f xs;
fun foldl f a xs = List.foldl f a (toList xs);
fun foldr f a xs = List.foldr f a (toList xs);

fun size xs = D.foldr (fn ((x, i), v) => i + v) 0 xs;
fun countUnique xs = D.size xs;

fun equal (xs, ys) = D.equal (xs, ys);
fun isEmpty xs = D.isEmpty xs;
fun contains xs x =
    let val v = D.get xs x
    in v > 0 end
    handle D.KeyError => false;
fun subset xs ys =
    let fun isLowerCount (x, i) =
            i <= (D.get ys x)
            handle D.KeyError => false;
    in
        D.foldr (fn ((x, i), b) => b andalso isLowerCount (x, i)) true xs
    end;

fun getFirst xs = #1 (D.getFirst xs);
fun countOf x xs = D.get xs x
                   handle D.KeyError => 0;

end;
