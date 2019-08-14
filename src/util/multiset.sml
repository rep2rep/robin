import "util.dictionary";

signature MULTISET =
sig
    type t;
    type 'a multiset;

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
