(*

set.sml

Provide a set structure for use throughout robin. Sets are backed by
dictionaries: the values are stored as keys, all associated with unit.
This means any improvements made to the dictionary data structure are
automatically inherited by the set structure.

*)

use "base.sml";
use (BASE^"util/robinlib.sml");
use (BASE^"util/dictionary.sml");



(* An abstract interface for what a set can do. *)
signature SET =
sig
    type t;
    type 'a set;

    val empty : t set;
    val fromList : t list -> t set;
    val toList : t set -> t list;

    val insert : t -> t set -> t set;
    val remove : t -> t set -> t set;

    val union : t set -> t set -> t set;
    val intersection : t set -> t set -> t set;
    val difference : t set -> t set -> t set;

    val map : (t -> 'a) -> t set -> 'a list; (* Yes, it really should return 'a set... but that's really hard! *)
    val filter : (t -> bool) -> t set -> t set;
    val foldl : ('a * t -> 'a) -> 'a -> t set -> 'a;
    val foldr: ('a * t -> 'a) -> 'a -> t set -> 'a;

    val size : t set -> int;

    val isEmpty : t set -> bool;
    val contains : t set -> t -> bool;
    val subset : t set -> t set -> bool;
end;



(* A functor to create set structures.
   We use a functor so that different sets will "just work" for
   different types. In this case any structure that has compare will
   work fine.
 *)
functor Set(O :
            sig
                type t;
                val compare : t * t -> order;
            end
           ) :> SET where type t = O.t =
struct

type t = O.t;
structure D = Dictionary(struct
                          type k = t;
                          val compare = O.compare;
                          end);
type 'a set = ('a, unit) D.dict;

val empty = D.empty;
fun fromList xs = D.fromPairList (map (fn x => (x, ())) xs);
fun toList xs = map (fn (x,_) => x) (D.toPairList xs);

fun insert x xs = D.insert (x, ()) xs;
fun remove x xs = D.remove x xs;

fun union xs ys = D.unionWith (fn (_, _, _) => ()) xs ys;
fun intersection xs ys = D.intersectionWith (fn (_, _, _) => ()) xs ys;
fun difference xs ys = D.foldl (fn (s, (v,_)) => (remove v s)) xs ys;

fun map f xs = D.map (fn (k, v) => f k) xs;
fun filter f xs = D.filter (fn (k, v) => f k) xs;
fun foldl f s xs = D.foldl (fn (x, (k, v)) => f(x, k)) s xs;
fun foldr f s xs = D.foldr (fn (x, (k, v)) => f (x, k)) s xs;

fun size xs = D.size xs;

fun isEmpty xs = D.isEmpty xs;
fun contains xs x =
    let
        val v = D.get x xs
    in
        true
    end
    handle KeyError => false;
fun subset xs ys =
    let
        val contained = map (fn x => contains ys x) xs;
    in
        RobinLib.all contained
    end;

end;
