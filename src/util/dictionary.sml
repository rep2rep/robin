(*

dictionary.sml

Provide a dictionary structure for associative look-ups. For now, a
dictionary is backed by a list of tuples, ordered by the first pair.
This is clearly a slow and unsustainable approach.

*)

(* An abstract signature for dictionaries *)
signature DICTIONARY =
sig
    type k;
    type ('k, 'v) dict;

    exception KeyError;

    val empty : (k, 'v) dict;
    val fromPairList : (k * 'v) list -> (k, 'v) dict;
    val toPairList : (k, 'v) dict -> (k * 'v) list;

    val insert : (k, 'v) dict -> (k * 'v) ->  (k, 'v) dict;
    val remove : (k, 'v) dict -> k -> (k, 'v) dict;

    val get : (k, 'v) dict -> k -> 'v;

    val keys : (k, 'v) dict -> k list;
    val values : (k, 'v) dict -> 'v list;
    val items : (k, 'v) dict -> (k * 'v) list;
    val size : (k, 'v) dict -> int;

    val union : (k, 'v) dict -> (k, 'v) dict -> (k, 'v) dict;
    val unionWith : ((k * 'v * 'v) -> 'v) -> (k, 'v) dict -> (k, 'v) dict -> (k, 'v) dict;

    val intersectionWith : ((k * 'v * 'v) -> 'v) -> (k, 'v) dict -> (k, 'v) dict -> (k, 'v) dict;

    val map : ((k * 'v) -> 'a) -> (k, 'v) dict -> 'a list; (* It would be nice to have this work to dictionaries *)
    val filter : ((k * 'v) -> bool) -> (k, 'v) dict -> (k, 'v) dict;
    val foldl : (('a * (k * 'v)) -> 'a) -> 'a -> (k, 'v) dict -> 'a;
    val foldr : (('a * (k * 'v)) -> 'a) -> 'a -> (k, 'v) dict -> 'a;

    val isEmpty : (k, 'v) dict -> bool;
end;


(* A functor to produce dictionaries keyed with Ks.
   While we can be polymorphic in the values (i.e., so long as the type
   of the values are all the same, we don't care what they are), we
   impose a restriction upon the keys: in this case, the keys must be
   orderable through a compare function.
*)
functor Dictionary(K :
                   sig
                       type k;
                       val compare : k * k -> order;
                   end
                  ) :> DICTIONARY where type k = K.k =
struct

type k = K.k;
type ('k, 'v) dict = ('k * 'v) list; (* For now *)

exception KeyError;

val empty = [];
fun fromPairList xs =
    let
        fun dedup [] = []
          | dedup [(k, v)] = [(k, v)]
          | dedup ((a,b)::(x,y)::zs) = if K.compare(a,x) = EQUAL then dedup ((a,y)::zs)
                               else ((a,b)::(dedup ((x,y)::zs)));
    in
        dedup (mergesort (fn ((a, b), (x, y)) => K.compare(a,x)) xs)
    end;
fun toPairList xs = xs;

fun insert [] (x,y) = [(x,y)]
  | insert ((k,v)::ys) (x, y) = if K.compare(x, k) = EQUAL then (x,y)::ys
                               else if K.compare(x, k) = GREATER then (k,v)::(insert ys (x,y))
                               else (x,y)::(k,v)::ys;
fun remove [] _ = []
  | remove ((k,v)::ys) x = if K.compare(x, k) = EQUAL then ys
                           else if K.compare(x, k) = GREATER then (k,v)::(remove ys x)
                           else (k,v)::ys;

fun get [] _ = raise KeyError
  | get ((k, v)::xs) x = if K.compare(x,k) = EQUAL then v
                         else if K.compare(x,k) = LESS then raise KeyError
                         else get xs x;

fun keys [] = []
  | keys ((k,v)::xs) = k::(keys xs);
fun values [] = []
  | values ((k,v)::xs) = v::(values xs);
fun items d = toPairList d;
fun size [] = 0
  | size (x::xs) = 1 + size xs;

fun unionWith _ [] xs = xs
  | unionWith _ xs [] = xs
  | unionWith f ((a,b)::xs) ((x,y)::ys) = if K.compare(a,x) = EQUAL then (a, f(a, b, y))::(unionWith f xs ys)
                                          else if K.compare(a,x) = LESS then (a,b)::(unionWith f xs ((x,y)::ys))
                                          else (x,y)::(unionWith f ((a,b)::xs) ys);
fun union a b = unionWith (fn (k, v1, v2) => raise KeyError) a b;

fun intersectionWith _ [] _ = []
  | intersectionWith _ _ [] = []
  | intersectionWith f ((a,b)::xs) ((x,y)::ys) = if K.compare(a,x) = EQUAL then (a, f(a,b, y))::(intersectionWith f xs ys)
                                                 else if K.compare(a,x) = LESS then intersectionWith f xs ((x,y)::ys)
                                                 else intersectionWith f ((a,b)::xs) ys;
fun map a b =
    let
        fun map' _ [] = []
          | map' f ((k,v)::xs) = (f (k, v))::(map' f xs);
    in
        map' a b
    end;
fun filter a b =
    let
        fun filter' f [] = []
          | filter' f (x::xs) = if (f x) then x::(filter' f xs)
                                else filter' f xs;
    in
        filter' a b
    end;

fun foldl a b c =
    let
        fun foldl' f s [] = s
          | foldl' f s (x::xs) = foldl' f (f (s, x)) xs;
    in
        foldl' a b c
    end;
fun foldr a b c =
    let
        fun foldr' f s [] = s
          | foldr' f s (x::xs) = f ((foldr' f s xs), x);
    in
        foldr' a b c
    end;

fun isEmpty [] = true
  | isEmpty _ = false;

end;
