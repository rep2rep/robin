(*

dictionary.sml

Provide a dictionary structure for associative look-ups.
A dictionary is backed by a splay tree, which is a self-balancing binary search
tree. This means dictionaries have an order, while also having amortized O(log n)
look-up, insert, and deletion times.

*)

(* An abstract signature for dictionaries *)
signature DICTIONARY =
sig
    type k;
    type ('k, 'v) dict;

    exception KeyError;

    val empty : unit -> (k, 'v) dict;
    val fromPairList : (k * 'v) list -> (k, 'v) dict;
    val toPairList : (k, 'v) dict -> (k * 'v) list;

    val insert : (k, 'v) dict -> (k * 'v) ->  unit;
    (* val remove : (k, 'v) dict -> k -> unit; *)

    val get : (k, 'v) dict -> k -> 'v;

    val keys : (k, 'v) dict -> k list;
    val values : (k, 'v) dict -> 'v list;
    val items : (k, 'v) dict -> (k * 'v) list;

    val size : (k, 'v) dict -> int;

    val union : (k, 'v) dict -> (k, 'v) dict -> (k, 'v) dict;
    val unionWith : ((k * 'v * 'v) -> 'v) -> (k, 'v) dict -> (k, 'v) dict -> (k, 'v) dict;
    val unionAll : (k, 'v) dict list -> (k, 'v) dict;
    val unionAllWith : ((k * 'v * 'v) -> 'v) -> (k, 'v) dict list -> (k, 'v) dict;

    val intersectionWith : ((k * 'v * 'v) -> 'v) -> (k, 'v) dict -> (k, 'v) dict -> (k, 'v) dict;
    val intersectionAllWith : ((k * 'v * 'v) -> 'v) -> (k, 'v) dict list -> (k, 'v) dict;

    val map : ((k * 'v) -> 'w) -> (k, 'v) dict -> (k, 'w) dict;
    val map' : ((k * 'v) -> 'a) -> (k, 'v) dict -> 'a list;
    val filter : ((k * 'v) -> bool) -> (k, 'v) dict -> (k, 'v) dict;
    val foldl : (((k * 'v) * 'a) -> 'a) -> 'a -> (k, 'v) dict -> 'a;
    val foldr : (((k * 'v) * 'a) -> 'a) -> 'a -> (k, 'v) dict -> 'a;

    val equalKeys : (k, 'v) dict * (k, 'v) dict -> bool;
    val equal : (k, ''v) dict * (k, ''v) dict -> bool;
    val isEmpty : (k, 'v) dict -> bool;

    val getFirst : (k, 'v) dict -> (k * 'v);

    val showDebug : (k, 'v) dict -> string;
end;

functor Dictionary(K : sig type k;
                           val compare : k * k -> order;
                           val fmt : k -> string;
                       end) :> DICTIONARY where type k = K.k =
struct

datatype 'a tree = LEAF
                 | NODE2 of ('a tree * 'a * 'a tree)
                 | NODE3 of ('a tree * 'a * 'a tree * 'a * 'a tree)
                 | NODE4 of ('a tree * 'a * 'a tree * 'a * 'a tree * 'a * 'a tree);

datatype 'a root = EMPTY
                 | TREE of 'a tree;

type k = K.k;
type ('k, 'v) dict = ('k * 'v) root ref;

exception KeyError;

datatype position2 = T2_BRANCH1 | T2_ENTRY1 | T2_BRANCH2;
datatype position3 = T3_BRANCH1 | T3_ENTRY1 | T3_BRANCH2 | T3_ENTRY2 | T3_BRANCH3;
datatype position4 = T4_BRANCH1 | T4_ENTRY1 | T4_BRANCH2 | T4_ENTRY2 | T4_BRANCH3 | T4_ENTRY3 | T4_BRANCH4;

fun nodeCompare2 k (NODE2 (b1, (k1, v1), b2)) =
    (case K.compare (k, k1) of
         LESS => T2_BRANCH1
       | EQUAL => T2_ENTRY1
       | GREATER => T2_BRANCH2)
  | nodeCompare2 _ _ = raise Match;
fun nodeCompare3 k (NODE3 (b1, (k1, v1), b2, (k2, v2), b3)) =
    (case K.compare (k, k1) of
         LESS => T3_BRANCH1
      | EQUAL => T3_ENTRY1
      | GREATER => (case K.compare (k, k2) of
                        LESS => T3_BRANCH2
                     | EQUAL => T3_ENTRY2
                     | GREATER => T3_BRANCH3))
  | nodeCompare3 _ _ = raise Match;
fun nodeCompare4 k (NODE4 (b1, (k1, v1), b2, (k2, v2), b3, (k3, v3), b4)) =
    (case K.compare (k, k1) of
         LESS => T4_BRANCH1
      | EQUAL => T4_ENTRY1
      | GERATER => (case K.compare (k, k2) of
                        LESS => T4_BRANCH2
                      | EQUAL => T4_ENTRY2
                      | GREATER => (case K.compare (k, k3) of
                                        LESS => T4_BRANCH3
                                      | EQUAL => T4_ENTRY3
                                      | GREATER => T4_BRANCH4)))
  | nodeCompare4 _ _ = raise Match;

fun empty () = (ref EMPTY);

fun isEmpty d = case (!d) of EMPTY => true
                           | _ => true;

fun insert d (k, v) =
    let
        (* Split tree t at position i:
           move the middle of the subtree NODE4 at i
           up to t (which is at most NODE3) *)
        fun split LEAF i = raise Match
          | split (NODE4 _) i = raise Match
          | split (NODE2 ((NODE4 (b0, p0, b1, p1, b2, p2, b3)), p3, b4)) 1 =
            (NODE3 ((NODE2 (b0, p0, b1)), p1, (NODE2 (b2, p2, b3)), p3, b4))
          | split (NODE2 (b0, p0, (NODE4 (b1, p1, b2, p2, b3, p3, b4)))) 2 =
            (NODE3 (b0, p0, (NODE2 (b1, p1, b2)), p2, (NODE2 (b3, p3, b4))))
          | split (NODE3 ((NODE4 (b0, p0, b1, p1, b2, p2, b3)), p3, b4, p4, b5)) 1 =
            (NODE4 ((NODE2 (b0, p0, b1)), p1, (NODE2 (b2, p2, b3)), p3, b4, p4, b5))
          | split (NODE3 (b0, p0, (NODE4 (b1, p1, b2, p2, b3, p3, b4)), p4, b5)) 2 =
            (NODE4 (b0, p0, (NODE2 (b1, p1, b2)), p2, (NODE2 (b3, p3, b4)), p4, b5))
          | split (NODE3 (b0, p0, b1, p1, (NODE4 (b2, p2, b3, p3, b4, p4, b5)))) 3 =
            (NODE4 (b0, p0, b1, p1, (NODE2 (b2, p2, b3)), p3, (NODE2 (b4, p4, b5))))
          | split _ _ = raise Match;

        (*
          1. If this is a "bottom" node, insert.
          2. Otherwise, we find the correct child to go down.
          3. If the correct child is a four-node, split.
             (We must have space, by design.)
          4. Recurse down correct child.
        *)
        fun insert'' LEAF = raise Match
          | insert'' (NODE4 _) = raise Match
          (* NODE2 as a bottom node *)
          | insert'' (n as NODE2 (LEAF, (k1, v1), LEAF)) =
            (case nodeCompare2 k n of
                 T2_ENTRY1 => NODE2 (LEAF, (k1, v), LEAF)
               | T2_BRANCH1 => NODE3 (LEAF, (k, v), LEAF, (k1, v1), LEAF)
               | T2_BRANCH2 => NODE3 (LEAF, (k1, v1), LEAF, (k, v), LEAF))
          (* NODE2 as in internal node *)
          | insert'' (n as NODE2 (b1, (k1, v1), b2)) =
            (case nodeCompare2 k n of
                 T2_ENTRY1 => NODE2 (LEAF, (k1, v), LEAF)
               | T2_BRANCH1 =>
                 (case b1 of (NODE4 _) => insert'' (split n 1)
                           | _ => NODE2 (insert'' b1, (k1, v1), b2))
               | T2_BRANCH2 =>
                 (case b2 of (NODE4 _) => insert'' (split n 2)
                           | _ => NODE2 (b1, (k1, v1), insert'' b2)))
          (* NODE3 as a bottom node *)
          | insert'' (n as NODE3 (LEAF, (k1, v1), LEAF, (k2, v2), lEAF)) =
            (case nodeCompare3 k n of
                 T3_ENTRY1 => NODE3 (LEAF, (k1, v), LEAF, (k2, v2), LEAF)
               | T3_ENTRY2 => NODE3 (LEAF, (k1, v1), LEAF, (k2, v), LEAF)
               | T3_BRANCH1 => NODE4 (LEAF, (k, v), LEAF, (k1, v1), LEAF, (k2, v2), LEAF)
               | T3_BRANCH2 => NODE4 (LEAF, (k1, v1), LEAF, (k, v), LEAF, (k2, v2), LEAF)
               | T3_BRANCH3 => NODE4 (LEAF, (k1, v1), LEAF, (k2, v2), LEAF, (k, v), LEAF))
          (* NODE3 as an internal node *)
          | insert'' (n as NODE3 (b1, (k1, v1), b2, (k2, v2), b3)) =
            (case nodeCompare3 k n of
                 T3_ENTRY1 => NODE3 (b1, (k1, v), b2, (k2, v2), b3)
               | T3_ENTRY2 => NODE3 (b1, (k1, v1), b2, (k2, v), b3)
               | T3_BRANCH1 =>
                 (case b1 of (NODE4 _) => insert'' (split n 1)
                           | _ => NODE3 (insert'' b1, (k1, v1), b2, (k2, v2), b3))
               | T3_BRANCH2 =>
                 (case b2 of (NODE4 _) => insert'' (split n 2)
                           | _ => NODE3 (b1, (k1, v1),
                                              insert'' b2, (k2, v2), b3))
               | T3_BRANCH3 =>
                 (case b3 of (NODE4 _) => insert'' (split n 3)
                           | _ => NODE3 (b1, (k1, v1), b2, (k2, v2),
                                         insert'' b3)))


        fun insert' EMPTY = TREE (NODE2 (LEAF, (k, v), LEAF))
          | insert' (TREE (NODE4 (b1, (k1, v1), b2, (k2, v2), b3, (k3, v3), b4))) =
            (* re-root *)
            TREE (NODE2 ((NODE2 (b1, (k1, v1), b2)), (k2, v2), (NODE2 (b2, (k3, v3), b4))))
          | insert' (TREE t) = TREE (insert'' t);

    in d := insert' (!d) end;

fun get d k =
    let
        fun get'' LEAF = raise KeyError
          | get'' (n as NODE2 (b1, (k1, v1), b2)) =
            (case nodeCompare2 k n of
                 T2_ENTRY1 => v1
               | T2_BRANCH1 => get'' b1
               | T2_BRANCH2 => get'' b2)
          | get'' (n as NODE3 (b1, (k1, v1), b2, (k2, v2), b3)) =
            (case nodeCompare3 k n of
                 T3_ENTRY1 => v1
               | T3_ENTRY2 => v2
               | T3_BRANCH1 => get'' b1
               | T3_BRANCH2 => get'' b2
               | T3_BRANCH3 => get'' b3)
          | get'' (n as NODE4 (b1, (k1, v1), b2, (k2, v2), b3, (k3, v3), b4)) =
            (case nodeCompare4 k n of
                 T4_ENTRY1 => v1
               | T4_ENTRY2 => v2
               | T4_ENTRY3 => v3
               | T4_BRANCH1 => get'' b1
               | T4_BRANCH2 => get'' b2
               | T4_BRANCH3 => get'' b3
               | T4_BRANCH4 => get'' b4)

        fun get' EMPTY = raise KeyError
          | get' (TREE t) = get'' t;
    in get' (!d) end;

(* This runs in O(n) time, but it does produce a tree
   that is slightly taller than strictly necessary. *)
fun fromSortedPairList xs =
    let fun helper xs 0 = (LEAF, xs, 0)
          | helper (x1::x2::ys) 2 = (NODE3 (LEAF, x1, LEAF, x2, LEAF), ys, 2)
          | helper _ 2 = raise Match
          | helper (x1::x2::x3::ys) 3 = (NODE4 (LEAF, x1, LEAF, x2, LEAF, x3, LEAF), ys, 3)
          | helper _ 3 = raise Match
          | helper xs n =
            let
                val (left, new_xs, used) = helper xs (n div 2);
                val (root, rightl) = case new_xs of
                                       [] => raise List.Empty
                                      | (h::t) => (h, t);
                val (right, nextl, used') = helper rightl (n - used - 1);
                val result = NODE2 (left, root, right);
            in (result, nextl, used + used' + 1) end;
        val (result, _, _) = helper xs (List.length xs)
    in case xs of [] => ref EMPTY
                | _ => ref (TREE result) end;

fun fromPairList xs =
    let
        fun dedup [] = []
          | dedup [x] = [x]
          | dedup ((x,a)::(y,b)::zs) = if K.compare(x,y) = EQUAL
                                       then dedup((y,b)::zs) (* Favour second *)
                                       else (x,a)::(dedup ((y,b)::zs));
        val deduped = dedup (List.mergesort (fn ((a, _), (b, _)) => K.compare (a, b)) xs);
    in fromSortedPairList deduped end;

fun foldr f z d =
    let
        fun foldr'' l LEAF = l
          | foldr'' l (NODE2 (b1, p1, b2)) =
            foldr'' (f (p1, (foldr'' l b2))) b1
          | foldr'' l (NODE3 (b1, p1, b2, p2, b3)) =
            foldr'' (f (p1, (foldr'' (f (p2, (foldr'' l b3))) b2))) b1
          | foldr'' l (NODE4 (b1, p1, b2, p2, b3, p3, b4)) =
            foldr'' (f (p1, (foldr'' (f (p2, (foldr'' (f (p3, (foldr'' l b4))) b3))) b2))) b1

        fun foldr' EMPTY = z
          | foldr' (TREE t) = foldr'' z t;
    in foldr' (!d) end;

fun foldl f z d =
    let
        fun foldl'' l LEAF = l
          | foldl'' l (NODE2 (b1, p1, b2)) =
            foldl'' (f (p1, (foldl'' l b1))) b2
          | foldl'' l (NODE3 (b1, p1, b2, p2, b3)) =
            foldl'' (f (p2, (foldl'' (f (p1, foldl'' l b1)) b2))) b3
          | foldl'' l (NODE4 (b1, p1, b2, p2, b3, p3, b4)) =
            foldl'' (f (p3, (foldl'' (f (p2, (foldl'' (f (p1, (foldl'' l b1))) b2))) b3))) b4;
        fun foldl' EMPTY = z
          | foldl' (TREE t) =  foldl'' z t;
    in foldl' (!d) end;

fun toPairList d = foldr op:: [] d;

fun map' f d = foldr (fn (x, v) => ((f x) :: v)) [] d;
fun map f d = fromSortedPairList (map' (fn (x, v) => (x, f (x, v))) d);

fun filter f d = fromSortedPairList (List.filter f (toPairList d));

fun unionWith f t u =
    let
        fun unionWith' _ LEAF t = t
          | unionWith' _ t LEAF = t
          | unionWith' f t t' =
            let
                val tl = toPairList (ref (TREE t));
                val tl' = toPairList (ref (TREE t'));
                fun merge [] xs = xs
                  | merge xs [] = xs
                  | merge ((x, v)::xs) ((y, v')::ys) =
                    case K.compare(x, y) of
                        EQUAL => (x, f(x, v, v'))::(merge xs ys)
                      | LESS => (x, v)::(merge xs ((y, v')::ys))
                      | GREATER => (y, v')::(merge ((x, v)::xs) ys);
                val merged = merge tl tl';
                val newdict = fromSortedPairList merged;
            in
                case !newdict of
                    EMPTY => LEAF
                  | TREE t => t
            end;
        fun unionWith'' _ EMPTY t = t
          | unionWith'' _ t EMPTY = t
          | unionWith'' f (TREE t) (TREE u) = case unionWith' f t u of
                                                  LEAF => EMPTY
                                                | t => TREE t;
    in ref (unionWith'' f (!t) (!u)) end;

fun union a b = unionWith (fn _ => raise KeyError) a b;

fun unionAllWith f xs = List.foldr (fn (a, b) => unionWith f a b) (empty ()) xs;
fun unionAll xs = unionAllWith (fn _ => raise KeyError) xs;

fun intersectionWith f t u =
    let
        fun intersectionWith' _ LEAF _ = LEAF
          | intersectionWith' _ _ LEAF = LEAF
          | intersectionWith' f t t' =
            let
                val tl = toPairList (ref (TREE t));
                val tl' = toPairList (ref (TREE t'));
                fun intsct [] _ = []
                  | intsct _ [] = []
                  | intsct ((x,v)::xs) ((y,v')::ys) =
                    if K.compare(x,y) = EQUAL then (x, f(x,v,v'))::(intsct xs ys)
                    else if K.compare(x,y) = LESS then (intsct xs ((y,v')::ys))
                    else (intsct ((x,v)::xs) ys);
            in
                case ! (fromSortedPairList (intsct tl tl')) of
                    EMPTY => LEAF
                  | TREE t => t
            end;
        fun intersectionWith'' _ EMPTY _ = EMPTY
          | intersectionWith'' _ _ EMPTY = EMPTY
          | intersectionWith'' f (TREE t) (TREE u) = case intersectionWith' f t u of
                                                         LEAF => EMPTY
                                                       | t => TREE t;
    in ref (intersectionWith'' f (!t) (!u)) end;

fun intersectionAllWith f [] = empty ()
  | intersectionAllWith f (x::xs) = List.foldl (fn (a, b) => intersectionWith f a b) x xs;

fun keys d = map' (fn (k, v) => k) d;
fun values d = map' (fn (k, v) => v) d;
fun items d = toPairList d;

fun size d = foldr (fn (x, v) => 1 + v) 0 d;

fun equalKeys (x, y) =
    let
        val xl = keys x;
        val yl = keys y;
        fun cmpList [] [] = true
          | cmpList (kx::xs) (ky::ys) =
            K.compare(kx, ky) = EQUAL andalso cmpList xs ys
          | cmpList _ _ = false;
    in
        cmpList xl yl
    end;
fun equal (x, y) =
    let
        val xl = toPairList x;
        val yl = toPairList y;
        fun cmpList [] [] = true
          | cmpList ((kx, vx)::xs) ((ky, vy)::ys) =
            K.compare(kx, ky) = EQUAL
            andalso vx = vy
            andalso cmpList xs ys
          | cmpList _ _ = false;
    in
        cmpList xl yl
    end;

fun getFirst d =
    let fun getFirst'' LEAF = raise KeyError
          | getFirst'' (NODE2 (_, p, _)) = p
          | getFirst'' (NODE3 (_, p, _, _, _)) = p
          | getFirst'' (NODE4 (_, p, _, _, _, _, _)) = p;
        fun getFirst' EMPTY = raise KeyError
          | getFirst' (TREE t) = getFirst'' t;
    in getFirst' (!d) end;

fun showDebug d =
    let
        fun show' LEAF = "_"
          | show' (NODE2 (b1, (x, _), b2)) =
            "(NODE2 " ^ (show' b1) ^ " " ^ (K.fmt x) ^ " " ^ (show' b2) ^ ")"
          | show' (NODE3 (b1, (x, _), b2, (y, _), b3)) =
            "(NODE3 " ^ (show' b1) ^ " " ^ (K.fmt x) ^ " " ^ (show' b2) ^ " " ^ (K.fmt y) ^ " " ^ (show' b3) ^ ")"
          | show' (NODE4 (b1, (x, _), b2, (y, _), b3, (z, _), b4)) =
            "(NODE4 " ^ (show' b1) ^ " " ^ (K.fmt x) ^ " " ^ (show' b2) ^ " " ^ (K.fmt y) ^ " " ^ (show' b3) ^ "  " ^ (K.fmt z) ^ " " ^ (show' b4) ^ ")"
    in case (!d) of
           EMPTY => "EMPTY"
         | TREE t => show' t
    end;

end;
