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
    (* val fromPairList : (k * 'v) list -> (k, 'v) dict; *)
    (* val toPairList : (k, 'v) dict -> (k * 'v) list; *)

    val insert : (k, 'v) dict -> (k * 'v) ->  unit;
    (* val remove : (k, 'v) dict -> k -> unit; *)

    (* val get : (k, 'v) dict -> k -> 'v; *)
    (* val update : (k, 'v) dict -> k -> ('v -> 'v) -> 'v; *)

    (* val keys : (k, 'v) dict -> k list; *)
    (* val values : (k, 'v) dict -> 'v list; *)
    (* val items : (k, 'v) dict -> (k * 'v) list; *)

    (* val size : (k, 'v) dict -> int; *)

    (* val union : (k, 'v) dict -> (k, 'v) dict -> (k, 'v) dict; *)
    (* val unionWith : ((k * 'v * 'v) -> 'v) -> (k, 'v) dict -> (k, 'v) dict -> (k, 'v) dict; *)
    (* val unionAll : (k, 'v) dict list -> (k, 'v) dict; *)
    (* val unionAllWith : ((k * 'v * 'v) -> 'v) -> (k, 'v) dict list -> (k, 'v) dict; *)

    (* val intersectionWith : ((k * 'v * 'v) -> 'v) -> (k, 'v) dict -> (k, 'v) dict -> (k, 'v) dict; *)
    (* val intersectionAllWith : ((k * 'v * 'v) -> 'v) -> (k, 'v) dict list -> (k, 'v) dict; *)

 (* It would be nice to have map work to dictionaries *)
    (* val map : ((k * 'v) -> 'a) -> (k, 'v) dict -> 'a list; *)
    (* val filter : ((k * 'v) -> bool) -> (k, 'v) dict -> (k, 'v) dict; *)
    (* val foldl : (((k * 'v) * 'a) -> 'a) -> 'a -> (k, 'v) dict -> 'a; *)
    (* val foldr : (((k * 'v) * 'a) -> 'a) -> 'a -> (k, 'v) dict -> 'a; *)
    (* val find : (k * 'v -> bool) -> (k, 'v) dict -> (k * 'v) option; *)

    (* val equalKeys : (k, 'v) dict * (k, 'v) dict -> bool; *)
    (* val equal : (k, ''v) dict * (k, ''v) dict -> bool; *)
    val isEmpty : (k, 'v) dict -> bool;

    (* val getFirst : (k, 'v) dict -> (k * 'v); *)
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

fun key_ (k, _) = k;

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
        fun insert'' (k, v) LEAF = raise Match
          | insert'' (k, v) (NODE4 _) = raise Match
          (* NODE2 as a bottom node *)
          | insert'' (k, v) (NODE2 (LEAF, (k1, v1), LEAF)) =
            (case K.compare (k, k1) of
                 EQUAL => NODE2 (LEAF, (k1, v), LEAF)
               | LESS => NODE3 (LEAF, (k, v), LEAF, (k1, v1), LEAF)
               | GREATER => NODE3 (LEAF, (k1, v1), LEAF, (k, v), LEAF))
          (* NODE2 as in internal node *)
          | insert'' (k, v) (n as NODE2 (b1, (k1, v1), b2)) =
            (case K.compare (k, k1) of
                 EQUAL => NODE2 (LEAF, (k1, v), LEAF)
               | LESS =>
                 (case b1 of (NODE4 _) => insert'' (k, v) (split n 1)
                           | _ => NODE2 (insert'' (k, v) b1, (k1, v1), b2))
               | GREATER =>
                 (case b2 of (NODE4 _) => insert'' (k, v) (split n 2)
                           | _ => NODE2 (b1, (k1, v1), insert'' (k, v) b2)))
          (* NODE3 as a bottom node *)
          | insert'' (k, b) (NODE3 (LEAF, (k1, v1), LEAF, (k2, v2), lEAF)) =
            (case K.compare (k, k1) of
                 EQUAL => NODE3 (LEAF, (k1, v), LEAF, (k2, v2), LEAF)
               | LESS => NODE4 (LEAF, (k, v), LEAF, (k1, v1), LEAF, (k2, v2), LEAF)
               | GREATER =>
                 (case K.compare (k, k2) of
                      EQUAL => NODE3 (LEAF, (k1, v1), LEAF, (k2, v), LEAF)
                    | LESS => NODE4 (LEAF, (k1, v1), LEAF, (k, v), LEAF, (k2, v2), LEAF)
                    | GREATER => NODE4 (LEAF, (k1, v1), LEAF, (k2, v2),
                                        LEAF, (k, v), LEAF)))
          (* NODE3 as an internal node *)
          | insert'' (k, v) (n as NODE3 (b1, (k1, v1), b2, (k2, v2), b3)) =
            (case K.compare(k, k1) of
                 EQUAL => NODE3 (b1, (k1, v), b2, (k2, v2), b3)
               | LESS =>
                 (case b1 of (NODE4 _) => insert'' (k, v) (split n 1)
                           | _ => NODE3 (insert'' (k, v) b1, (k1, v1), b2, (k2, v2), b3))
               | GREATER =>
                 (case K.compare(k, k2) of
                      EQUAL => NODE3 (b1, (k1, v1), b2, (k2, v), b3)
                    | LESS =>
                      (case b2 of (NODE4 _) => insert'' (k, v) (split n 2)
                                | _ => NODE3 (b1, (k1, v1),
                                              insert'' (k, v) b2, (k2, v2), b3))
                    | GERATER =>
                      (case b3 of (NODE4 _) => insert'' (k, v) (split n 3)
                                | _ => NODE3 (b1, (k1, v1), b2, (k2, v2),
                                              insert'' (k, v) b3))))


        fun insert' EMPTY (k, v) = TREE (NODE2 (LEAF, (k, v), LEAF))
          | insert' (TREE (NODE4 (b1, (k1, v1), b2, (k2, v2), b3, (k3, v3), b4))) (k, v) =
            (* re-root *)
            TREE (NODE2 ((NODE2 (b1, (k1, v1), b2)), (k2, v2), (NODE2 (b2, (k3, v3), b4))))
          | insert' (TREE t) (k, v) = TREE (insert'' (k, v) t);

in d := insert' (!d) (k, v) end;

end;
