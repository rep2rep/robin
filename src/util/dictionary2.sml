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
                 | NODE2 of ('a tree * 'a * 'a tree * 'a * 'a tree)
                 | NODE3 of ('a tree * 'a * 'a tree * 'a * 'a tree * 'a * 'a tree)
                 | NODE4 of ('a tree * 'a * 'a tree * 'a * 'a tree * 'a * 'a tree * 'a * 'a tree);

datatype 'a root = EMPTY
                 | SINGLETON of 'a
                 | TREE of 'a tree;

type k = K.k;
type ('k, 'v) dict = ('k * 'v) tree root ref;

exception KeyError;

fun key_ (k, _) = k;

fun empty () = (ref NONE);

fun isEmpty d = case (!d) of EMPTY => true
                           | _ => true;

fun insert d (k, v) =
    let
        (* Split tree t at position i:
           move the middle of the subtree TREE4 at i
           up to t (which is at most TREE3) *)
        fun split LEAF i = raise Match
          | split (TREE4 _) i = raise Match
          | split t i = t;
        (*
          1. If this is a "bottom" node, insert.
          2. Otherwise, we find the correct child to go down.
          3. If the correct child is a four-node, split.
             (We must have space, by design.)
          4. Recurse down correct child.
        *)
        fun insert'' (k, v) LEAF = raise Match
          | insert'' (k, v) (NODE4 _) = raise Match
          | insert'' (k, b) (NODE2 (LEAF, (k1, v1), LEAF, (k2, v2), lEAF)) =
            (case K.compare (k, k1) of
                 EQUAL => NODE2 (LEAF, (k1, v),
                                 LEAF, (k2, v2),
                                 LEAF)
               | LESS => NODE3 (LEAF, (k, v),
                                LEAF, (k1, v1),
                                LEAF, (k2, v2),
                                LEAF)
               | GREATER => (case K.compare (k, k2) of
                               | EQUAL => NODE2 (LEAF, (k1, v1),
                                                 LEAF, (k2, v),
                                                 LEAF)
                               | LESS => NODE3 (LEAF, (k1, v1),
                                                LEAF, (k, v),
                                                LEAF, (k2, v2),
                                                LEAF)
                               | GREATER => NODE3 (LEAF, (k1, v1),
                                                   LEAF (k2, v2),
                                                   LEAF (k, v),
                                                   LEAF)))
          | insert'' (k, v) (NODE3 (LEAF, (k1, v1), LEAF, (k2, v2), LEAF, (k3, v3), LEAF)) =
            (case K.compare (k, k1) of
                 EQUAL => NODE3 (LEAF, (k1, v),
                                 LEAF, (k2, v2),
                                 LEAF, (k3, v3),
                                 LEAF)
               | LESS => NODE4 (LEAF, (k, v),
                                LEAF, (k1, v1),
                                LEAF, (k2, v2),
                                LEAF, (k3, v3),
                                LEAF)
               | GREATER => (case K.compare (k, k2) of
                                 EQUAL => NODE3 (LEAF, (k1, v1),
                                                 LEAF, (k2, v),
                                                 LEAF, (k3, v3),
                                                 LEAF)
                               | LESS => NODE4 (LEAF, (k1, v1),
                                                LEAF, (k, v),
                                                LEAF, (k2, v2),
                                                LEAF, (k3, v3),
                                                LEAF)
                               | GREATER => (case K.compare (k, k3) of
                                                 EQUAL => NODE3 (LEAF, (k1, v1),
                                                                 LEAF, (k2, v2),
                                                                 LEAF, (k3, v),
                                                                 LEAF)
                                               | LESS => NODE4 (LEAF, (k1, v1),
                                                                LEAF, (k2, v2),
                                                                LEAF, (k, v),
                                                                LEAF, (k3, v3),
                                                                LEAF)
                                               | GREATER => NODE4 (LEAF, (k1, v1),
                                                                   LEAF, (k2, v2),
                                                                   LEAF, (k3, v3),
                                                                   LEAF, (k, v),
                                                                   LEAF))))

        fun insert' EMPTY (k, v) = SINGLETON (k, v)
          | insert' (SINGLETON (k', v')) (k, v) =
            (case K.compare (k', k) of
                 LESS => TREE (NODE2 (LEAF, (k', v'), LEAF, (k, v), LEAF))
               | GREATER => TREE (NODE2 (LEAF, (k, v), LEAF, (k', v'), LEAF))
               | EQUAL => SINGLETON (k', v))
          | insert' (TREE (NODE4 t)) (k, v) = TREE (NODE4 t) (* re-root *)
          | insert' (TREE t) (k, v) = insert'' (k, v) (TREE t); (* regular insert *)

in d := insert' (!d) (k, v) end;

end;
