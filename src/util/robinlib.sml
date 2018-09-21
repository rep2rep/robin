(*

robinlib.sml

Define some useful things for the whole robin system.

*)

use "base.sml";

signature ROBINLIB =
sig
    val mergesort : ('a * 'a -> order) -> 'a list -> 'a list;
    val intersperse : 'a -> 'a list -> 'a list;
    val enumerate : 'a list -> (int * 'a) list;
    val enumerateFrom : int -> 'a list -> (int * 'a) list;
end;


structure RobinLib : ROBINLIB =
struct

fun mergesort cmp [] = []
  | mergesort cmp [x] = [x]
  | mergesort cmp items =
    let
        fun split [] = ([], [])
          | split [x] = ([x], [])
          | split (x::y::zs) = let val (left, right) = split zs
                               in (x::left, y::right) end;
        fun merge [] xs = xs
          | merge xs [] = xs
          | merge (x::xs) (y::ys) =
            if cmp(x, y) = LESS then
                x::(merge xs (y::ys))
            else
                y::(merge (x::xs) ys);

        val (left, right) = split items;
        val (sortedLeft, sortedRight) = (mergesort cmp left, mergesort cmp right);
        val result = merge sortedLeft sortedRight;
    in result
    end;


fun intersperse _ [] = []
  | intersperse _ [x] = [x]
  | intersperse y (x::xs) = x::y::(intersperse y xs);


fun enumerateFrom _ [] = []
  | enumerateFrom i (x::xs) = (i, x)::(enumerateFrom (i+1) xs);

fun enumerate xs = enumerateFrom 0 xs;

end;
