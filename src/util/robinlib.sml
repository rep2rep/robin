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
    val all : bool list -> bool;
    val any : bool list -> bool;
    val lookaheadN : (TextIO.instream *  int) -> string;
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

fun all [] = true
  | all (b::bs) = b andalso (all bs);

fun any [] = false
  | any (b::bs) = b orelse (any bs);

fun lookaheadN (istr, count) =
    let
        val oldstream = TextIO.getInstream istr;
        val (lookahead, newstream) = TextIO.StreamIO.inputN(oldstream, count)
    in
        lookahead
    end;

end;
