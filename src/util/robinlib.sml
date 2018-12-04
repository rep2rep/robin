(*

robinlib.sml

Define some useful things for the whole robin system.

*)

signature ROBINLIB =
sig
    val import : string -> unit;
    val mergesort : ('a * 'a -> order) -> 'a list -> 'a list;
    val intersperse : 'a -> 'a list -> 'a list;
    val enumerate : 'a list -> (int * 'a) list;
    val enumerateFrom : int -> 'a list -> (int * 'a) list;
    val all : bool list -> bool;
    val any : bool list -> bool;
    val max : (('a * 'a) -> order) -> 'a list -> 'a;
    val min : (('a * 'a) -> order) -> 'a list -> 'a;
    val dropWhile : ('a -> bool) -> 'a list -> 'a list;
    val takeWhile : ('a -> bool) -> 'a list -> 'a list;
    val listToString : ('a -> string) -> 'a list -> string;
    val lookaheadN : (TextIO.instream *  int) -> string;
    val stringTrim : string -> string;
end;


structure RobinLib : ROBINLIB =
struct

val IMPORTED_ : string list ref = ref [];

fun import filename =
    let
        fun subDots str = String.implode
                                 (map (fn c => if c = #"." then #"/" else c)
                                      (String.explode str))
    in
        if (List.exists (fn s => s = filename) (!IMPORTED_))
        then () (* filename has already been imported *)
        else (
            IMPORTED_ := filename :: (!IMPORTED_);
            use (BASE^(subDots filename)^".sml")
        )
    end;


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

fun max _ [] = raise List.Empty
  | max cmp (x::xs) = List.foldl (fn (a, b) => if cmp(a, b) = GREATER then a else b) x xs

fun min _ [] = raise List.Empty
  | min cmp (x::xs) = List.foldl (fn (a, b) => if cmp(a, b) = LESS then a else b) x xs

fun dropWhile pred [] = []
  | dropWhile pred (x::xs) = if (pred x) then dropWhile pred xs
                             else x::(dropWhile pred xs);

fun takeWhile pred [] = []
  | takeWhile pred (x::xs) = if (pred x) then x::(takeWhile pred xs)
                             else (takeWhile pred xs);

fun listToString fmt items =
    let
        val stringItems = map fmt items;
        val withCommas = intersperse ", " stringItems;
        val joined = foldr (fn (x, y) => x ^ y) "" withCommas;
    in
        "[" ^ joined ^ "]"
    end;

fun lookaheadN (istr, count) =
    let
        val oldstream = TextIO.getInstream istr;
        val (lookahead, newstream) = TextIO.StreamIO.inputN(oldstream, count)
    in
        lookahead
    end;

fun stringTrim str =
    let
        val chars = String.explode str;
        val remainingChars = List.rev
                                 (dropWhile
                                      Char.isSpace
                                      (List.rev
                                           (dropWhile
                                                Char.isSpace
                                                chars)));
    in
        String.implode remainingChars
    end;

end;
