(*

robinlib.sml

Define some useful things for the whole robin system.
Names that contain trailing double underscores are provided only for convenience,
and are in no way guaranteed to be available or consistent.

*)

signature ROBINLIB =
sig
    val import : string -> unit;
    val imported__ : unit -> string list;
    val imported__asFilenames__ : unit -> string list;
    val spread : ('a -> 'b) -> ('a * 'a) -> ('b * 'b);
    val flip : ('a * 'b) -> ('b * 'a);
    val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c;
    val uncurry: ('a -> 'b -> 'c) -> ('a * 'b) -> 'c;
end;


structure RobinLib : ROBINLIB =
struct

val IMPORTING_STACK_ : string list ref = ref [];
val IMPORTED_ : string list ref = ref ["util.robinlib"];

fun makeFilename str =
    let
        fun subDots s = String.implode
                            (map (fn c => if c = #"." then #"/" else c)
                                 (String.explode s));
    in
        BASE ^ (subDots str) ^ ".sml"
    end;


fun imported__ () = List.rev (!IMPORTED_);
fun imported__asFilenames__ () =
    map makeFilename (imported__ ());

fun import filename =
    if (List.exists (fn s => s = filename) ((!IMPORTING_STACK_) @ (!IMPORTED_)))
    then ()(* filename has already been imported *)
    else (
        IMPORTING_STACK_ := filename :: (!IMPORTING_STACK_);
        use (makeFilename filename);
        IMPORTED_ := (List.hd (!IMPORTING_STACK_))::(!IMPORTED_);
        IMPORTING_STACK_ := List.tl (!IMPORTING_STACK_)
    ) handle IO.Io e => (IMPORTING_STACK_ := List.tl (!IMPORTING_STACK_); raise IO.Io e);


fun spread f (a, b) = (f a, f b);

fun flip (a, b) = (b, a);

fun curry f a b = f (a, b);

fun uncurry f (a, b) = f a b;

end;




signature LIST =
sig
    include LIST;

    val remove : ''a -> ''a list -> ''a list;

    val mergesort : ('a * 'a -> order) -> 'a list -> 'a list;

    val intersperse : 'a -> 'a list -> 'a list;

    val enumerate : 'a list -> (int * 'a) list;
    val enumerateFrom : int -> 'a list -> (int * 'a) list;

    val flatmap : ('a -> 'b list) -> 'a list -> 'b list;

    val toString : ('a -> string) -> 'a list -> string;

    val max : (('a * 'a) -> order) -> 'a list -> 'a;
    val min : (('a * 'a) -> order) -> 'a list -> 'a;

    val takeWhile : ('a -> bool) -> 'a list -> 'a list;
    val dropWhile : ('a -> bool) -> 'a list -> 'a list;

    val rotate : int -> 'a list -> 'a list;

    val product : 'a list -> 'b list -> ('a * 'b) list;

end;

structure List : LIST =
struct

open List;

fun remove needle haystack = List.filter (fn x => x <> needle) haystack;

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

fun intersperse s [] = []
  | intersperse s (y::ys) =
    let fun intersperse' [] ans = List.rev ans
          | intersperse' (x::xs) ans = intersperse' xs (x::s::ans)
    in
        intersperse' ys [y]
    end;

fun enumerateFrom start list =
    let
        fun enumerateFrom' _ [] ans = List.rev ans
          | enumerateFrom' i (x::xs) ans = enumerateFrom' (i+1) xs ((i, x)::ans)
    in
        enumerateFrom' start list []
    end;

fun enumerate xs = enumerateFrom 0 xs;

fun flatmap f xs = concat (map f xs);

fun toString fmt items = "[" ^ String.concatWith ", " (map fmt items) ^ "]";

fun max _ [] = raise List.Empty
  | max cmp (x::xs) = List.foldl (fn (a, b) => if cmp(a, b) = GREATER
                                               then a
                                               else b)
                                 x xs;

fun min _ [] = raise List.Empty
  | min cmp (x::xs) = List.foldl (fn (a, b) => if cmp(a, b) = LESS
                                               then a
                                               else b)
                                 x xs;

fun dropWhile pred [] = []
  | dropWhile pred (x::xs) = if pred x then (dropWhile pred xs)
                             else x::xs;

fun takeWhile pred list =
    let fun takeWhile' [] ans = List.rev ans
          | takeWhile' (x::xs) ans = if pred x then takeWhile' xs (x::ans)
                                      else List.rev ans;
    in takeWhile' list []
    end;

fun rotate 0 xs = xs
  | rotate n xs =
    let
        val a = take (xs, n);
        val b = drop (xs, n);
    in
        b @ a
    end;

fun product [] _ = []
  | product _ [] = []
  | product [x] (y::ys) = (x, y)::(product [x] ys)
  | product (x::xs) (ys) = (product [x] ys) @ (product xs ys);

end;




signature COMPARISON =
sig

    val join : ('a * 'a -> order) -> ('b * 'b -> order) -> (('a * 'b) * ('a * 'b)) -> order;
    val rev : ('a * 'a -> order) -> ('a * 'a) -> order;

end;


structure Comparison : COMPARISON =
struct

fun join c1 c2 = fn ((a, x), (b, y)) =>
                    case (c1 (a, b)) of
                        EQUAL => c2 (x, y)
                      | cmp => cmp;

fun rev c = fn p => case (c p) of
                        LESS => GREATER
                      | EQUAL => EQUAL
                      | GREATER => LESS;

end;




signature TEXT_IO =
sig

    include TEXT_IO;

    val lookaheadN : (instream *  int) -> string;

end;

structure TextIO :> TEXT_IO =
struct

open TextIO;

fun lookaheadN (istr, count) =
    let
        val oldstream = getInstream istr;
        val (lookahead, newstream) = StreamIO.inputN(oldstream, count);
        val _ = setInstream (istr, oldstream);
    in
        lookahead
    end;

end;




signature OPTION =
sig

    include OPTION;

    val oneOf : ('a -> 'b option) list -> 'a -> 'b option;

end;

structure Option : OPTION =
struct

open Option;

fun oneOf [] _ = NONE
  | oneOf (f::fs) x = case f x of
                          SOME y => SOME y
                        | NONE => oneOf fs x;

end;
