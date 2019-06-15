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
    val remove : ''a -> ''a list -> ''a list;
    val mergesort : ('a * 'a -> order) -> 'a list -> 'a list;
    val intersperse : 'a -> 'a list -> 'a list;
    val enumerate : 'a list -> (int * 'a) list;
    val enumerateFrom : int -> 'a list -> (int * 'a) list;
    val flatmap : ('a -> 'b list) -> 'a list -> 'b list;
    val cartesianProduct : 'a list -> 'b list -> ('a * 'b) list;
    val all : bool list -> bool;
    val any : bool list -> bool;
    val max : (('a * 'a) -> order) -> 'a list -> 'a;
    val min : (('a * 'a) -> order) -> 'a list -> 'a;
    val dropWhile : ('a -> bool) -> 'a list -> 'a list;
    val takeWhile : ('a -> bool) -> 'a list -> 'a list;
    val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list;
    val replicate : int -> 'a -> 'a list;
    val listToString : ('a -> string) -> 'a list -> string;
    val lookaheadN : (TextIO.instream *  int) -> string;
    val spread : ('a -> 'b) -> ('a * 'a) -> ('b * 'b);
    val flip : ('a * 'b) -> ('b * 'a);
    val cmpJoin : ('a * 'a -> order) -> ('b * 'b -> order) -> (('a * 'b) * ('a * 'b)) -> order;
    val revCmp : ('a * 'a -> order) -> ('a * 'a) -> order;
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

fun remove needle haystack = List.filter (fn x => x = needle) haystack;

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

fun flatmap f xs = List.foldr (fn (y, ys) => (f y) @ ys) [] xs;

fun cartesianProduct xs ys =
    let
        fun joinall ans x [] = ans
          | joinall ans x (y::ys) = joinall ((x, y)::ans) x ys
        fun cartprod ans [] _ = List.rev(ans)
          | cartprod ans _ [] = List.rev(ans)
          | cartprod ans (x::xs) ys = cartprod (joinall ans x ys) xs ys;
    in
        cartprod [] xs ys
    end;

fun all [] = true
  | all (b::bs) = b andalso (all bs);

fun any [] = false
  | any (b::bs) = b orelse (any bs);

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

fun unfold f seed =
    let
        fun unfold' f seed ans =
            case f seed of
                NONE => ans
              | SOME (x, next) => (unfold' f next (x::ans));
    in
        List.rev (unfold' f seed [])
    end;

fun replicate n x =
    let fun gen 0 = NONE
          | gen n = SOME (x, n-1)
    in
        unfold gen n
    end;

fun listToString fmt items =
    let
        val stringItems = map fmt items;
        val withCommas = intersperse ", " stringItems;
        val joined = String.concat withCommas;
    in
        "[" ^ joined ^ "]"
    end;

fun lookaheadN (istr, count) =
    let
        val oldstream = TextIO.getInstream istr;
        val (lookahead, newstream) = TextIO.StreamIO.inputN(oldstream, count);
        val _ = TextIO.setInstream (istr, oldstream);
    in
        lookahead
    end;

fun spread f (a, b) = (f a, f b);

fun flip (a, b) = (b, a);

fun cmpJoin c1 c2 = fn ((a, x), (b, y)) =>
                       case (c1 (a, b)) of
                           EQUAL => c2 (x, y)
                         | cmp => cmp;

fun revCmp c = fn p => case (c p) of
                           LESS => GREATER
                         | EQUAL => EQUAL
                         | GREATER => LESS;
end;

open RobinLib;
