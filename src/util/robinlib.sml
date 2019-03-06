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
        val joined = String.concat withCommas;
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
