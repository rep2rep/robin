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
    val mappair : ('a -> 'b) -> ('a * 'a) -> ('b * 'b);
    val mapfst : ('a -> 'b) -> ('a * 'c) -> ('b * 'c);
    val mapsnd : ('b -> 'c) -> ('a * 'b) -> ('a * 'c);
    val flip : ('a * 'b) -> ('b * 'a);
    val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c;
    val uncurry: ('a -> 'b -> 'c) -> ('a * 'b) -> 'c;
    val fails : (unit -> 'a) -> bool;
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
    ) handle e => (IMPORTING_STACK_ := List.tl (!IMPORTING_STACK_); raise e);


fun mappair f (a, b) = (f a, f b);

fun mapfst f (a, b) = (f a, b);

fun mapsnd f (a, b) = (a, f b);

fun flip (a, b) = (b, a);

fun curry f a b = f (a, b);

fun uncurry f (a, b) = f a b;

fun fails f = (f(); false)
              handle _ => true;

end;

open RobinLib;



signature LIST =
sig
    include LIST;

    val remove : ''a -> ''a list -> ''a list;
    val removeDuplicates : ''a list -> ''a list;

    val mergesort : ('a * 'a -> order) -> 'a list -> 'a list;

    val intersperse : 'a -> 'a list -> 'a list;

    val enumerate : 'a list -> (int * 'a) list;
    val enumerateFrom : int -> 'a list -> (int * 'a) list;

    val filterOption : ('a option) list -> 'a list;

    val isPermutationOf : ('a * 'a -> bool) -> 'a list -> 'a list -> bool;

    val mapArgs : ('a -> 'b) -> 'a list -> ('a * 'b) list;
    val flatmap : ('a -> 'b list) -> 'a list -> 'b list;

    val product : ('a list * 'b list) -> ('a * 'b) list;

    val toString : ('a -> string) -> 'a list -> string;

    val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list;
    val replicate : int -> 'a -> 'a list;

    val max : (('a * 'a) -> order) -> 'a list -> 'a;
    val min : (('a * 'a) -> order) -> 'a list -> 'a;

    val takeWhile : ('a -> bool) -> 'a list -> 'a list;
    val dropWhile : ('a -> bool) -> 'a list -> 'a list;

    val split : ('a list * int) -> ('a list * 'a list);

    val rotate : int -> 'a list -> 'a list;

    val weightedSumIndexed : ('a -> real) -> ('a -> real) -> 'a list -> real;
    val sumIndexed : ('a -> real) -> 'a list -> real;
    val weightedSum : (real -> real) -> real list -> real;
    val sum : real list -> real;

    val weightedAvgIndexed : ('a -> real) -> ('a -> real) -> 'a list -> real;
    val avgIndexed : ('a -> real) -> 'a list -> real;
    val weightedAvg : (real -> real) -> real list -> real;
    val avg : real list -> real;

    val argmax : ('a -> real) -> 'a list -> ('a * real);
    val argmin : ('a -> real) -> 'a list -> ('a * real);
end;

structure List : LIST =
struct

open List;

fun remove needle haystack = List.filter (fn x => x <> needle) haystack;

fun removeDuplicates [] = []
  | removeDuplicates (h::t) = h :: removeDuplicates (remove h t);

fun split (xs, i) =
    let
        fun split' fst xs 0 = (List.rev fst, xs)
          | split' fst [] _ = raise Subscript
          | split' fst (x::xs) i = split' (x::fst) xs (i-1);
    in
        split' [] xs i
    end;

fun mergesort cmp [] = []
  | mergesort cmp [x] = [x]
  | mergesort cmp items =
    let fun merge [] xs = xs
          | merge xs [] = xs
          | merge (x::xs) (y::ys) =
            if cmp(x, y) = GREATER then
                y::(merge (x::xs) ys)
            else
                x::(merge xs (y::ys));

        val (left, right) = split (items, Int.div (length items, 2));
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

fun filterOption [] = []
  | filterOption (NONE :: L) = filterOption L
  | filterOption ((SOME x) :: L) = x :: filterOption L;

fun findAndRemove _ _ [] = (false,[])
  | findAndRemove f x (a::L) =
      if f (x, a) then (true,L)
      else let val (found,L') = findAndRemove f x L
           in (found,a::L')
           end;

fun isPermutationOf _ [] [] = true
  | isPermutationOf f (a::A) B = let val (found,B') = findAndRemove f a B
                                 in if found then isPermutationOf f A B'
                                    else false
                                 end
  | isPermutationOf _ _ _ = false;


fun mapArgs f xs = map (fn x => (x, f x)) xs;

fun flatmap f xs = concat (map f xs);

fun product (xs, ys) =
    let
        fun joinall ans x [] = ans
          | joinall ans x (y::ys) = joinall ((x, y)::ans) x ys
        fun cartprod ans [] _ = List.rev(ans)
          | cartprod ans _ [] = List.rev(ans)
          | cartprod ans (x::xs) ys = cartprod (joinall ans x ys) xs ys;
    in
        cartprod [] xs ys
    end;

fun toString fmt items = "[" ^ String.concatWith ", " (map fmt items) ^ "]";

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

fun weightedSumIndexed w f L =
    List.foldr (fn (x, s) => ((w x) * (f x)) + s) 0.0 L;

fun weightedSum w L = weightedSumIndexed w (fn x => x) L;

fun sumIndexed f L = weightedSumIndexed (fn _ => 1.0) f L;
fun sum L = weightedSumIndexed (fn _ => 1.0) (fn x => x) L;

fun weightedAvgIndexed w f L = if null L then raise Empty else (weightedSumIndexed w f L) / (sumIndexed w L)

fun weightedAvg w L = weightedAvgIndexed w (fn x => x) L;

fun avgIndexed f L = weightedAvgIndexed (fn _ => 1.0) f L;
fun avg L = weightedAvgIndexed (fn _ => 1.0) (fn x => x) L;

fun argmax _ [] = raise Match
  | argmax f [x] = (x, f x)
  | argmax f (x::L) = let val r = argmax f L
                          val v = f x
                      in if v > #2 r then (x,v) else r
                      end;

fun argmin _ [] = raise Match
  | argmin f [x] = (x, f x)
  | argmin f (x::L) = let val r = argmin f L
                          val v = f x
                      in if v < #2 r then (x,v) else r
                      end;

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
