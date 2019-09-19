signature STREAM = sig

    type 'a stream;

    val empty : 'a stream;
    val cons : 'a * 'a stream -> 'a stream;

    val head : 'a stream -> 'a;
    val tail : 'a stream -> 'a stream;

    val fromList : 'a list -> 'a stream;
    val toList : 'a stream -> 'a list; (* Finite only! *)

    val map : ('a -> 'b) -> 'a stream -> 'b stream;
    val flatmap : ('a -> 'b stream) -> 'a stream -> 'b stream;
    val filter : ('a -> bool) -> 'a stream -> 'a stream;
    val fold : ('b * 'a -> 'b) -> 'b -> 'a stream -> 'b; (* Finite only! *)

    val take : int -> 'a stream -> 'a stream;
    val drop : int -> 'a stream -> 'a stream;
    val takeWhile : ('a -> bool) -> 'a stream -> 'a stream;
    val dropWhile : ('a -> bool) -> 'a stream -> 'a stream;

    val exists: 'a -> 'a stream -> bool; (* Finite only! *)

    val interleave : 'a stream -> 'a stream -> 'a stream;

    val length : 'a stream -> int; (* Finite only! *)

    val nats : 'a stream;
    val unfold : ('a -> 'b option) -> 'a -> 'b stream;

end;

structure Stream = struct

datatype 'a stream = EMPTY
                   | CONS of 'a * (unit -> 'a stream);

fun force x = (x());

val empty = EMPTY;

fun cons (x, xs) = CONS(x, fn () => xs);

fun head EMPTY = raise Subscript
  | head (CONS(x, xf)) = x;

fun tail EMPTY = raise Subscript
  | tail (CONS(x, xf)) = force xf;

fun interleave EMPTY y = y
  | interleave x EMPTY = x
  | interleave (CONS(x, xf)) y = CONS(x, fn () => interleave y (force xf));

fun fromList [] = EMPTY
  | fromList (x::xs) = CONS(x, fn () => fromList xs);

fun toList s =
    let
        fun toList' EMPTY ans = List.rev ans
          | toList' (CONS(x, xf)) ans = toList' (force xf) (x::ans);
    in
        toList' s []
    end;

fun map f EMPTY = EMPTY
  | map f (CONS(x, xf)) = CONS(f x, fn () => map f (force xf));

fun flatmap f EMPTY = EMPTY
  | flatmap f (CONS(x, xf)) = interleave (f x) (flatmap f (force xf));

fun filter f EMPTY = EMPTY
  | filter f (CONS(x, xf)) = if f x
                             then CONS(x, fn () => filter f (force xf))
                             else filter f (force xf);

fun fold f a EMPTY = a
  | fold f a (CONS(x, xf)) = fold f (f a x) (force xf);

fun take 0 _ = EMPTY
  | take _ EMPTY = raise Subscript
  | take n (CONS(x, xf)) = CONS(x, fn () => take (n-1) (force xf));

fun drop 0 x = x
  | drop _ EMPTY = raise Subscript
  | drop n (CONS(x, xf)) = drop (n-1) (force xf);

fun takeWhile f EMPTY = EMPTY
  | takeWhile f (CONS(x, xf)) = if f x
                                then CONS(x, fn () => takeWhile f (force xf))
                                else EMPTY;

fun dropWhile f EMPTY = EMPTY
  | dropWhile f (CONS(x, xf)) = if f x
                                then dropWhile f (force xf)
                                else CONS(x, xf);

fun exists f EMPTY = false
  | exists f (CONS(x, xf)) = f x orelse exists f (force xf);

fun length EMPTY = 0
  | length (CONS(x, xf)) = 1 + length (force xf);

fun unfold f s = case f s of
                     SOME x => CONS(s, fn () => unfold f x)
                   | NONE => CONS(s, fn () => EMPTY);

val nats = unfold (fn x => SOME (x + 1)) 0;

end;
