signature STREAM = sig

    type 'a stream;

    val empty : 'a stream;
    val cons : 'a * 'a stream -> 'a stream;
    val null : 'a stream -> bool;

    val head : 'a stream -> 'a;
    val lazyHead : 'a stream -> (unit -> 'a);
    val tail : 'a stream -> 'a stream;
    val step : 'a stream -> ('a * 'a stream);
    val lazyStep : 'a stream -> ((unit -> 'a) * 'a stream);

    val fromList : 'a list -> 'a stream;
    val toList : 'a stream -> 'a list; (* Finite only! *)

    val map : ('a -> 'b) -> 'a stream -> 'b stream;
    val mapPartial : ('a -> 'b option) -> 'a stream -> 'b stream;
    val flatmap : ('a -> 'b stream) -> 'a stream -> 'b stream;
    val filter : ('a -> bool) -> 'a stream -> 'a stream;
    val fold : ('b * 'a -> 'b) -> 'b -> 'a stream -> 'b; (* Finite only! *)

    val take : int -> 'a stream -> 'a stream;
    val takeList : int -> 'a stream -> 'a list;
    val drop : int -> 'a stream -> 'a stream;
    val takeWhile : ('a -> bool) -> 'a stream -> 'a stream;
    val takeListWhile : ('a -> bool) -> 'a stream -> 'a list;
    val dropWhile : ('a -> bool) -> 'a stream -> 'a stream;

    val all : ('a -> bool) -> 'a stream -> bool;
    val exists: ('a -> bool) -> 'a stream -> bool; (* Finite only! *)

    val interleave : 'a stream -> 'a stream -> 'a stream;
    val interleave' : 'a stream -> (unit -> 'a stream) -> 'a stream;
    val interleaveAll : 'a stream stream -> 'a stream;

    val zip : 'a stream -> 'b stream -> ('a * 'b) stream;
    val product : 'a stream -> 'b stream -> ('a * 'b) stream;

    val length : 'a stream -> int; (* Finite only! *)

    val nats : int stream;
    val repeat : 'a -> 'a stream;
    val unfold : ('a -> 'a option) -> 'a -> 'a stream;

end;

structure Stream : STREAM = struct

datatype 'a stream = EMPTY
                   | CONS of (unit -> 'a) * (unit -> 'a stream);

fun force x = (x());

val empty = EMPTY;

fun null EMPTY = true
  | null _ = false;

fun cons (x, xs) = CONS(fn () => x, fn () => xs);

fun head EMPTY = raise Subscript
  | head (CONS(x, xf)) = force x;

fun tail EMPTY = raise Subscript
  | tail (CONS(x, xf)) = force xf;

fun step EMPTY = raise Subscript
  | step (CONS(x, xf)) = (force x, force xf);

fun lazyHead EMPTY = raise Subscript
  | lazyHead (CONS(x, xf)) = x;

fun lazyStep EMPTY = raise Subscript
  | lazyStep (CONS(x, xf)) = (x, force xf);

fun interleave' EMPTY y = y()
  | interleave' (CONS(x, xf)) y = CONS(x, fn () => interleave' (force y) xf);

fun interleave a b = interleave' a (fn () => b);

fun interleaveAll EMPTY = EMPTY
  | interleaveAll (CONS(s,ss)) = interleave' (force s) (fn () => interleaveAll (force ss));

fun fromList [] = EMPTY
  | fromList (x::xs) = CONS(fn () => x, fn () => fromList xs);

fun toList s =
    let
        fun toList' EMPTY ans = List.rev ans
          | toList' (CONS(x, xf)) ans = toList' (force xf) ((force x)::ans);
    in
        toList' s []
    end;

fun map f EMPTY = EMPTY
  | map f (CONS(x, xf)) = CONS(fn () => f (force x), fn () => map f (force xf));

fun mapPartial f EMPTY = EMPTY
  | mapPartial f (CONS(x, xf)) =
    case f (force x) of
        SOME v => CONS(fn () => v, fn () => mapPartial f (force xf))
      | NONE => mapPartial f (force xf);

fun flatmap f xs = interleaveAll (map f xs);

fun filter f EMPTY = EMPTY
  | filter f (CONS(x, xf)) = let val v = force x
                             in if f v
                                then CONS(fn () => v, fn () => filter f (force xf))
                                else filter f (force xf) end;

fun fold f a EMPTY = a
  | fold f a (CONS(x, xf)) = fold f (f (a, (force x))) (force xf);

fun take 0 _ = EMPTY
  | take _ EMPTY = EMPTY
  | take n (CONS(x, xf)) = CONS(x, fn () => take (n-1) (force xf));

fun takeList i xs = toList (take i xs);

fun drop 0 x = x
  | drop _ EMPTY = EMPTY
  | drop n (CONS(x, xf)) = drop (n-1) (force xf);

fun takeWhile f EMPTY = EMPTY
  | takeWhile f (CONS(x, xf)) = let val v = force x
                                in if f v
                                   then CONS( fn () => v, fn () => takeWhile f (force xf))
                                   else EMPTY end;

fun takeListWhile f xs = toList (takeWhile f xs);

fun dropWhile f EMPTY = EMPTY
  | dropWhile f (CONS(x, xf)) = let val v = force x
                                in if f v
                                   then dropWhile f (force xf)
                                   else CONS(fn () => v, xf) end;

fun all f EMPTY = true
  | all f (CONS(x, xf)) = f (force x) andalso all f (force xf);

fun exists f EMPTY = false
  | exists f (CONS(x, xf)) = f (force x) orelse exists f (force xf);

fun zip _ EMPTY = EMPTY
  | zip EMPTY _ = EMPTY
  | zip (CONS(x, xf)) (CONS(y, yf)) = CONS(fn () => ((force x), (force y)), fn () => zip (force xf) (force yf));

fun length EMPTY = 0
  | length (CONS(x, xf)) = 1 + length (force xf);

fun unfold f s = let fun uf x = case x of
                                    SOME v => CONS(fn () => v, fn () => uf (f v))
                                  | NONE => EMPTY;
                 in uf (SOME s) end;

fun repeat x = unfold (fn _ => SOME x) x;

val nats = unfold (fn x => SOME (x + 1)) 0;

fun product _ EMPTY = EMPTY
  | product EMPTY _ = EMPTY
  | product (CONS(x, xf)) yf = let fun lazyRepeat x = CONS(x, fn () => lazyRepeat x);
                               in interleave' (zip (lazyRepeat x) yf) (fn () => product (force xf) yf) end;

end;
