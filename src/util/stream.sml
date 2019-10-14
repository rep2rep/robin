import "util.random";
signature STREAM = sig

    type 'a stream;

    val empty : 'a stream;
    val cons : 'a * 'a stream -> 'a stream;
    val null : 'a stream -> bool;

    val hd : 'a stream -> 'a;
    val lazyHd : 'a stream -> (unit -> 'a);
    val tl : 'a stream -> 'a stream;
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
    val interleaveAll : 'a stream stream -> 'a stream;

    val zip : 'a stream -> 'b stream -> ('a * 'b) stream;
    val product : 'a stream -> 'b stream -> ('a * 'b) stream;

    val length : 'a stream -> int; (* Finite only! *)

    val nats : int stream;
    val repeat : 'a -> 'a stream;
    val unfold : ('a -> 'a option) -> 'a -> 'a stream;

end;

structure Stream : STREAM = struct

datatype 'a stream = STREAM of unit -> 'a streamHelper
     and 'a streamHelper = EMPTY | CONS of (unit -> 'a) * 'a stream;

fun force (STREAM s) = (s());

val empty = STREAM (fn () => EMPTY);

fun null s = case force s of EMPTY => true
                           | _ => false;

fun cons (x, s) = STREAM (fn () => CONS (fn () => x, s));

fun hd s = case force s of EMPTY => raise Subscript
                         | CONS(x, xf) => x();

fun tl s = case force s of EMPTY => raise Subscript
                         | CONS(x, xf) => xf;

fun step s = case force s of EMPTY => raise Subscript
                           | CONS (x, xf) => (x(), xf);

fun lazyHd s = (print"GettingHeadLazy\n";case force s of EMPTY => raise Subscript
                             | CONS(x, xf) => x);

fun lazyStep s = (print"GettingStepLazy\n";case force s of EMPTY => raise Subscript
                               | CONS(x, xf) => (x, xf));

fun interleave s t = STREAM (fn () => case force s of EMPTY => force t
                                                    | CONS(x, xf) => CONS (x, interleave t xf));

fun interleaveAll s = STREAM (fn () => case force s of EMPTY => EMPTY
                                                     | CONS(x, xf) => force (interleave (x()) (interleaveAll xf)));

fun fromList [] = STREAM (fn () => EMPTY)
  | fromList (x::xs) = STREAM (fn () => CONS(fn () => x, fromList xs));

fun toList (STREAM s) =
    let
        fun toList' s ans = case force s of EMPTY => List.rev ans
                                          | CONS(x, xf) => toList' xf ((x())::ans)
    in
        toList' (STREAM s) []
    end;

fun map f s = STREAM (fn () => case force s of EMPTY => EMPTY
                                             | CONS(x, xf) => CONS(fn () => f (x()), map f xf));

fun mapPartial f s = STREAM (fn () => case force s of EMPTY => EMPTY
                                                    | CONS(x, xf) => let val v = x()
                                                                     in case f v of SOME y => CONS(fn () => y, mapPartial f xf)
                                                                                  | NONE => force (mapPartial f xf) end);

fun flatmap f s = interleaveAll (map f s);

fun filter f s = STREAM (fn () => case force s of EMPTY => EMPTY
                                                | CONS(x, xf) => let val v = x()
                                                                 in case f v of true => CONS(fn () => v, filter f xf)
                                                                              | false => force (filter f xf) end);

fun fold f a s = case force s of EMPTY => a
                               | CONS(x, xf) => fold f (f(a, x())) xf;

fun take 0 _ = STREAM (fn () => EMPTY)
  | take n s = STREAM (fn () => case force s of EMPTY => EMPTY
                                              | CONS(x, xf) => CONS(x, take (n-1) xf));

fun takeList i s = toList (take i s);

fun drop 0 x = x
  | drop i s = STREAM (fn () => case force s of EMPTY => EMPTY
                                              | CONS(x, xf) => force (drop (i-1) xf));

fun takeWhile f s = STREAM (fn () => case force s of EMPTY => EMPTY
                                                  | CONS(x, xf) => let val v = x()
                                                                   in if f v then CONS(fn () => v, takeWhile f xf)
                                                                      else EMPTY end);

fun takeListWhile f s = toList (takeWhile f s);

fun dropWhile f s = STREAM (fn () => case force s of EMPTY => EMPTY
                                                   | CONS(x, xf) => let val v = x()
                                                                    in if f v then force (dropWhile f xf)
                                                                       else CONS(fn () => v, xf) end);
fun all f s = case force s of EMPTY => true
                            | CONS(x, xf) => f (x ()) andalso all f xf;

fun exists f s = case force s of EMPTY => false
                               | CONS(x, xf) => f (x ()) orelse exists f xf;

fun zip s t = STREAM (fn () => case (force s, force t) of
                                   (EMPTY, _) => EMPTY
                                 | (_, EMPTY) => EMPTY
                                 | (CONS(x, xf), CONS(y, yf)) => CONS(fn () => (x(),y()), zip xf yf));

fun length s =
    let
        fun len' t ans = case force t of EMPTY => ans
                                       | CONS(x, xf) => len' xf (1 + ans);
    in
        len' s 0
    end;

fun unfold f z = let fun uf x = STREAM (fn () => case x of SOME v => CONS(fn () => v, uf (f v))
                                                         | NONE => EMPTY)
                 in uf (SOME z) end;

fun repeat k = unfold (fn _ => SOME k) k;

val nats = unfold (fn x => SOME (x + 1)) 0;

fun product s t = STREAM(fn () => case (force s, force t) of (_, EMPTY) => EMPTY
                                                           | (EMPTY, _) => EMPTY
                                                           | (CONS(x, xf), _) =>
                                                             let
                                                                 fun repeatl x = STREAM(fn () => CONS(x, repeatl x));
                                                             in
                                                                 force (interleave (zip (repeatl x) t) (product xf t))
                                                             end);

end;
