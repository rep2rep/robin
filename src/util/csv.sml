(*

csv.sml

This CSV structure provides utilities for reading and writing CSV files.
In particular, it is configurable to parse different 'flavours' of CSV:
you can set the delimiter and the newline character, with potentially
more customisation to be added in future.

*)

signature CSVConfig =
sig
    val delimiters : char list;
    val newlines : string list;
end;


signature CSVIO =
sig
    type instream;
    type outstream;

    val newlines : char list list;
    val delimiters : char list;
    val lookaheadDistance : int;

    val openIn : string -> instream;
    val closeIn : instream -> unit;
    val inputCell : instream -> string;
    val inputRow : instream -> string list;
    val input : instream -> string list list;
    val skipDelimiter : instream -> bool;
    val endOfCell : instream -> bool;
    val endOfRow : instream -> bool;
    val endOfStream : instream -> bool;

    val openOut : string -> outstream;
    val closeOut : outstream -> unit;
    val outputCell : outstream -> string -> bool -> unit;
    val outputRow : outstream -> string list -> unit;
    val output : outstream -> string list list -> unit;
    val flushOut : outstream -> unit;
end;


functor CSVIO (Config : CSVConfig) : CSVIO =
struct

type instream = TextIO.instream;
type outstream = TextIO.outstream;

val delimiters = Config.delimiters;
val newlines = mergesort
                   (fn (a, b) => let
                        val la = List.length a;
                        val lb = List.length b;
                    in
                        if la < lb then GREATER  (* We want it reversed *)
                        else if la > lb then LESS
                        else EQUAL
                   end)
                   (map String.explode Config.newlines);
val lookaheadDistance = List.foldr Int.max 0 (map List.length newlines);

fun isDelimiter c = List.exists (fn x => x = c) delimiters;
fun matchNewline cs = List.find
                          (fn ns => (List.take (cs, (List.length ns))) = ns
                                    handle Subscript => false)
                          newlines;
fun isNewline cs = case (matchNewline cs) of NONE => false
                                           | SOME _ => true;

fun openIn filename = TextIO.openIn filename;
fun closeIn istr = TextIO.closeIn istr;

fun endOfStream istr = TextIO.endOfStream istr;
fun endOfRow istr =
    case (lookaheadN (istr, lookaheadDistance)) of
        "" => true
      | c => isNewline (String.explode c);
fun endOfCell istr =
    (endOfRow istr) orelse (case (TextIO.lookahead istr) of
                                NONE => true
                              | SOME c => isDelimiter c);

fun skipDelimiter istr =
    case (TextIO.lookahead istr) of
        NONE => false
      | SOME c => if (isDelimiter c) then (TextIO.input1 istr; true) else false;


fun inputCell istr = let
    fun escapedQuote istr = let
        val next2 = String.explode (lookaheadN (istr, 2));
        val (current, next) = case next2 of
                                  [] => (#"0", #"0")
                                | [x] => (x, #"0")
                                | [x, y] => (x, y)
                                | (x::y::z) => (x, y);
    in
        if (current = #"\"" andalso next = #"\"") then true
        else false
    end;
    fun readCell quoted result =
        if (endOfCell istr andalso not quoted) then result
        else case (TextIO.lookahead istr) of
                 NONE => result
               | SOME c => if (isDelimiter c) then
                               if quoted then (TextIO.input1 istr; readCell quoted ((Char.toString c) :: result))
                               else result
                           else if (c = #"\"" andalso not quoted) then (* Open quoted cell *)
                               (TextIO.input1 istr; readCell true result)
                           else if (c = #"\"" andalso quoted) then (* Close quoted cell, or escaped *)
                               if (escapedQuote istr) then
                                   (TextIO.inputN (istr, 2); readCell true ("\"" :: result)) (* Consume the quotes and carry on *)
                               else (TextIO.input1 istr;
                                     result) (* End of cell *)
                           else readCell quoted (TextIO.inputN (istr, 1) :: result);
in
    String.concat (List.rev (readCell false []))
end;

fun inputRow istr = let
    fun dowhile' cond eval result =
        if (cond ()) then (dowhile' cond eval ((eval ())::result))
        else result;
    fun dowhile cond eval = List.rev (dowhile' cond eval []);
    val startsEmpty = endOfRow istr;
    val result = (if not (endOfRow istr) then inputCell istr else "")::
                 (dowhile (fn () => not (endOfRow istr))
                          (fn () => (skipDelimiter istr; inputCell istr)));
in
    if startsEmpty then let
        val newlineChars = String.explode (lookaheadN (istr, lookaheadDistance));
        val matchedNewline = matchNewline newlineChars;
        val consumeDistance = case matchedNewline of
                                  NONE => 0
                               | SOME nl => List.length nl;
    in
        (TextIO.inputN (istr, consumeDistance); [])
    end
    else let
        val newlineChars = String.explode (lookaheadN (istr, lookaheadDistance));
        val matchedNewline = matchNewline newlineChars;
        val consumeDistance = case matchedNewline of
                                  NONE => 0
                               | SOME nl => List.length nl;
    in
        (TextIO.inputN (istr, consumeDistance); result) (* Consume the newline *)
    end
end;

fun input istr = let
    fun dowhile' cond eval result =
        if (cond ()) then (dowhile' cond eval ((eval ())::result))
        else result;
    fun dowhile cond eval = List.rev (dowhile' cond eval []);
    val startsEmpty = endOfStream istr;
in
    if startsEmpty then []
    else dowhile (fn () => not (endOfStream istr))
                 (fn () => inputRow istr)
end;


fun openOut filename = TextIO.openOut filename;
fun closeOut ostr = TextIO.closeOut ostr;
fun flushOut ostr = TextIO.flushOut ostr;

(* TODO: Implement output *)
fun outputCell ostr value isEndOfLine = ();
fun outputRow ostr values = ();
fun output ostr values = ();

end;

structure CSVDefault = CSVIO(struct val delimiters = [#","];
                                    val newlines = ["\r\n"];
                             end);
