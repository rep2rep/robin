(*

csv.sml

This CSV structure provides utilities for reading and writing CSV files.
In particular, it is configurable to parse different 'flavours' of CSV:
you can set the delimiter and the newline character, with potentially
more customisation to be added in future.

*)

use "base.sml";
use (BASE^"util/robinlib.sml");

signature CSVConfig =
sig
    val delimiter : char;
    val newline : string;
end;


signature CSVIO =
sig
    type instream;
    type outstream;

    val newline : char list;
    val delimiter : char;
    val lookaheadDistance : int;

    val openIn : string -> instream;
    val closeIn : instream -> unit;
    val inputCell : instream -> string;
    val inputRow : instream -> string list;
    val skipDelimiter : instream -> bool;
    val endOfCell : instream -> bool;
    val endOfRow : instream -> bool;
    val endOfStream : instream -> bool;

    val openOut : string -> outstream;
    val closeOut : outstream -> unit;
    val outputCell : 'a -> 'b -> 'c -> unit;
    val outputRow : 'a -> 'b -> unit;
    val flushOut : outstream -> unit;
end;


functor CSVIO (Config : CSVConfig) : CSVIO =
struct

type instream = TextIO.instream;
type outstream = TextIO.outstream;

val delimiter = Config.delimiter;
val newline = String.explode Config.newline;
val lookaheadDistance = List.length newline;

fun openIn filename = TextIO.openIn filename;
fun closeIn istr = TextIO.closeIn istr;

fun endOfStream istr = TextIO.endOfStream istr;
fun endOfRow istr =
    case (RobinLib.lookaheadN (istr, lookaheadDistance)) of
        "" => true
      | c => String.explode c = newline;
fun endOfCell istr =
    (endOfRow istr) orelse (case (TextIO.lookahead istr) of
                                NONE => true
                              | SOME c => c = delimiter);

fun skipDelimiter istr =
    case (TextIO.lookahead istr) of
        NONE => false
      | SOME c => if c = delimiter then (TextIO.input1 istr; true) else false;


fun inputCell istr = let
    fun escapedQuote istr = let
        val next2 = String.explode (RobinLib.lookaheadN (istr, 2));
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
               | SOME c => if c = delimiter then
                               if quoted then (TextIO.input1 istr; readCell quoted (result ^ (Char.toString c)))
                               else result
                           else if (c = #"\"" andalso not quoted) then (* Open quoted cell *)
                               (TextIO.input1 istr; readCell true result)
                           else if (c = #"\"" andalso quoted) then (* Close quoted cell, or escaped *)
                               if (escapedQuote istr) then
                                   (TextIO.inputN (istr, 2); readCell true (result ^ "\"")) (* Consume the quotes and carry on *)
                               else (TextIO.input1 istr;
                                     result) (* End of cell *)
                           else readCell quoted (result ^ TextIO.inputN (istr, 1));
in
    readCell false ""
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
    if startsEmpty then []
    else (TextIO.inputN (istr, lookaheadDistance); result) (* Consume the newline *)
end;


fun openOut filename = TextIO.openOut filename;
fun closeOut ostr = TextIO.closeOut ostr;
fun flushOut ostr = TextIO.flushOut ostr;

(* TODO: Implement output *)
fun outputCell ostr value isEndOfLine = ();
fun outputRow ostr values = ();

end;

structure CSVDefault = CSVIO(struct val delimiter = #",";
                                    val newline = "\r\n";
                             end);
