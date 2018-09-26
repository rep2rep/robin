(*

csv.sml

This CSV structure provides utilities for reading and writing CSV files.
In particular, it is configurable to parse different 'flavours' of CSV:
you can set the delimiter and the newline character, with potentially
more customisation to be added in future.

*)


signature CSVConfig =
sig
    val delimiter : char;
    val newline : string;
end;


functor CSVIO (Config : CSVConfig) =
struct

type instream = TextIO.instream;
type outstream = TextIO.outstream;

val delimiter = Config.delimiter;
val newline = Config.newline;

fun openIn filename = TextIO.openIn filename;
fun closeIn istr = TextIO.closeIn istr;

fun endOfStream istr = TextIO.endOfStream istr;
fun endOfRow istr = false; (* TODO: Implement this *)

(* TODO: Implement input *)
fun inputCell istr = "";
fun inputRow istr = [""];

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
