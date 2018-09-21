use "base.sml";
use (BASE^"strategies/representation_selection.sml");


fun readQuestion fileName = ("q", "One");

fun main () =
    let
        val args = CommandLine.arguments ();
        val question = readQuestion (List.hd args); (* The first argument is the problem filename *)
        val bestRepresentations = topKRepresentations question 4;
    in (
        map (fn (r, s) => print (r ^ " (score " ^ (Real.toString s) ^ ")" ^ "\n")) bestRepresentations;
        0
    ) end;
