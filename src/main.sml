use "base.sml";
use (BASE^"strategies/representation_selection.sml");

structure RepSelect = RepresentationSelection;

fun readQuestion fileName = ("medical", "bayes");

fun main () =
    let
        val today = Date.fmt "%Y-%m-%d" (Date.fromTimeLocal (Time.now()));
        (* The first argument is the problem filename, second is number of reps to try *)
        (* val args = CommandLine.arguments (); *)
        (* val question = readQuestion (List.hd args); *)
        (* val SOME numAlternatives = Int.fromString (List.hd (List.tl args)); *)
        val question = readQuestion "fake";
        val numAlternatives = 4;
        val (qName, qRep) = question;
        val _ = print ("BEGIN algorithm-trace-" ^ today ^ "\n");
        val _ = RepSelect.init(
                [BASE^"strategies/tables/RS_table_bayes.csv"],
                [BASE^"strategies/tables/correspondence_table.csv"],
                [BASE^"strategies/tables/Q_table_" ^ (qName) ^ "_" ^ (qRep) ^ ".csv"]);
        val bestRepresentations = RepSelect.topKRepresentations question numAlternatives;
    in
        print ("RECOMMEND: " ^
               (if (List.null bestRepresentations)
                then "NONE"
                else (#1 (List.hd bestRepresentations))) ^
               "\n");
        print ("\nEND algorithm-trace-" ^ today ^ "\n");
        0
     end;
