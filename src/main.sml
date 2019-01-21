import "util.logging";
import "strategies.representation_selection";

(* To see a full trace of the algorithm, we enable logging.
   If this seems too 'noisy', you can use `Logging.disable ()`.
   (Alternatively, because disabled is the default logging state,
   you can just comment out the following line.)
*)
Logging.enable ();

exception ArgumentError of int;

structure RepSelect = RepresentationSelection;

(* For now, we always solve the "medical" problem that starts in
   a specified representation (read from command line).
   This will obviously need to be input by a user in the future.
*)
fun readQuestion fileName = ("medical", fileName);

(* The first argument is the problem filename, second is number of reps to try *)
fun parseArgs () =
    let
        val args = CommandLine.arguments ();
        val defaultAlts = 1;
        val noNumAltError = "WARNING: " ^
                            "No specified number of representations to offer, " ^
                            "using " ^
                            (Int.toString defaultAlts) ^ "\n";
    in
        case args of
            [] => (print "ERROR: No arguments given, requires 1 or 2."; raise ArgumentError 0)
          | [fname] => (print noNumAltError; (fname, defaultAlts))
          | (fname::altNumString::rest) =>
            case (Int.fromString altNumString) of
                SOME k => (fname, k)
              | NONE => (print noNumAltError; (fname, defaultAlts))
    end;

fun main () =
    let
        val today = Date.fmt "%Y-%m-%d" (Date.fromTimeLocal (Time.now()));
        val (qFileName, numAlternatives) = parseArgs ();
        val question = readQuestion qFileName;
        val (qName, qRep) = question;
        val _ = Logging.write ("BEGIN algorithm-trace-" ^ today ^ "\n");
        val _ = RepSelect.init(
                [BASE^"strategies/tables/RS_table_bayes.csv",
                 BASE^"strategies/tables/RS_table_natlang.csv",
                 BASE^"strategies/tables/RS_table_euler.csv",
                 BASE^"strategies/tables/RS_table_geometric.csv",
                 BASE^"strategies/tables/RS_table_contingency.csv",
                 BASE^"strategies/tables/RS_table_1dimps.csv",
                 BASE^"strategies/tables/RS_table_conjtrees.csv"],
                [BASE^"strategies/tables/correspondences_generic.csv",
                 BASE^"strategies/tables/correspondences_natlang_interface.csv",
                 BASE^"strategies/tables/correspondences_natlang_realarith.csv",
                 BASE^"strategies/tables/correspondences_natlang_bayes.csv",
                 BASE^"strategies/tables/correspondences_natlang_geometric.csv",
                 BASE^"strategies/tables/correspondences_natlang_euler.csv",
                 BASE^"strategies/tables/correspondences_natlang_contingency.csv",
                 BASE^"strategies/tables/correspondences_natlang_conjtrees.csv",
                 BASE^"strategies/tables/correspondences_natlang_1dimps.csv"],
                [BASE^"strategies/tables/Q_table_" ^ (qName) ^ "_" ^ (qRep) ^ ".csv"]);
        val bestRepresentations = RepSelect.topKRepresentations question numAlternatives;
    in
        Logging.write ("RECOMMEND: " ^
             (if (List.null bestRepresentations)
              then "NONE"
              else (listToString (fn (s, _) => s) bestRepresentations)) ^
             "\n");

        if (List.null bestRepresentations)
        then print ("We have no recommended representations for the " ^
                    qName ^
                    " problem.\n")
        else (print ("For the " ^
                     qName ^
                     " problem we recommend using one of these " ^
                     "(in order of suitability)" ^
                     ":\n");
              map (fn (s, _) => print ("  " ^ s ^ "\n")) bestRepresentations;
             ());

        Logging.write ("\nEND algorithm-trace-" ^ today ^ "\n");
        0
     end;
