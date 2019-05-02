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

fun filesMatchingPrefix dir prefix =
    let
        fun getWholeDir direc out = case OS.FileSys.readDir (direc) of
                                      SOME f => getWholeDir direc (f::out)
                                    | NONE => List.rev out;
        val dirstream = OS.FileSys.openDir dir;
        val filenames = getWholeDir dirstream [];
        val filteredFiles = List.filter (String.isPrefix prefix) filenames;
        fun attachDir p = dir ^ p;
    in
        map (OS.FileSys.fullPath o attachDir) filteredFiles
    end;

(* The user supplies the specified problem as "name:representation",
   for example "medical:bayes". This gets deconstructed to load a particular file.
*)
fun readQuestion fileName =
    let
        val separator = #":";
    in
        case (String.tokens (fn c => c = separator) fileName) of
            [p, r] => (p, r)
          | _ => (Logging.error ("ERROR: cannot parse \"problem" ^
                                 (str separator) ^
                                 "representation\" from the first argument");
                  raise ArgumentError 1)
    end;

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
            [] => (Logging.error "ERROR: No arguments given, requires 1 or 2."; raise ArgumentError 0)
          | [fname] => (Logging.write noNumAltError; (fname, defaultAlts))
          | (fname::altNumString::rest) =>
            case (Int.fromString altNumString) of
                SOME k => (fname, k)
              | NONE => (Logging.write noNumAltError; (fname, defaultAlts))
    end;

fun main () =
    let
        val today = Date.fmt "%Y-%m-%d" (Date.fromTimeLocal (Time.now()));
        val (qFileName, numAlternatives) = parseArgs ();
        val question = readQuestion qFileName;
        val (qName, qRep) = question;
        val _ = Logging.write ("BEGIN algorithm-trace-" ^ today ^ "\n");
        val _ = RepSelect.init(
                filesMatchingPrefix ("tables/") "RS_table_",
                filesMatchingPrefix ("tables/") "correspondences_",
                ["tables/Q_table_" ^ (qName) ^ "_" ^ (qRep) ^ ".csv"]);
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
