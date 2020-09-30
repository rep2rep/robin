import "util.logging";
import "util.configuration";
import "strategies.representation_selection";

(* To see a full trace of the algorithm, we enable logging.
   If this seems too 'noisy', you can use `Logging.disable ()`.
   (Alternatively, because disabled is the default logging state,
   you can just comment out the following line.)
*)
Logging.enable ();

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

fun parseArgs () =
    let
        val args = CommandLine.arguments ();
        val configuration =
            case args of
                [fname] => Configuration.configFromFile fname
              | _ => Configuration.configFromCommandLine args;
    in configuration end
    handle Configuration.ArgumentError s => (Logging.error(s);
                                             raise Configuration.ArgumentError s)

fun main () =
    let
        val today = Date.fmt "%Y-%m-%d" (Date.fromTimeLocal (Time.now()));
        val version = "robin-" ^ ROBIN_VERSION;
        val ((qName, qRep), numAlternatives, tableDir') = parseArgs ();
        val tableDir = case tableDir' of SOME d => d
                                       | NONE => "tables/";
        val rsFiles = filesMatchingPrefix tableDir "RS_table_";
        val corrFiles = filesMatchingPrefix tableDir "correspondences_";
        val _ = Logging.write ("BEGIN algorithm-trace-"
                               ^ today
                               ^ " with "
                               ^ version ^ "\n");
        val _ = RepSelect.init(rsFiles, corrFiles,
                               [tableDir ^ "Q_table_"
                                ^ (qName) ^ "_" ^ (qRep) ^ ".csv"]);
        val bestRepresentations = RepSelect.topKRepresentations (qName, qRep) numAlternatives;
    in
        Logging.write ("RECOMMEND: " ^
             (if (List.null bestRepresentations)
              then "NONE"
              else (List.toString (fn (s, _) => s) bestRepresentations)) ^
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
