import "util.logging";
import "util.configuration";

import "strategies.properties.tables";
import "strategies.properties.discover_correspondences";

(* To see a full trace of the algorithm, we enable logging.
   If this seems too 'noisy', you can use `Logging.disable ()`.
   (Alternatively, because disabled is the default logging state,
   you can just comment out the following line.)
*)
Logging.enable ();
val _ = registerPropertyReaders
            PropertyTables.setQGenerators
            PropertyTables.setRSGenerators;

structure FindCorrs = DiscoverCorrespondences;

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
                                             raise Configuration.ArgumentError s);

fun repl state =
    let
        val prompt = "? ";
        fun eval s i = case i of
                           SOME i => (SOME s, "beep boop, you said '" ^ (Parser.stripSpaces i) ^ "'!\n")
                         | NONE => (NONE, "Bye!\n");

        val _ = print prompt;
        val read = TextIO.inputLine TextIO.stdIn;
        val (newState, output) = eval state read;
        val _ = print output;
    in
        case newState of
            SOME newState => repl newState
          | NONE => ()
    end;

fun requestInput prompt =
    let val _ = print prompt;
        val read = TextIO.inputLine TextIO.stdIn;
    in case read of SOME v => v
                  | NONE => "" end;

fun main () =
    let
        val rss = [];
        val corrs = [];
        val rsFiles = if List.null rss
                      then filesMatchingPrefix "tables/" "RS_table_"
                      else map (fn rs => "tables/RS_table_" ^ rs ^ ".csv") rss;
        val corrFiles = if List.null corrs
                        then filesMatchingPrefix "tables/" "correspondences_"
                        else map (fn c => "tables/correspondences_" ^ c ^ ".csv") corrs;
        val correspondences = List.concat (map PropertyTables.loadCorrespondenceTable
                                               corrFiles);
        val allRSs = PropertyTables.FileDict.unionAll
                         (map PropertyTables.loadRepresentationTable rsFiles);
        val newRSName = requestInput "Which is the new RS? ";
        val newRS = PropertyTables.FileDict.get allRSs newRSName;
        val _ = PropertyTables.FileDict.remove allRSs newRSName;
        val oldRSs = PropertyTables.FileDict.values allRSs;
        val state = (correspondences, oldRSs, newRS);
    in
        (* repl state; *)
        FindCorrs.discover state;
        0
     end;
