import "util.logging";

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
                [] => (NONE, NONE)
              | [rs] => (SOME rs, NONE)
              | [rs, count] => (SOME rs, Int.fromString count)
              | (rs::count::_) => (Logging.error ("Ignoring extra arguments..."); (SOME rs, Int.fromString count));
    in configuration end;

fun repl state =
    let
        fun fmt c r = (Correspondence.toString c)
                      ^ "\n\t"
                      ^ (FindCorrs.reasonString r);
        fun eval s i = case i of
                           SOME i => (let val ((c, r), t) = Stream.step s
                                      in (SOME t,  fmt c r) end
                                      handle Subscript => (NONE, "\nAll out of suggestions!\n"))
                         | NONE => (NONE, "\nBye!\n");

        val read = TextIO.inputLine TextIO.stdIn;
        val (newState, output) = eval state read;
        val _ = print output;
    in
        case newState of
            SOME newState => repl newState
          | NONE => ()
    end;

fun showAll stream =
    let
        fun fmt c r = (Correspondence.toString c)
                      ^ "\n\t"
                      ^ (FindCorrs.reasonString r);
        fun next s =
            let val ((c, r), t) = Stream.step s
            in print (fmt c r); next t end
            handle Subscript => ();
    in
        next stream
    end;

fun requestInput prompt =
    let val _ = print prompt;
        val read = TextIO.inputLine TextIO.stdIn;
    in case read of SOME v => v
                  | NONE => "" end;

fun main () =
    let
        fun loadRS name =
            let val _ = print ("Loading RS table " ^ name ^ "... \n");
            in
                PropertyTables.loadRepresentationTable name
            end;
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
                         (map loadRS rsFiles);
        val _ = print ("RS Tables found: " ^ (List.toString (fn s => s) (PropertyTables.FileDict.keys allRSs)) ^ "\n");

        val (rsname, rscount) = parseArgs ();

        val newRSName = case rsname of
                            SOME name => (print ("Taking " ^ name ^ " as the new RS.\n"); name)
                          | NONE => Parser.stripSpaces (requestInput "Which is the new RS? ");
        val newRS = PropertyTables.FileDict.get allRSs newRSName
                    handle PropertyTables.FileDict.KeyError =>
                           (print ("No RS named "^newRSName^"!\n"); PropertySet.empty ());
        val _ = PropertyTables.FileDict.remove allRSs newRSName;
        val oldRSs = PropertyTables.FileDict.values allRSs;
        val state = (correspondences, oldRSs, newRS);
        val suggestions = FindCorrs.discover state;
    in
        case rscount of
            NONE => (print ("Press <enter> to view suggestions, ctrl-D to exit.\n"); repl suggestions)
          | SOME k => showAll (Stream.take k suggestions);
        0
    end
    handle exn =>
           let
               fun printError exn (SOME {file,startLine,startPosition,endLine,endPosition}) =
                   Logging.error ("Exception at " ^ file
                                  ^ ":" ^ (Int.toString startLine) ^ ":" ^ (Int.toString startPosition)
                                  ^ "-" ^ (Int.toString endLine) ^ ":" ^ (Int.toString endPosition) ^ "\n"
                                  ^ (exnMessage exn) ^ "\n")
                 | printError exn NONE = Logging.error ("An exception occurred at an unknown location.\n"
                                                        ^ (exnMessage exn) ^ "\n");
           in
               printError exn (PolyML.Exception.exceptionLocation exn);
               PolyML.Exception.reraise exn
           end;
