import "util.logging";
import "util.dictionary";

import "strategies.properties.tables";
import "strategies.correspondences.discovery";

(* To see a full trace of the algorithm, we enable logging.
   If this seems too 'noisy', you can use `Logging.disable ()`.
   (Alternatively, because disabled is the default logging state,
   you can just comment out the following line.)
*)
Logging.enable ();
val _ = registerPropertyReaders
            PropertyTables.setQGenerators
            PropertyTables.setRSGenerators;

structure TableDict = Dictionary(struct
                                  type k = string;
                                  val compare = String.compare;
                                  val fmt = (fn s => s);
                                  end);

structure FindCorrs = DiscoverCorrespondences;

exception Return of int;

fun parseArgs args =
    let fun splitFlags ans [] = List.rev(ans)
          | splitFlags ((f, a)::ans) (x::xs) =
            if String.isPrefix "--" x
            then splitFlags ((x, [])::(f, List.rev(a))::ans) xs
            else splitFlags ((f, x::a)::ans) xs
          | splitFlags [] _ = raise Match;
        fun getFlags f [] = raise Empty
          | getFlags f ((g, vs)::args) =
            if f = g then vs
            else getFlags f args;
        fun helper args =
            let val groups = splitFlags [("", [])] args;
                val rss = getFlags "--rs" groups
                          handle Empty => [];
                val corrs = getFlags "--corr" groups
                            handle Empty => [];
                val settings =
                    case (getFlags "" groups) of
                        [] => (NONE, NONE)
                      | [rs] => (SOME rs, NONE)
                      | [rs, count] => (SOME rs, Int.fromString count)
                      | (rs::count::_) =>
                        (Logging.error ("Ignoring extra arguments...");
                         (SOME rs, Int.fromString count));

            in (settings, rss, corrs) end;
    in
        case args of
            NONE => helper (CommandLine.arguments ())
          | SOME a => helper a
    end;


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


fun repl state =
    let
        fun fmt c r = (Correspondence.toString c)
                      ^ "\n\t"
                      ^ (FindCorrs.reasonString r);
        fun eval s i = case i of
                           SOME i => (let val ((c, r), t) = Stream.step s
                                      in (SOME t,  fmt c r) end
                                      handle Subscript =>
                                             (NONE,
                                              "\nAll out of suggestions!\n"))
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
        fun fmt (c, r) = (Correspondence.toString c)
                      ^ "\n\t"
                      ^ (FindCorrs.reasonString r)
                      ^ "\n";
        fun next s =
            let val (x, t) = Stream.lazyStep s
            in print (fmt (x())); next t end
            handle Subscript => print "End of suggestions.\n";
    in
        next stream
    end;

fun requestInput prompt =
    let val _ = print prompt;
        val read = TextIO.inputLine TextIO.stdIn;
    in case read of SOME v => v
                  | NONE => "" end;

fun loadTables (rss, corrs) =
    let
        fun loadRS name =
            let val _ = Logging.write ("Loading RS table " ^ name ^ "... \n");
                fun toDict (rs, props) = TableDict.fromPairList [(rs, props)];
            in
                toDict (PropertyTables.loadRepresentationTable name)
            end;
        val rsFiles = if List.null rss
                      then filesMatchingPrefix "tables/" "RS_table_"
                      else rss;
        val corrFiles = if List.null corrs
                        then filesMatchingPrefix "tables/" "correspondences_"
                        else corrs;
        val correspondences = List.concat
                                  (map PropertyTables.loadCorrespondenceTable
                                       corrFiles);
        val allRSs = TableDict.unionAll
                         (map loadRS rsFiles);
        val _ = Logging.write ("RS Tables found: "
                               ^ (List.toString (fn s => s)
                                                (TableDict.keys allRSs))
                               ^ "\n");
    in
        (allRSs, correspondences)
    end;

fun focusRS allRSs rsname =
    let
        val newRSName = case rsname of
                            SOME name => (Logging.write
                                              ("Taking " ^ name
                                               ^ " as the new RS.\n"); name)
                          | NONE => Parser.stripSpaces
                                        (requestInput "Which is the new RS? ");
        val newRS = TableDict.get allRSs newRSName
                    handle TableDict.KeyError =>
                           (Logging.error ("No RS named " ^ newRSName ^ "!\n");
                            PropertySet.empty (); raise Return 1);
        val _ = TableDict.remove allRSs newRSName;
        val oldRSs = TableDict.values allRSs;
    in
        (newRS, oldRSs)
    end;

fun main () =
    let
        val (settings, rsfiles, corrfiles) =
            parseArgs NONE
            handle Empty => (Logging.error "Missing arguments: source_rs count\n";
                            raise Return 1);
        val (allRSs, correspondences) = loadTables (rsfiles, corrfiles);
        val (rsname, rscount) = settings;
        val (newRS, oldRSs) = focusRS allRSs rsname;

        val state = (correspondences, oldRSs, newRS);
        val _ = if Option.isSome rscount then ()
                else print ("Press <enter> to view suggestions, ctrl-D to exit.\n");
        fun strongEnough (_, _, v) = true; (* Real.abs(v) > 0.2; *)
        fun reflexive (a, b, _) = Correspondence.F.equal
                                      (fn (p, q) =>
                                          Property.compare (p, q) = EQUAL)
                                      (a, b);
        val suggestions = Stream.filter
                              (fn (c, r) => strongEnough c
                                            andalso not (reflexive c))
                              (FindCorrs.discover state);
    in
        case rscount of
            NONE => repl suggestions
          | SOME k => if k = ~1
                      then showAll suggestions
                      else showAll (Stream.take k suggestions);
        0
    end
    handle Return i => i
         | exn =>
           let
               fun printError exn (SOME {file,
                                         startLine, startPosition,
                                         endLine, endPosition}) =
                   Logging.error ("Exception at " ^ file
                                  ^ ":" ^ (Int.toString startLine)
                                  ^ ":" ^ (Int.toString startPosition)
                                  ^ "-" ^ (Int.toString endLine)
                                  ^ ":" ^ (Int.toString endPosition) ^ "\n"
                                  ^ (exnMessage exn) ^ "\n")
                 | printError exn NONE = Logging.error (
                       "An exception occurred at an unknown location.\n"
                       ^ (exnMessage exn) ^ "\n");
           in
               printError exn (PolyML.Exception.exceptionLocation exn);
               PolyML.Exception.reraise exn
           end;
