import "util.logging";
import "util.configuration";

import "strategies.properties.property";
import "strategies.properties.tables";
import "strategies.properties.readers"; (* Must come after strategies.property_tables *)
import "strategies.properties.importance";
import "strategies.correspondences.correspondence";

(* To see a full trace of the algorithm, we enable logging.
   If this seems too 'noisy', you can use `Logging.disable ()`.
   (Alternatively, because disabled is the default logging state,
   you can just comment out the following line.)
*)
Logging.enable ();

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

fun dedupCorrespondences [] = []
      | dedupCorrespondences (x::xs) = let
          fun removeCorr y [] = []
            | removeCorr y (z::zs) =
              if Correspondence.matchingProperties y z
              then (
                  if Correspondence.equal y z then
                      removeCorr y zs
                  else
                      (Logging.error ("ERROR: Conflicting correspondences:\n");
                       Logging.error ("\t" ^
                                      (Correspondence.toString y) ^
                                      "\n");
                       Logging.error ("\t" ^
                                      (Correspondence.toString z) ^
                                      "\n");
                       raise Fail "Conflicting correspondence values")
              )
              else z::(removeCorr y zs);
      in
          x::(dedupCorrespondences (removeCorr x xs))
      end;

fun parseArgs () =
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

        val rawArgs = splitFlags [("", [])] (CommandLine.arguments ());
        val positional = getFlags "" rawArgs;
        val tables = SOME (getFlags "--tables" rawArgs)
                     handle Empty => NONE;
        val args = case positional of
                       [ab, c, d] => let val (a, _, b) = Parser.breakOn ":" ab
                                     in (a, b, c, d, tables) end
                     | l => (print ("Expected three arguments, got "
                                    ^ (Int.toString (List.length l)) ^ "\n");
                             raise Match);
    in args end;

fun main () =
    let
        val _ = registerPropertyReaders
                    PropertyTables.setQGenerators
                    PropertyTables.setRSGenerators;

        val (qName, qRep, altRep, outfile, tabdir) = parseArgs ();
        val tableDir = case tabdir of SOME d => List.hd d
                                    | NONE => "tables/";
        val qFile =  tableDir ^ "Q_table_" ^ qName ^ "_" ^ qRep ^ ".csv";
        val rsFile = tableDir ^ "RS_table_" ^ altRep ^ ".csv";
        val corrFiles = filesMatchingPrefix tableDir  "correspondences_";

        val q = PropertyTables.loadQuestionTable qFile;
        val _ = Logging.write ("LOAD " ^ qFile ^ "\n");
        val rs = PropertyTables.loadRepresentationTable rsFile;
        val _ = Logging.write ("LOAD " ^ rsFile ^ "\n");
        val corr = dedupCorrespondences (
                List.concat
                    (map (fn t => (Logging.write ("LOAD " ^ t ^ "\n");
                                   PropertyTables.loadCorrespondenceTable t))
                         corrFiles));


        (* qTable targetRSTable corrTable *)
        val newTable = PropertyTables.computePseudoQuestionTable q rs corr;
        val _ = PropertyTables.questionTableToCSV newTable outfile;
    in 0 end;
