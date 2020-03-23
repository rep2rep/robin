import "util.logging";
import "util.dictionary";

import "strategies.properties.tables";

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
                                  type k = string * string;
                                  val compare =
                                      Comparison.join String.compare
                                                      String.compare;
                                  val fmt = (fn (s, t) =>
                                                "(" ^ s ^ ", " ^ t ^ ")");
                                  end);

exception Return of int;

fun parseArgs NONE = CommandLine.arguments ()
  | parseArgs (SOME args) = args;

fun loadTables filenames =
    let
        fun loadTable name =
            let fun toDict (rs, props) = TableDict.fromPairList [(rs, props)];
            in
                toDict (PropertyTables.loadQuestionTable name)
            end;
    in
        TableDict.unionAll (map loadTable filenames)
    end;

fun printCSV propertyset =
    let
        fun groupByFirst xs =
            let
                fun collectLike (a, bs) [] ans = List.rev ((a, List.rev bs)::ans)
                  | collectLike (a, bs) ((x, y, z)::xs) ans =
                    if a = x
                    then collectLike (a, (y, z)::bs) xs ans
                    else collectLike (x, [(y, z)]) xs ((a, List.rev bs)::ans);
                val sorted = List.mergesort
                                 (fn ((a, b, c), (x, y, z)) =>
                                     Kind.compare(a, x))
                                 xs;
            in
                case sorted of
                    [] => []
                  | ((a, b, c)::xs) => collectLike (a, [(b, c)]) xs []
            end;
        fun makeRow (k, vs) =
            let
                fun pluralise "mode" = "mode"
                  | pluralise "error_allowed" = "error_allowed"
                  | pluralise k = k ^ "s";
                val kstring = pluralise (Kind.toString k);
                fun stringify (v, a) =
                    let
                        val property = Property.fromKindValueAttributes (k, v, a)
                        val fullstring = Property.toString property;
                        val (_, _, s) = Parser.breakOn "-" fullstring;
                    in
                        s
                    end;
            in
                [kstring, String.concatWith ", " (map stringify vs)]
            end;
        val output = TextIO.stdOut;
        val grouped = groupByFirst (PropertySet.map
                                        Property.toKindValueAttributes
                                        propertyset);
        val rows = map makeRow grouped;
        val _ = CSVDefault.outputRow output ["unionedTable"];
        val _ = CSVDefault.output output rows;
        val _ = CSVDefault.flushOut output;
    in
        ()
    end;

fun main () =
    let
        val filenames = parseArgs NONE;
        val qtables = loadTables filenames;
        val unionedTable = PropertySet.unionAll
                               (map
                                    (PropertySet.fromList o
                                     (QPropertySet.map
                                          QProperty.withoutImportance))
                                    (TableDict.values qtables));
    in
        printCSV (unionedTable);
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
