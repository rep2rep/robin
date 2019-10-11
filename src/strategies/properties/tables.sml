import "util.logging";
import "util.set";
import "util.dictionary";
import "util.csv";

import "strategies.properties.property";
import "strategies.properties.importance";
import "strategies.properties.correspondence";
import "strategies.properties.readers";

signature PROPERTYTABLES =
sig
    exception TableError of string;

    structure FileDict : DICTIONARY;

    type qgenerator = (string -> (Property.value * Attribute.T list) list) * Kind.kind * Importance.importance;
    type rsgenerator = (string -> (Property.value * Attribute.T list) list) * Kind.kind;

    val loadCorrespondenceTable : string -> Correspondence.correspondence list;
    val loadQuestionTable : string -> (FileDict.k, QPropertySet.t QPropertySet.set) FileDict.dict;
    val loadRepresentationTable : string -> (FileDict.k, PropertySet.t PropertySet.set) FileDict.dict;

    val setQGenerator : (string * qgenerator) -> unit;
    val setQGenerators : (string * qgenerator) list -> unit;
    val setRSGenerator : (string * rsgenerator) -> unit;
    val setRSGenerators : (string * rsgenerator) list -> unit;

end;


structure PropertyTables : PROPERTYTABLES =
struct

exception TableError of string;

structure SQ = QPropertySet;
structure S = PropertySet;
val qset' = SQ.fromList;
val set' = S.fromList;

structure D = PropertyDictionary;
val dict' = D.fromPairList;
fun getValue d k = SOME (D.get d k)
                   handle D.KeyError => NONE;

structure FileDict = Dictionary(struct
                                 type k = string;
                                 val compare = String.compare;
                                 val fmt = (fn s => s);
                                 end);
val filedict' = FileDict.fromPairList;

structure GenDict = Dictionary(struct
                                 type k = string;
                                 val compare = String.compare;
                                 val fmt = (fn s => s);
                                 end);
val gdict' = GenDict.fromPairList;

structure CSVLiberal = CSVIO(struct val delimiters = [#","];
                                    val newlines = ["\r", "\n", "\r\n"];
                             end);

type qgenerator = (string -> (Property.value * Attribute.T list) list) * Kind.kind * Importance.importance;
type rsgenerator = (string -> (Property.value * Attribute.T list) list) * Kind.kind;

fun readCorrespondence q r s =
    let
        val rowString = q ^ "," ^ r ^ "," ^ s;
    in
        [Correspondence.fromString rowString]
        handle Correspondence.ParseError =>
               raise TableError ("Failed to parse row: " ^ rowString ^ "\n")
    end;

fun loadCorrespondenceTable filename =
    let
        fun makeRow [x, y] = (readCorrespondence x y "1.0")
          | makeRow [x, y, z] = (readCorrespondence x y z)
          | makeRow r = raise TableError
                              ("Correspondence table entry malformed: " ^
                               (List.toString (fn s => s) r));
        val csvFile = CSVLiberal.openIn filename;
        val csvData = CSVLiberal.input csvFile;
        val _ = CSVLiberal.closeIn csvFile;
    in
        List.flatmap makeRow csvData
    end
    handle IO.Io e => (Logging.error ("ERROR: File '" ^ filename ^ "' could not be loaded\n");
                       raise (IO.Io e))
         | TableError reason => (
             Logging.error ("ERROR: CSV parsing failed in file '" ^ filename ^ "'\n");
             Logging.error ("       " ^ reason ^ "\n");
             raise TableError reason
         );

(*
We provide a way to extend the known set of property generators with custom
generation functions. These take the form of
    (property-key, (generator, prefix, importance))
where property-key and prefix are strings, importance is an importance, and
generator is a function. To add them, use either setGenerators with a list,
or setGenerator with a tuple. Avoid running map over setGenerator, as it is
faster to use the predefined 'plural'.
Note that there are two types: QGenerators and RSGenerators. The first include
the importance part, while the second do not. This is because there is no
concept of importance when dealing with the RS in abstract terms. The question
generators give the default importance of a property, but the property tables
can over-ride this importance by specifying it in a third column.
*)
val qPropertyKeyMap = ref (GenDict.empty ());
fun setQGenerators new =
    let
        val _ = qPropertyKeyMap := GenDict.union (gdict' new) (!qPropertyKeyMap);
    in () end;
fun setQGenerator new =
    let
        val _ =  GenDict.insert (!qPropertyKeyMap) new;
    in () end;

val rPropertyKeyMap = ref (GenDict.empty ());
fun setRSGenerators new =
    let
        val _ = rPropertyKeyMap := GenDict.union (gdict' new) (!rPropertyKeyMap);
    in () end;
fun setRSGenerator new =
    let
        val _ = GenDict.insert (!rPropertyKeyMap) new;
    in () end;


fun loadQorRSPropertiesFromFile sets parseRow genProps filename  =
    let
        val (setEmpty, setUnion) = sets;
        val csvFile = CSVLiberal.openIn filename;
        val csvDataWithHeader = CSVLiberal.input csvFile;
        val csvHeader =
            (case (List.hd csvDataWithHeader) of
                 [] => raise TableError "Table header blank"
               | [h] => h
               | [h, ""] => h
               | _ => raise TableError "Missing table header")
            handle List.Empty => raise TableError "Table is empty";
        val csvData = (List.tl csvDataWithHeader)
                      handle List.Empty => raise TableError "Table is empty";

        val properties = List.foldr
                             (fn (r, xs) => setUnion (genProps r) xs)
                             (setEmpty ())
                             (map parseRow csvData);
        val _ = CSVLiberal.closeIn csvFile;
    in
        [(csvHeader, properties)]
    end
    handle IO.Io e => (Logging.error ("ERROR: File '" ^ filename ^ "' could not be loaded\n");
                       raise (IO.Io e))
         | TableError reason => (
             Logging.error ("ERROR: CSV parsing failed in file '" ^ filename ^ "'\n");
             Logging.error ("       " ^ reason ^ "\n");
             raise TableError reason
         );

fun loadQuestionTable filename = let
    val sets = (SQ.empty, SQ.union);
    fun parseImportance s = case Importance.fromString s of
                                SOME i => i
                              | NONE => raise TableError ("Unknown importance '" ^ s ^ "'");
    fun parseRow [x, y] = (x, y, NONE)
      | parseRow [x, y, z] = (x, y, SOME (parseImportance z))
      | parseRow _ = raise TableError "Malformed question property entry";
    fun genProps (key, args, overrideImportance) =
        let
            fun findQGenerator k = SOME (GenDict.get (!qPropertyKeyMap) k)
                                   handle GenDict.KeyError => NONE;
            val (valparser, keypre, defaultImportance) =
                case (findQGenerator key) of
                    SOME kt => kt
                  | NONE => ((fn s => [(Property.Label s,[])]), (Kind.fromString key), Importance.Low);
            val importance = case overrideImportance of
                                    NONE => defaultImportance
                                  | SOME i => i;
            fun makeProp (v,A) = QProperty.fromPair
                                   (Property.fromKindValueAttributes (keypre, v, A), importance);
        in
            qset' (map makeProp (valparser args))
        end
        handle Kind.KindError =>
               raise TableError ("Unable to determine kind of " ^ key);
in
    filedict' (loadQorRSPropertiesFromFile sets parseRow genProps filename)
end;

fun loadRepresentationTable filename = let
    val sets = (S.empty, S.union);
    fun parseRow [x, y] = (x, y)
      | parseRow _ = raise TableError "Malformed representation property entry";
    fun genProps (key, args) =
        let
            fun findRSGenerator k = SOME (GenDict.get (!rPropertyKeyMap) k)
                                    handle GenDict.KeyError => NONE;
            val (valparser, keypre) = case (findRSGenerator key) of
                                          SOME kt => kt
                                        | NONE => raise TableError ("Unknown property kind: " ^ key);
            fun makeProp (v,A) = Property.fromKindValueAttributes (keypre, v, A);
        in
            set' (map makeProp (valparser args))
        end
        handle PropertyReader.ReadError (r, v) =>
               raise TableError ("Unable to read " ^ r ^ ": " ^ v);
in
    filedict' (loadQorRSPropertiesFromFile sets parseRow genProps filename)
end;

end;
