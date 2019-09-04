import "util.logging";
import "util.set";
import "util.dictionary";
import "util.csv";

import "strategies.properties.property";
import "strategies.properties.kind";
import "strategies.properties.importance";
import "strategies.properties.correspondence";
import "strategies.properties.readers";

signature PROPERTYTABLES =
sig
    exception TableError of string;

    type qgenerator = (string -> (Property.value * Attribute.T list) list) * Kind.kind * Importance.importance;
    type rsgenerator = (string -> (Property.value * Attribute.T list) list) * Kind.kind;
    type questiontable = (string * string) * QPropertySet.t QPropertySet.set;
    type representationtable = string * PropertySet.t PropertySet.set;
    type correspondencetable = Correspondence.correspondence list;

    val loadCorrespondenceTable : string -> correspondencetable;
    val loadQuestionTable : string -> questiontable;
    val loadRepresentationTable : string -> representationtable;

    val computePseudoQuestionTable: questiontable -> representationtable -> correspondencetable -> questiontable;
    val questionTableToCSV: questiontable -> string -> unit;

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
type questiontable = (string * string) * QPropertySet.t QPropertySet.set;
type representationtable = string * PropertySet.t PropertySet.set;
type correspondencetable = Correspondence.correspondence list;

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


fun loadQorRSPropertiesFromFile sets parsers genProps filename  =
    let
        val (setEmpty, setUnion) = sets;
        val (parseHeader, parseRow) = parsers;
        val csvFile = CSVLiberal.openIn filename;
        val csvDataWithHeader = CSVLiberal.input csvFile;
        val csvHeader = parseHeader (List.hd csvDataWithHeader)
                        handle List.Empty => raise TableError "Table is empty";
        val csvData = (List.tl csvDataWithHeader)
                      handle List.Empty => raise TableError "Table is empty";

        val properties = List.foldr
                             (fn (r, xs) => setUnion (genProps r) xs)
                             (setEmpty ())
                             (map parseRow csvData);
    in
        (csvHeader, properties)
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
    fun parseHeader [] = raise TableError ("Q Table "
                                           ^ "has empty header")
      | parseHeader [x] = raise TableError ("Q Table "
                                            ^ "is missing RS label in header")
      | parseHeader [x, ""] = raise TableError ("Q Table "
                                                ^ "has blank RS label in header")
      | parseHeader [x, y] = (x, y)
      | parseHeader _ = raise TableError ("Q Table "
                                          ^ "has malformed header");
    fun parseRow [x, y] = (x, y, NONE)
      | parseRow [x, y, z] = (x, y, SOME (parseImportance z))
      | parseRow _ = raise TableError "Malformed question property entry";
    val parsers = (parseHeader, parseRow);
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
    loadQorRSPropertiesFromFile sets parsers genProps filename
end;

fun loadRepresentationTable filename = let
    val sets = (S.empty, S.union);
    fun parseHeader [] = raise TableError ("RS table "
                                           ^ "has empty header.")
      | parseHeader [x] = x
      | parseHeader [x, ""] = x
      | parseHeader _ = raise TableError ("RS Table "
                                          ^ "has malformed header.")
    fun parseRow [x, y] = (x, y)
      | parseRow _ = raise TableError ("RS Table "
                                       ^ "has malformed property entry");
    val parsers = (parseHeader, parseRow);
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
    loadQorRSPropertiesFromFile sets parsers genProps filename
end;

local structure KindSet = Set(struct
                               type t = Kind.kind;
                               val compare = Kind.compare;
                               val fmt = Kind.toString;
                               end) in
fun computePseudoQuestionTable qTable targetRSTable corrTable = let
    val ((qName, qRS), sourceProperties) = qTable;
    val (targetRSName, targetRSProperties) = targetRSTable;
    fun translateProperty (correspondence, importance) =
        let
            val allKinds = KindSet.fromList Kind.allKinds;
            val badKinds = KindSet.fromList [Kind.ErrorAllowed,
                                             Kind.NumTokens,
                                             Kind.NumDistinctTokens];
            val propertyKinds = KindSet.difference allKinds badKinds;
            val strength = Correspondence.strength correspondence;
            val properties = PropertySet.filter
                                 (fn p => KindSet.contains propertyKinds
                                                           (Property.kindOf p))
                                 (Correspondence.rightMatches
                                      targetRSProperties correspondence);
            val makeQProperty = fn p => QProperty.fromPair (p, importance);
        in
            if strength > 0.5
            then SOME (QPropertySet.fromList
                           (PropertySet.map makeQProperty properties))
            else NONE
        end;
    val sourceProps = QPropertySet.withoutImportances sourceProperties;
    val matches =
        let fun liftImportance c =
                map (fn i => (c, i))
                    (Correspondence.liftImportances sourceProperties c);
            fun alreadyCorr correspondences corr =
                List.exists (Correspondence.matchingProperties corr)
                            correspondences;
            val baseCorrs = List.filter
                                (Correspondence.match sourceProps
                                                      targetRSProperties)
                                corrTable;
            val allIdentities = PropertySet.map
                                    Correspondence.identity
                                    (PropertySet.collectLeftMatches
                                         sourceProps targetRSProperties);
            val identityCorrs = List.filter
                                    (fn corr => not (alreadyCorr baseCorrs corr))
                                    allIdentities;
            val correspondences = identityCorrs @ baseCorrs;
        in List.flatmap liftImportance correspondences end;
    val newProperties = QPropertySet.unionAll
                            (List.mapPartial translateProperty matches);
    val errorAllowed = QPropertySet.find
                           (fn qp => (QProperty.kindOf qp) = Kind.ErrorAllowed)
                           sourceProperties;
    val _ = case errorAllowed of
                SOME ea => QPropertySet.insert newProperties ea
              | NONE => ();
in
    (("pseudo-" ^ qName, targetRSName), newProperties)
end;

fun questionTableToCSV ((qname, qrs), qproperties) filename =
    let
        val propertyKinds = map (fn (s, (r, k, i)) => (k, i, s))
                                (GenDict.items (!qPropertyKeyMap));

        fun qPropertyToTriple qp =
            let
                fun valueString (Property.Label s) = s
                  | valueString (Property.Number i) = Int.toString i
                  | valueString (Property.Boolean b) = if b then "TRUE" else "FALSE"
                  | valueString (Property.Type t) = Type.toString t
                  | valueString (Property.Raw s) = s;
                val (p, importance) = QProperty.toPair qp;
                val (kind, rawValue, attributes) = Property.toKindValueAttributes p;
                val value = valueString rawValue;
            in
                (kind, value, importance)
            end;
        fun getLabel (k, i) =
            let
                fun notRelated (_, _, x) = not (String.isSubstring "_related_" x);
                fun findName [] = (Logging.error("Cannot find "
                                                 ^ (Kind.toString k)
                                                 ^ ", "
                                                 ^ (Importance.toString i)
                                                 ^ "\n");
                                   raise Match)
                  | findName ((k', i', s)::xs) =
                    if k' = k andalso i' = i then s
                    else findName xs;
            in
                findName (List.filter notRelated propertyKinds)
            end;
        fun groupByFirst xs =
            let
                fun collectLike (a, bs) [] ans = List.rev ((a, List.rev bs)::ans)
                  | collectLike (a, bs) ((x, y)::xs) ans =
                    if a = x
                    then collectLike (a, y::bs) xs ans
                    else collectLike (x, [y]) xs ((a, List.rev bs)::ans);
                val sorted = List.mergesort
                                 (fn ((a, b), (x, y)) => String.compare(a, x))
                                 xs;
            in
                case sorted of
                    [] => []
                  | ((a, b)::xs) => collectLike (a, [b]) xs []
            end;
        fun makeCell xs = map
                              (fn (a, b) =>
                                  [a, String.concat (List.intersperse ", " b)])
                              xs;
        val triples = QPropertySet.map qPropertyToTriple qproperties;
        val pairs = map (fn (k, v, i) => (getLabel (k, i), v)) triples;
        val cellData = (makeCell o groupByFirst) pairs;
        val csvFile = CSVLiberal.openOut filename;
        val _ = CSVLiberal.outputRow csvFile [qname, qrs];
        val _ = CSVLiberal.output csvFile cellData;
        val _ = (CSVLiberal.flushOut csvFile; CSVLiberal.closeOut csvFile);
    in () end;

end;

end;
