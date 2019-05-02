import "util.logging";
import "util.set";
import "util.dictionary";
import "util.csv";

import "strategies.property";
import "strategies.property_importance";
import "strategies.property_correspondence";

signature PROPERTYTABLES =
sig
    exception TableError of string;

    type qgenerator = (string -> Property.value list) * Property.kind * Importance.importance;
    type rsgenerator = (string -> Property.value list) * Property.kind;
    type questiontable = (string * string) * QPropertySet.t QPropertySet.set;
    type representationtable = string * PropertySet.t PropertySet.set;
    type correspondencetable = Correspondence.correspondence list;

    val loadCorrespondenceTable : string -> correspondencetable;
    val loadQuestionTable : string -> questiontable;
    val loadRepresentationTable : string -> representationtable;

    val computePsuedoQuestionTable: questiontable -> representationtable -> correspondencetable -> questiontable;

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

type qgenerator = (string -> Property.value list) * Property.kind * Importance.importance;
type rsgenerator = (string -> Property.value list) * Property.kind;
type questiontable = (string * string) * QPropertySet.t QPropertySet.set;
type representationtable = string * PropertySet.t PropertySet.set;
type correspondencetable = Correspondence.correspondence list;

datatype CorrTree = Prop of string
                  | Neg of CorrTree
                  | Conj of CorrTree * CorrTree
                  | Disj of CorrTree * CorrTree;


fun readCorrespondence q r s =
    let
        val rowString = q ^ "," ^ r ^ "," ^ s;
    in
        [Correspondence.fromString rowString]
    end;

fun loadCorrespondenceTable filename =
    let
        fun makeRow [x, y] = (readCorrespondence x y "1.0")
          | makeRow [x, y, z] = (readCorrespondence x y z)
          | makeRow r = raise TableError
                              ("Correspondence table entry malformed: " ^
                               (listToString (fn s => s) r));
        val csvFile = CSVLiberal.openIn filename;
        val csvData = CSVLiberal.input csvFile;
    in
        List.foldr (fn (r, xs) => (makeRow r) @ xs) [] csvData
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
                  | NONE => ((fn s => [Property.Label s]),Property.kindOfString (key ), Importance.Low);
            val importance = case overrideImportance of
                                    NONE => defaultImportance
                                  | SOME i => i;
            fun makeProp v = QProperty.fromPair
                                 (Property.fromKindValuePair ( keypre, v),
                                  importance);
        in
            qset' (map makeProp (valparser args))
        end;
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
                                        | NONE => ((fn s => [Property.Label s]), Property.kindOfString (key ));
            fun makeProp v = Property.fromKindValuePair ( keypre, v);
        in
            set' (map makeProp (valparser args))
        end;
in
    loadQorRSPropertiesFromFile sets parsers genProps filename
end;

local structure KindSet = Set(struct
                               type t = Property.kind;
                               fun compare (k, k') =
                                   String.compare(Property.stringOfKind k,
                                                  Property.stringOfKind k');
                               val fmt = Property.stringOfKind;
                               end) in
fun computePsuedoQuestionTable qTable targetRSTable corrTable = let
    val ((qName, qRS), sourceProperties) = qTable;
    val (targetRSName, targetRSProperties) = targetRSTable;
    val badKinds = (KindSet.fromList o (map Property.kindOfString)) [
                       "error_allowed",
                       "num_tokens",
                       "num_distinct_tokens"
                   ];
    val propertyKinds = List.filter
                            (fn k => not (KindSet.contains badKinds k))
                            (map (fn (r, k, i) => k)
                                 (GenDict.values (!qPropertyKeyMap)));
    fun dropImportance props =
        PropertySet.fromList (QPropertySet.map (QProperty.withoutImportance) props);
    fun liftImportance c =
        let
            fun collateImportances props =
                let
                    val uniqueProperties = dropImportance props;
                    fun collectImportances p' qps =
                        let
                            fun filterSome' [] ans = List.rev ans
                              | filterSome' ((SOME x)::xs) ans = filterSome' xs (x::ans)
                              | filterSome' (NONE::xs) ans = filterSome' xs ans;
                            fun filterSome xs = filterSome' xs [];
                            fun someImportance qp =
                                let
                                    val (p, i) = QProperty.toPair qp;
                                in
                                    if (Property.compare (p', p) = EQUAL)
                                    then SOME i else NONE
                                end;
                        in
                            filterSome (QPropertySet.map someImportance qps)
                        end;
                in
                    PropertyDictionary.fromPairList (
                        PropertySet.map
                            (fn p => (p, collectImportances p props))
                            uniqueProperties)
                end;
            val importanceLookup = collateImportances sourceProperties;
            val getImportance = PropertyDictionary.get importanceLookup;
            val flatten = flatmap (fn x => x);
            val qp = Correspondence.leftMatches
                         (dropImportance sourceProperties)
                         c;
        in
            map (fn i => (c, i)) (flatten (PropertySet.map getImportance qp))
        end;
    val sourceProps = dropImportance sourceProperties;
    val matches' = List.filter
                       (Correspondence.match
                            sourceProps
                            targetRSProperties)
                      corrTable;
    val identityPairs = PropertySet.map
                            Correspondence.identity
                            (PropertySet.collectLeftMatches
                                 sourceProps
                                 targetRSProperties);
    val identityPairs' = List.filter
                             (fn corr =>
                                 not(List.exists
                                         (Correspondence.matchingProperties corr)
                                         matches'))
                             identityPairs;
    val matches = flatmap liftImportance (matches' @ identityPairs');
    fun translateProperty (correspondence, importance) =
        let
            val strength = Correspondence.strength correspondence;
            fun hasKind k p = (Property.kindOf p) = k;
            fun hasValidKind p =
                any (map (fn k => hasKind k p) propertyKinds);
            val properties =
                PropertySet.filter hasValidKind (Correspondence.rightMatches
                                                     targetRSProperties
                                                     correspondence);
            val makeQProperty = fn p => QProperty.fromPair (p, importance);
        in
            if strength > 0.5
            then SOME (QPropertySet.fromList
                           (PropertySet.map makeQProperty properties))
            else NONE
        end;
    val errorAllowed = let
        fun findError [] = raise Match
          | findError (x::xs) =
            let
                val propString = Property.toString
                                     (QProperty.withoutImportance x);
                val isErrorAllowed = String.isPrefix "error-allowed-" propString;
            in
                if isErrorAllowed then x else findError xs
            end;
    in
        findError (QPropertySet.toList sourceProperties)
    end;
    val unionAll = List.foldr (fn (a, b) => QPropertySet.union a b) (QPropertySet.empty ());
    val newProperties = unionAll (filtermap translateProperty matches);
    val _ = QPropertySet.insert newProperties errorAllowed
in
    (("pseudo-" ^ qName, targetRSName), newProperties)
end;
end;

end;
