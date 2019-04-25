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

    type qgenerator = (string -> string list) * string * Importance.importance;
    type rsgenerator = (string -> string list) * string;
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

type qgenerator = (string -> string list) * string * Importance.importance;
type rsgenerator = (string -> string list) * string;
type questiontable = (string * string) * QPropertySet.t QPropertySet.set;
type representationtable = string * PropertySet.t PropertySet.set;
type correspondencetable = Correspondence.correspondence list;

datatype CorrTree = Prop of string
                  | Neg of CorrTree
                  | Conj of CorrTree * CorrTree
                  | Disj of CorrTree * CorrTree;


fun readCorrespondence qpString rspString strengthString =
    let
        fun tokenize string =
            let
                fun cluster [] xs = cluster [[]] xs
                  | cluster cs [] = cs
                  | cluster (c::cs) (x::xs) =
                    case x of
                        #"(" => cluster ([]::[#"("]::c::cs) xs
                      | #")" => cluster ([]::[#")"]::c::cs) xs
                      | s => if Char.isSpace s
                             then cluster ([]::c::cs) xs
                             else cluster ((s::c)::cs) xs;
            in
                List.rev
                    (map (String.implode o List.rev)
                         (List.filter
                              (fn cs => not (List.null cs))
                              (cluster [[]] (String.explode string))))
            end;

        fun parseAtom tokens =
            let
                val nextTok = if (List.null tokens) then NONE
                              else (SOME (List.hd tokens));
            in
                case nextTok of
                    SOME "(" =>
                    let
                        val useTokens = List.tl tokens;
                        val (disjtree, remainingTokens) = parseDisj useTokens;
                        val closePar = if (List.null remainingTokens) then NONE
                                       else (SOME (List.hd remainingTokens));
                    in
                        case closePar of
                            SOME ")" => (disjtree, (List.tl remainingTokens))
                          | SOME other => raise TableError "Missing closing paren"
                          | NONE => (disjtree, [])
                    end
                  | SOME other => (Prop other, (List.tl tokens))
                  | NONE => raise TableError "Unexpected EOF"
            end
        and parseNeg tokens =
            let
                val nextTok = if (List.null tokens) then NONE
                              else (SOME (List.hd tokens));
            in
                case nextTok of
                    SOME "NOT" =>
                    let
                        val useTokens = List.tl tokens;
                        val (atomTree, remainingTokens) = parseAtom useTokens;
                    in
                        (Neg atomTree, remainingTokens)
                    end
                  | SOME other => parseAtom tokens
                  | NONE => raise TableError "Unexpected EOF"
            end
        and parseConj tokens =
            let
                val (readIn, nextTokens) = parseNeg tokens;
                val nextTok = if (List.null nextTokens) then NONE
                              else (SOME (List.hd nextTokens));
            in
                case nextTok of
                    SOME "AND" =>
                    let
                        val useTokens = List.tl nextTokens;
                        val (conjtree, remainingTokens) = parseConj useTokens;
                    in
                        (Conj (readIn, conjtree), remainingTokens)
                    end
                  | SOME other => (readIn, nextTokens)
                  | NONE => (readIn, [])
            end
        and parseDisj tokens =
            let
                val (readIn, nextTokens) = parseConj tokens;
                val nextTok = if (List.null nextTokens) then NONE
                              else (SOME (List.hd nextTokens));
            in
                case nextTok of
                    SOME "OR" =>
                    let
                        val useTokens = List.tl nextTokens;
                        val (distree, remainingTokens) = parseDisj useTokens;
                    in
                        (Disj (readIn, distree), remainingTokens)
                    end
                  | SOME other => (readIn, nextTokens)
                  | NONE => (readIn, [])
            end;

        fun parse tokens =
            let
                val (tree, tokens) = parseDisj tokens;
            in
                if (List.null tokens) then tree else raise TableError "Unprocessed content"
            end;

        fun normalise (Prop s) = Prop s
          | normalise (Neg s) =
            let
                val s' = normalise s;
            in
                case s' of
                    Prop k => Neg (Prop k)
                  | Neg k => normalise k
                  | Conj (a, b) => normalise (Disj (Neg a, Neg b))
                  | Disj (a, b) => normalise (Conj (Neg a, Neg b))
            end
          | normalise (Conj (a, b)) =
            let
                val a' = normalise a;
                val b' = normalise b;
            in
                case (a', b') of
                    (Disj (u, v), w) => normalise (Disj (Conj (u, w),
                                                         Conj (v, w)))
                  | (u, Disj (v, w)) => normalise (Disj (Conj (u, v),
                                                         Conj (u, w)))
                  | (u, v) => Conj (u, v)
            end
          | normalise (Disj (a, b)) =
            let
                val a' = normalise a;
                val b' = normalise b;
            in
                Disj (a', b')
            end;

        fun setify (Prop s) = (set' [Property.fromString s], S.empty ())
          | setify (Neg (Prop s)) = (S.empty (), set' [Property.fromString s])
          | setify (Neg _) = raise TableError
                                   "Correspondences incorrectly normalised"
          | setify (Conj (a, b)) =
            let
                fun ConjFlatten (Prop s) = [Prop s]
                  | ConjFlatten (Neg a) = [Neg a]
                  | ConjFlatten (Conj (a, b)) = (ConjFlatten a) @ (ConjFlatten b)
                  | ConjFlatten (Disj _) = raise TableError
                                                 "Correspondences incorrectly normalised";
                fun isPos (Prop s) = true
                  | isPos _ = false;
                fun isNeg (Neg a) = true
                  | isNeg _ = false;
                fun stripTreeness (Prop s) = Property.fromString s
                  | stripTreeness (Neg (Prop s)) = Property.fromString s
                  | stripTreeness _ = raise TableError
                                            "Correspondences incorrectly normalised";
                val flattened = ConjFlatten (Conj (a, b));
                val positives = set' (map stripTreeness (List.filter isPos flattened));
                val negatives = set' (map stripTreeness (List.filter isNeg flattened));
            in
                (positives, negatives)
            end
          | setify (Disj _) = raise TableError
                                              "Correspondences incorrectly normalised";

        val read = (normalise o parse o tokenize);

        fun strength str =
            case (Real.fromString str) of
                SOME v => v
              | NONE => raise TableError "Correspondence strength is not a float";

        fun genCorr (Disj (a, b)) t2 v =
            let
                val expa = genCorr a t2 v;
                val expb = genCorr b t2 v;
                val expc = genCorr (normalise (Conj (a, b))) t2 (~v);
            in
                expa @ expb @ expc
            end
          | genCorr t1 (Disj (a, b)) v =
            let
                val expa = genCorr t1 a v;
                val expb = genCorr t1 b v;
                val expc = genCorr t1 (normalise (Conj (a, b))) (~v);
            in
                expa @ expb @ expc
            end
          | genCorr t1 t2 v = [(t1, t2, v)];


        val qpTree = read qpString;
        val rspTree = read rspString;
        val corVal = strength strengthString;
    in
        map
            (fn (t1, t2, v) => (setify t1, setify t2, v))
            (genCorr qpTree rspTree corVal)
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
                  | NONE => ((fn s => [s]), key ^ "-", Importance.Low);
            val importance = case overrideImportance of
                                    NONE => defaultImportance
                                  | SOME i => i;
            fun makeProp v = QProperty.fromPair
                                 (Property.fromString (keypre ^ v),
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
                                        | NONE => ((fn s => [s]), key ^ "-");
            fun makeProp v = Property.fromString (keypre ^ v);
        in
            set' (map makeProp (valparser args))
        end;
in
    loadQorRSPropertiesFromFile sets parsers genProps filename
end;

fun computePsuedoQuestionTable qTable targetRSTable corrTable = let
    val ((qName, qRS), sourceProperties) = qTable;
    val (targetRSName, targetRSProperties) = targetRSTable;
    (* val propertyKinds = GenDict.keys (!qPropertyKeyMap); *)
in
    (("pseudo-" ^ qName, qRS), sourceProperties)
end;

end;
