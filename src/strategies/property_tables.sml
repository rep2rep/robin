import "util.set";
import "util.dictionary";
import "util.csv";
import "util.logging";

signature PROPERTYTABLES =
sig
    exception TableError of string;

    structure S : SET;
    structure D : DICTIONARY;

    type correspondence = ((S.t S.set * S.t S.set) * (S.t S.set * S.t S.set) * real);

    val loadCorrespondenceTable : string -> correspondence list;
    val loadQuestionTable : string -> (D.k, S.t S.set) D.dict;
    val loadRepresentationTable : string -> (D.k, S.t S.set) D.dict;

    val setGenerator : (string * ((string -> string list) * string)) -> unit;
    val setGenerators : (string * ((string -> string list) * string)) list -> unit;

end;


structure PropertyTables : PROPERTYTABLES =
struct

exception TableError of string;

structure S = Set(struct
                   type t = string;
                   val compare = String.compare;
                   val fmt = fn s => s;
                   end);
val set' = S.fromList;

structure D = Dictionary(struct
                          type k = string;
                          val compare = String.compare;
                          end);
val dict' = D.fromPairList;
fun getValue d k = SOME (D.get d k)
                   handle D.KeyError => NONE;

structure CSVLiberal = CSVIO(struct val delimiters = [#","];
                                    val newlines = ["\r", "\n", "\r\n"];
                             end);

type correspondence = ((S.t S.set * S.t S.set) * (S.t S.set * S.t S.set) * real);

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

        fun setify (Prop s) = (set' [s], S.empty ())
          | setify (Neg (Prop s)) = (S.empty (), set' [s])
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
                fun stripTreeness (Prop s) = s
                  | stripTreeness (Neg (Prop s)) = s
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
        val _ = Logging.write ("LOAD " ^ filename ^ "\n");
        val csvFile = CSVLiberal.openIn filename;
        val csvData = CSVLiberal.input csvFile;
    in
        List.foldr (fn (r, xs) => (makeRow r) @ xs) [] csvData
    end
    handle IO.Io e => (print ("ERROR: File '" ^ filename ^ "' could not be loaded\n");
                       raise (IO.Io e))
         | TableError reason => (
             print ("ERROR: CSV parsing failed in file '" ^ filename ^ "'\n");
             print ("       " ^ reason ^ "\n");
             raise TableError reason
         );

(*
We provide a way to extend the known set of property generators with custom
generation functions. These take the form of (property-key, (generator, prefix))
where property-key and prefix are strings, and generator is a function.
To add them, use either setGenerators with a list, or setGenerator with a tuple.
Avoid running map over setGenerator, as it is faster to use the predefined 'plural'.
*)
val propertyKeyMap = ref (D.empty);
fun setGenerators new =
    let
        val _ = propertyKeyMap := D.union (dict' new) (!propertyKeyMap);
    in () end;
fun setGenerator new =
    let
        val _ = propertyKeyMap := D.insert (!propertyKeyMap) new;
    in () end;

fun loadQorRSPropertiesFromFile filename =
    let
        val _ = Logging.write ("LOAD " ^ filename ^ "\n");
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

        fun parseRow [x, y] = (x, y)
          | parseRow _ = raise TableError "Malformed property entry";

        fun genProps key args =
            let
                val (valparser, keypre) =
                    case (getValue (!propertyKeyMap) key) of
                        SOME kt => kt
                      | NONE => ((fn s => [s]), key ^ "-");
            in
                map (fn v => keypre ^ v) (valparser args)
            end;

        val properties = List.foldr
                             (fn ((k, v), xs) => S.union (set' (genProps k v)) xs)
                             (S.empty ())
                             (map parseRow csvData);
    in
        [(csvHeader, properties)]
    end
    handle IO.Io e => (print ("ERROR: File '" ^ filename ^ "' could not be loaded\n");
                       raise (IO.Io e))
         | TableError reason => (
             print ("ERROR: CSV parsing failed in file '" ^ filename ^ "'\n");
             print ("       " ^ reason ^ "\n");
             raise TableError reason
         );

fun loadQuestionTable filename = dict' (loadQorRSPropertiesFromFile filename);
fun loadRepresentationTable filename = dict' (loadQorRSPropertiesFromFile filename);

end;
