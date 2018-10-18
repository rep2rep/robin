import "util/set.sml";
import "util/dictionary.sml";
import "util/csv.sml";
import "util/logging.sml";

signature PROPERTYTABLES =
sig
    exception TableError;

    structure S : SET;
    structure D : DICTIONARY;

    type correspondence = ((S.t S.set * S.t S.set) * (S.t S.set * S.t S.set) * real);

    val loadCorrespondenceTable : string -> correspondence list;
    val loadQuestionTable : string -> (D.k, S.t S.set) D.dict;
    val loadRepresentationTable : string -> (D.k, S.t S.set) D.dict;

end;


structure PropertyTables : PROPERTYTABLES =
struct

exception TableError;

structure S = Set(struct
                   type t = string;
                   val compare = String.compare;
                   val fmt = fn s => s;
                   end);
val cmp = fn a => fn b => String.compare(a, b);
val set' = S.fromList;
val insert' = S.insert;
val subset' = S.subset;

structure D = Dictionary(struct
                          type k = string;
                          val compare = String.compare;
                          end);
val dict' = D.fromPairList;
fun getValue d k = D.get k d;

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
                          | SOME other => raise TableError
                          | NONE => (disjtree, [])
                    end
                  | SOME other => (Prop other, (List.tl tokens))
                  | NONE => raise TableError
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
                  | NONE => raise TableError
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
                if (List.null tokens) then tree else raise TableError
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

        fun setify (Prop s) = (set' [s], S.empty)
          | setify (Neg (Prop s)) = (S.empty, set' [s])
          | setify (Neg _) = raise TableError
          | setify (Conj (a, b)) =
            let
                fun ConjFlatten (Prop s) = [Prop s]
                  | ConjFlatten (Neg a) = [Neg a]
                  | ConjFlatten (Conj (a, b)) = (ConjFlatten a) @ (ConjFlatten b)
                  | ConjFlatten (Disj _) = raise TableError;
                fun isPos (Prop s) = true
                  | isPos _ = false;
                fun isNeg (Neg a) = true
                  | isNeg _ = false;
                fun stripTreeness (Prop s) = s
                  | stripTreeness (Neg (Prop s)) = s
                  | stripTreeness _ = raise TableError;
                val flattened = ConjFlatten (Conj (a, b));
                val positives = set' (map stripTreeness (List.filter isPos flattened));
                val negatives = set' (map stripTreeness (List.filter isNeg flattened));
            in
                (positives, negatives)
            end
          | setify (Disj _) = raise TableError; (* Should be eliminated by now *)

        val read = (normalise o parse o tokenize);

        fun strength str =
            case (Real.fromString str) of
                SOME v => v
              | NONE => raise TableError;

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
        fun makeRow row = case row of
                              [x, y] => (readCorrespondence x y "1.0")
                            | [x, y, z] => (readCorrespondence x y z)
                            | _ => raise TableError;
        val _ = Logging.write ("LOAD " ^ filename ^ "\n");
        val csvFile = CSVDefault.openIn filename;
        val csvData = CSVDefault.input csvFile;
    in
        List.foldr (fn (r, xs) => (makeRow r) @ xs) [] csvData
    end
    handle IO.Io e => (print ("ERROR: File '" ^ filename ^ "' could not be loaded\n");
                       raise (IO.Io e))
         | TableError => (
             Logging.write ("ERROR: CSV parsing failed in file '" ^ filename ^ "'\n");
             raise TableError
         );


fun loadQuestionTable filename = D.empty;
fun loadRepresentationTable filename = D.empty;

end;
