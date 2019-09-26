import "strategies.representation_selection";


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

fun loadQs problem =
    let val paths = filesMatchingPrefix "tables/" ("Q_table_" ^ problem)
    in map PropertyTables.loadQuestionTable paths
    end;

fun loadQ q rs =
    let val paths = filesMatchingPrefix "tables/" ("Q_table_" ^ q ^ "_" ^ rs)
        val L = map PropertyTables.loadQuestionTable paths
    in if length L > 1 then raise Fail "ambiguous Q" else
       if length L = 0 then raise Fail ("no Q with this name: " ^ ("Q_table_" ^ q ^ rs)) else
         hd L
    end;

fun loadRS rs =
    let val paths = filesMatchingPrefix "tables/" ("RS_table_" ^ rs)
        val L = map PropertyTables.loadRepresentationTable paths
    in if length L > 1 then raise Fail "ambiguous RS" else
       if length L = 0 then raise Fail "no RS with this name" else
         hd L
    end;

fun RS_order (x,y) =
    let fun numify r = if r = "natlang" then 1 else
                       if r = "bayes" then 2 else
                       if r = "geometric" then 3 else
                       if r = "contingency" then 4 else
                       if r = "probabilitytrees" then 5 else
                       if r = "euler" then 6 else raise Match;
        val (((_,rx),_),_) = x
        val (((_,ry),_),_) = y
    in Int.compare (numify rx, numify ry)
    end;

fun crunch_raw L =
    let val normL = map (fn (x,v) => (x, 1000.0 * v)) L
        val sorted = List.mergesort RS_order normL
    in sorted
    end;

fun crunch_norm L =
    let val max = #2 (List.argmax #2 L)
        val min = #2 (List.argmin #2 L)
        val normL = map (fn (x,v) => (x, 1000.0 * (v - min) / (max - min))) L
        val sorted = List.mergesort RS_order normL
    in sorted
    end;

fun crunch_rank L =
    let val sorted = List.mergesort (fn (x,y) => Real.compare (#2 x, #2 y)) L
        fun rank _ [] = [] (* replaces score with rank *)
          | rank i ((x,_)::t) = (x,real i) :: rank (i+1) t
    in List.mergesort RS_order (rank 1 sorted)
    end;

fun dummy_rank qL = List.mergesort RS_order (map (fn x => (x,0.0)) qL)

fun subRSVariety_score qL crunch=
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.subRSVariety (#2(loadRS r)))
    in crunch (map f qL)
    end;

fun tokenRegistration_score qL crunch=
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.tokenRegistration x)
    in crunch (map f qL)
    end;

fun expressionRegistration_score qL crunch=
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.expressionRegistration x (#2(loadRS r)))
    in crunch (map f qL)
    end;

fun tokenConceptMapping_score qL crunch=
    let val ((q,_),_) = hd qL
        val bayesian = #2(loadQ q "bayes")
        fun f ((q,r),x) = (((q,r),x), CognitiveProperties.tokenConceptMapping bayesian (QPropertySet.withoutImportances x) (*)(#2(loadRS r))*))
    in crunch (map f qL)
    end;

fun expressionConceptMapping_score qL crunch=
    let val ((q,_),_) = hd qL
        val bayesian = #2(loadQ q "bayes")
        fun f ((q,r),x) = (((q,r),x), CognitiveProperties.expressionConceptMapping bayesian (QPropertySet.withoutImportances x) (*)(#2(loadRS r))*))
    in crunch (map f qL)
    end;

fun numberOfTokenTypes_score qL crunch=
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.numberOfTokenTypes x)
    in crunch (map f qL)
    end;

fun numberOfExpressionTypes_score qL crunch=
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.numberOfExpressionTypes x)
    in crunch (map f qL)
    end;

fun quantityScale_score qL crunch=
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.quantityScale x)
    in crunch (map f qL)
    end;

fun expressionComplexity_score qL crunch=
    let fun f ((q,r),x) = let val v = CognitiveProperties.expressionComplexity x
                          in (((q,r),x), v)
                          end;
    in crunch (map f qL)
    end;

fun problemSpaceBranchingFactor_score qL crunch=
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.problemSpaceBranchingFactor x (#2(loadRS r)))
    in crunch (map f qL)
    end;

signature VECT =
sig
  val vectorPlus : real list -> real list -> real list;
  val vectorSum : real list list -> real list
end;

structure Vect : VECT =
struct
fun vectorPlus [] [] = []
  | vectorPlus (x1::L1) (x2::L2) = (x1 + x2) :: vectorPlus L1 L2
  | vectorPlus _ _ = raise Match;

fun vectorSum [] = raise Match
  | vectorSum [v] = v
  | vectorSum (v::L) = vectorPlus v (vectorSum L);
end;

fun printAsInteger r = Int.toString (Real.toInt IEEEReal.TO_NEAREST r) handle Domain => "NaN";

fun cognitiveScores qL crunch =
    let val w1 = 1.0
        val w2 = 2.0
        val w3 = 4.0
        val rss = map (fn (((_,r),_),_) => r) (dummy_rank qL)
        val c1 = map (fn (_,v) => w1 * v) (tokenRegistration_score qL crunch)
        val c2 = map (fn (_,v) => w1 * v) (expressionRegistration_score qL crunch)
        val c3 = map (fn (_,v) => w1 * v) (tokenConceptMapping_score qL crunch)
        val c4 = map (fn (_,v) => w1 * v) (expressionConceptMapping_score qL crunch)
        val c5 = map (fn (_,v) => w1 * v) (numberOfTokenTypes_score qL crunch)
        val c6 = map (fn (_,v) => w1 * v) (numberOfExpressionTypes_score qL crunch)
        val c7 = map (fn (_,v) => w2 * v) (quantityScale_score qL crunch)
        val c8 = map (fn (_,v) => w2 * v) (expressionComplexity_score qL crunch)
        val c9 = map (fn (_,v) => w3 * v) (subRSVariety_score qL crunch)
        val c10 = map (fn (_,v) => w3 * v) (problemSpaceBranchingFactor_score qL crunch)
        val totals = Vect.vectorSum [c1,c2,c3,c4,c5,c6,c7,c8,c9,c10]
        val forLatex = (String.concat ("\n & w & \\textbf{":: List.intersperse "} & \\textbf{" rss) ^ "} \\\\ \\hline \n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{token registration}" :: (printAsInteger w1) :: map printAsInteger c1)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{expression registration}" :: (printAsInteger w1) :: map printAsInteger c2)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{token-concept mapping}" :: (printAsInteger w1) :: map printAsInteger c3)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{expression-concept mapping}" :: (printAsInteger w1) :: map printAsInteger c4)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{number of token types}" :: (printAsInteger w1) :: map printAsInteger c5)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{number of expression types}" :: (printAsInteger w1) :: map printAsInteger c6)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{quantity scale}" :: (printAsInteger w2) :: map printAsInteger c7)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{expression complexity}" :: (printAsInteger w2) :: map printAsInteger c8)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{subRS variety}" :: (printAsInteger w3) :: map printAsInteger c9)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{problem-space branching factor}" :: (printAsInteger w3) :: map printAsInteger c10)) ^ " \\\\ \\hline\\hline \n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{Total}" :: "" :: map printAsInteger totals)) ^ " \\\\ \n")
    in (print forLatex)
    end;

val B = loadQs "birds";
val C = QPropertySet.map QProperty.withoutImportance (QPropertySet.collectOfKind (#2 (List.nth (B,1))) Kind.Token);
val P = QPropertySet.map QProperty.withoutImportance (QPropertySet.collectOfKind (#2 (List.nth (B,1))) Kind.Pattern);
val p = List.nth (P,1);

(*)
val _ = cognitiveScores B crunch_norm;
val _ = print "\n"
val _ = cognitiveScores B crunch_rank;*)

val S = quantityScale_score B crunch_raw;
