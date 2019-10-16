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
                       if r = "euler" then 6 else
                       if r = "setalgebra" then 7 else
                       if r = "expeuler" then 8 else
                       if r = "expnatlang" then 9 else raise Match;
        val (((_,rx),_),_) = x
        val (((_,ry),_),_) = y
    in Int.compare (numify rx, numify ry)
    end;

fun crunch_raw L =
    let val max = #2 (List.argmax (fn x => if Real.==(#2 x,Real.posInf) then Real.negInf else #2 x) L)
        val min = #2 (List.argmin (fn x => if Real.==(#2 x,Real.negInf) then Real.posInf else #2 x) L)
        val normL = map (fn (x,v) => (x, if Real.== (v, Real.posInf)
                                          then 1.0 * (max+0.01)
                                          else if Real.== (v, Real.negInf)
                                                then 1.0 * (min-0.01)
                                                else  1.0 * v)) L
        val sorted = List.mergesort RS_order normL
    in sorted
    end;

fun crunch_norm L =
    let val max = #2 (List.argmax #2 L)
        val min = #2 (List.argmin #2 L)
        val normL = map (fn (x,v) => (x, 1.0 * (v - min) / (max - min))) L
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
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.subRSVariety (QPropertySet.withoutImportances x) (*(#2(loadRS r))*))
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
        fun f ((q,r),x) = (((q,r),x), CognitiveProperties.tokenConceptMapping bayesian (QPropertySet.withoutImportances x) (*(#2(loadRS r))*))
    in crunch (map f qL)
    end;

fun expressionConceptMapping_score qL crunch=
    let val ((q,_),_) = hd qL
        val bayesian = #2(loadQ q "bayes")
        fun f ((q,r),x) = (((q,r),x), CognitiveProperties.expressionConceptMapping bayesian (QPropertySet.withoutImportances x) (*(#2(loadRS r))*))
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
    let fun f ((q,r),x) = let val _ = print ("\n" ^ r ^ "... ")
                              val v = CognitiveProperties.expressionComplexity x
                              val _ = print ("\n")
                          in (((q,r),x), v)
                          end;
    in crunch (map f qL)
    end;

fun arity_score qL crunch=
    let fun f ((q,r),x) = let val v = CognitiveProperties.arity x
                          in (((q,r),x), v)
                          end;
    in crunch (map f qL)
    end;

fun inferenceType_score qL crunch=
    let fun f ((q,r),x) = let val v = CognitiveProperties.inferenceType x
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
fun printNumber r = Real.fmt (StringCvt.FIX (SOME 6)) r handle Domain => "NaN";

fun cognitiveScores_latex qL crunch =
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
        val forLatex = (String.concat ("\n & \\textbf{":: List.intersperse "} & \\textbf{" rss) ^ "} \\\\ \\hline \n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{token registration}":: map printNumber c1)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{expression registration}":: map printNumber c2)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{token-concept mapping}":: map printNumber c3)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{expression-concept mapping}":: map printNumber c4)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{number of token types}":: map printNumber c5)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{number of expression types}":: map printNumber c6)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{quantity scale}" :: map printNumber c7)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{expression complexity}":: map printNumber c8)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{subRS variety}":: map printNumber c9)) ^ " \\\\ \\hline\n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{problem-space branching factor}":: map printNumber c10)) ^ " \\\\ \\hline\\hline \n") ^
                        (String.concat (List.intersperse " & " ("\\textbf{Total}" :: map printNumber totals)) ^ " \\\\ \n")
    in (print forLatex)
    end;

fun cognitiveScores qL crunch =
    let val w1 = 1.0
        val w2 = 2.0
        val w3 = 4.0
        val u1 = 0.5
        val u2 = 1
        val u3 = 2
        val rss = map (fn (((_,r),_),_) => r) (dummy_rank qL)
        val c1 = map (fn (_,v) => v) (tokenRegistration_score qL crunch)
        val c2 = map (fn (_,v) => v) (expressionRegistration_score qL crunch)
        val c3 = map (fn (_,v) => v) (tokenConceptMapping_score qL crunch)
        val c4 = map (fn (_,v) => v) (expressionConceptMapping_score qL crunch)
        val c5 = map (fn (_,v) => v) (numberOfTokenTypes_score qL crunch)
        val c6 = map (fn (_,v) => v) (numberOfExpressionTypes_score qL crunch)
        val c7 = map (fn (_,v) => v) (quantityScale_score qL crunch)
        val c8 = map (fn (_,v) => v) (expressionComplexity_score qL crunch)
        val c9 = map (fn (_,v) => v) (arity_score qL crunch)
        val c10 = map (fn (_,v) => v) (inferenceType_score qL crunch)
        val c11 = map (fn (_,v) => v) (subRSVariety_score qL crunch)
        val c12 = map (fn (_,v) => v) (problemSpaceBranchingFactor_score qL crunch)
        val totals = Vect.vectorSum [c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12]
        val forLatex = (String.concat ("\n , , ," :: List.intersperse " , " rss) ^ "   \n") ^
                        (String.concat (List.intersperse " , " ("token registration , ," :: map printNumber c1)) ^ "  \n") ^
                        (String.concat (List.intersperse " , " ("expression registration , ," :: map printNumber c2)) ^ "  \n") ^
                        (String.concat (List.intersperse " , " ("token-concept mapping , ," :: map printNumber c3)) ^ "  \n") ^
                        (String.concat (List.intersperse " , " ("expression-concept mapping , ," :: map printNumber c4)) ^ "  \n") ^
                        (String.concat (List.intersperse " , " ("number of token types , ," :: map printNumber c5)) ^ "  \n") ^
                        (String.concat (List.intersperse " , " ("number of expression types , ," :: map printNumber c6)) ^ "  \n") ^
                        (String.concat (List.intersperse " , " ("quantity scale , ,":: map printNumber c7)) ^ "  \n") ^
                        (String.concat (List.intersperse " , " ("expression complexity , ,":: map printNumber c8)) ^ "  \n") ^
                        (String.concat (List.intersperse " , " ("arity , ,":: map printNumber c9)) ^ "  \n") ^
                        (String.concat (List.intersperse " , " ("inference type , ,":: map printNumber c10)) ^ "  \n") ^
                        (String.concat (List.intersperse " , " ("subRS variety , ,":: map printNumber c11)) ^ "  \n") ^
                        (String.concat (List.intersperse " , " ("problem-space branching factor , ,":: map printNumber c12)) ^ "   \n")
                    (*    (String.concat (List.intersperse " , " ("Total" :: "" :: map printNumber totals)) ^ "  \n")*)
    in (print forLatex)
    end;

val B = loadQs "birds";
val C = QPropertySet.map QProperty.withoutImportance (QPropertySet.collectOfKind (#2 (List.nth (B,0))) Kind.Token);
val C' = List.filter (fn x => #2 (Property.getNumFunction "occurrences" x) > 0.0) C
val P = QPropertySet.map QProperty.withoutImportance (QPropertySet.collectOfKind (#2 (List.nth (B,0))) Kind.Pattern);
val P' = List.filter (fn x => #2 (Property.getNumFunction "occurrences" x) > 0.0) P
val p1 = List.nth (P,0);
val p2 = List.nth (P,1);
val p3 = List.nth (P,2);


val _ = print "\n"
val _ = cognitiveScores B crunch_raw;
(*)
val S = quantityScale_score B crunch_raw;*)
