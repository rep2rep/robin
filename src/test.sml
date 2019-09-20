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


fun subRSVariety_rank qL =
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.subRSVariety (#2(loadRS r)))
        fun display (((_,r),_),v) = r ^ ": " ^ (Real.toString v) ^ "\n"
        val sorted = List.mergesort (fn (x,y) => Real.compare (#2 x, #2 y)) (map f qL)
    in (print (String.concat (map display sorted)); sorted)
    end;

fun tokenRegistration_rank qL =
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.tokenRegistration x)
        fun display (((_,r),_),v) = r ^ ": " ^ (Real.toString v) ^ "\n"
        val sorted = List.mergesort (fn (x,y) => Real.compare (#2 x, #2 y)) (map f qL)
    in (print (String.concat (map display sorted)); sorted)
    end;

fun expressionRegistration_rank qL =
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.expressionRegistration x (#2(loadRS r)))
        fun display (((_,r),_),v) = r ^ ": " ^ (Real.toString v) ^ "\n"
        val sorted = List.mergesort (fn (x,y) => Real.compare (#2 x, #2 y)) (map f qL)
    in (print (String.concat (map display sorted)); sorted)
    end;

fun tokenConceptMapping_rank qL =
    let val ((q,_),_) = hd qL
        val bayesian = #2(loadQ q "bayes")
        fun f ((q,r),x) = (((q,r),x), CognitiveProperties.tokenConceptMapping bayesian (#2(loadRS r)))
        fun display (((_,r),_),v) = r ^ ": " ^ (Real.toString v) ^ "\n"
        val sorted = List.mergesort (fn (x,y) => Real.compare (#2 x, #2 y)) (map f qL)
    in (print (String.concat (map display sorted)); sorted)
    end;

fun expressionConceptMapping_rank qL =
    let val ((q,_),_) = hd qL
        val bayesian = #2(loadQ q "bayes")
        fun f ((q,r),x) = (((q,r),x), CognitiveProperties.expressionConceptMapping bayesian (#2(loadRS r)))
        fun display (((_,r),_),v) = r ^ ": " ^ (Real.toString v) ^ "\n"
        val sorted = List.mergesort (fn (x,y) => Real.compare (#2 x, #2 y)) (map f qL)
    in (print (String.concat (map display sorted)); sorted)
    end;

fun numberOfTokenTypes_rank qL =
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.numberOfTokenTypes x)
        fun display (((_,r),_),v) = r ^ ": " ^ (Real.toString v) ^ "\n"
        val sorted = List.mergesort (fn (x,y) => Real.compare (#2 x, #2 y)) (map f qL)
    in (print (String.concat (map display sorted)); sorted)
    end;

fun numberOfExpressionTypes_rank qL =
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.numberOfExpressionTypes x)
        fun display (((_,r),_),v) = r ^ ": " ^ (Real.toString v) ^ "\n"
        val sorted = List.mergesort (fn (x,y) => Real.compare (#2 x, #2 y)) (map f qL)
    in (print (String.concat (map display sorted)); sorted)
    end;

fun quantityScale_rank qL =
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.quantityScale x)
        fun display (((_,r),_),v) = r ^ ": " ^ (Real.toString v) ^ "\n"
        val sorted = List.mergesort (fn (x,y) => Real.compare (#2 x, #2 y)) (map f qL)
    in (print (String.concat (map display sorted)); sorted)
    end;

fun expressionComplexity_rank qL =
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.expressionComplexity x)
        fun display (((_,r),_),v) =  r ^ ": " ^ (Real.toString v) ^ "\n"
        val sorted = List.mergesort (fn (x,y) => Real.compare (#2 x, #2 y)) (map f qL)
    in (print (String.concat (map display sorted)); sorted)
    end;

fun problemSpaceBranchingFactor_rank qL =
    let fun f ((q,r),x) = (((q,r),x), CognitiveProperties.problemSpaceBranchingFactor x (#2(loadRS r)))
        fun display (((_,r),_),v) = r ^ ": " ^ (Real.toString v) ^ "\n"
        val sorted = List.mergesort (fn (x,y) => Real.compare (#2 x, #2 y)) (map f qL)
    in (print (String.concat (map display sorted)); sorted)
    end;

val B = loadQs "birds";

val _ = (print "subRS variety: \n";
         subRSVariety_rank B; print "\n\n";
         print "token registration: \n";
         tokenRegistration_rank B; print "\n\n";
         print "expression registration: \n";
         expressionRegistration_rank B; print "\n\n";
         print "token-concept mapping: \n";
         tokenConceptMapping_rank B; print "\n\n";
         print "expression-concept mapping: \n";
         expressionConceptMapping_rank B; print "\n\n";
         print "number of token types: \n";
         numberOfTokenTypes_rank B; print "\n\n";
         print "number of expression types: \n";
         numberOfExpressionTypes_rank B; print "\n\n";
         print "quantity scale: \n";
         quantityScale_rank B; print "\n\n";
         print "expression complexity: \n";
         expressionComplexity_rank B; print "\n\n";
         print "problem-space branching factor: \n";
         problemSpaceBranchingFactor_rank B; print "\n")
