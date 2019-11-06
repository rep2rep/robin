import "strategies.properties.tables";
import "strategies.properties.pattern";

signature COGNITIVE_PROPERTIES =
sig

  type qtable = QPropertySet.t QPropertySet.set;
  type rstable = PropertySet.t PropertySet.set;
  type corrtable = Correspondence.correspondence list;
  type userprofile;

  val modifyImportances : real -> qtable -> qtable;

  val numberOfTokens : qtable -> real;
  val varietyOfTokens : qtable -> real;

  val numberOfPatterns : qtable -> real;
  val varietyOfExpressions : qtable -> real;

  val subRSVariety : rstable -> real;

  val tokenRegistration : qtable -> real;

  val expressionRegistration : qtable -> rstable -> real;

  val tokenConceptMapping : qtable -> qtable -> real;
  val numberOfTokenTypes : qtable -> real;

  val expressionConceptMapping : qtable -> qtable -> real;
  val numberOfExpressionTypes : qtable -> real;

  val quantityScale : qtable -> real;

  val expressionComplexity : qtable (*-> rstable*) -> real;
  val arity : qtable -> real;
  val inferenceType : qtable -> real;

  val problemSpaceBranchingFactor : qtable -> rstable -> real;
  val solutionDepth : qtable -> real;
end;

structure CognitiveProperties : COGNITIVE_PROPERTIES =
struct

structure TableDict = Dictionary(struct
                                  type k = string * string;
                                  val compare =
                                      Comparison.join String.compare String.compare;
                                  val fmt =
                                      (fn (s, t) => "(" ^ s ^ ", " ^ t ^ ")");
                                  end);


type qtable = QPropertySet.t QPropertySet.set;
type rstable = PropertySet.t PropertySet.set;
type corrtable = Correspondence.correspondence list;
type userprofile = (string * real) list;

(* here u is assumed to be a real between 0 and 1, where 0 flattens everything
    (makes everything of High importance) and 1 leaves importance alone. Meant
    to represent users, with 0 being representative of a complete novice for
    whom importance is not discernible, while 1 is an expert for whom importance
    is perfectly discernible *)
fun modifyImportances u qT =
    let fun updImportance q =
            case QProperty.toPair q of (p,i) =>
                QProperty.fromPair (p, Importance.weight(1.0 + u*i - u))
    in QPropertySet.fromList (QPropertySet.map updImportance qT)
    end;


(* C and W are meant to be set per property/process, while T is meant to be set
   from the User Profile. *)
fun sigmoid C W T x = 1.0 - (1.0 / (1.0 + Math.pow(C,((x-T)/W))));

fun collectOfKindPresentInQ qS k = QPropertySet.filter (fn x => (#2 (Property.getNumFunction "occurrences" (QProperty.withoutImportance x)) > 0.0 handle Property.NoAttribute _ => false)
                                                         orelse (#2 (Property.getNumFunction "uses" (QProperty.withoutImportance x)) > 0.0 handle Property.NoAttribute _ => false))
                                                       (QPropertySet.collectOfKind qS k)

fun gravity x = (Importance.weight (QProperty.importanceOf x))
                  * #2 (Property.getNumFunction "occurrences" (QProperty.withoutImportance x))

fun logGravity x = (Importance.weight (QProperty.importanceOf x))
                  * Math.ln(1.0 + #2 (Property.getNumFunction "occurrences" (QProperty.withoutImportance x)))/Math.ln(2.0)

fun numberOfTokens qT =
    let val C = QPropertySet.toList (QPropertySet.collectOfKind qT Kind.Token)
    in List.sumIndexed (#2 o (Property.getNumFunction "occurrences") o QProperty.withoutImportance) C
    end;


fun varietyOfTokens qT =
    let val C = QPropertySet.toList (QPropertySet.collectOfKind qT Kind.Token)
        fun f x = if #2 (Property.getNumFunction "occurrences" (QProperty.withoutImportance x)) > 0.0 then 1.0 else 0.0
    in List.sumIndexed f C
    end;



fun numberOfTokenTypes qT =
    let val C = collectOfKindPresentInQ qT Kind.Token
        val T = QPropertySet.map (Property.getTypeOfValue o QProperty.withoutImportance) C
    in real (List.length (List.removeDuplicates T))
    end;


fun numberOfExpressionTypes qT =
    let val P = collectOfKindPresentInQ qT Kind.Pattern
        val C = collectOfKindPresentInQ qT Kind.Token
        fun getNonTrivialExpressionTypes [] = []
          | getNonTrivialExpressionTypes (x::X) =
            let val (typs,t) = Type.getInOutTypes (Property.getTypeOfValue x)
            in if null typs then getNonTrivialExpressionTypes X else t :: getNonTrivialExpressionTypes X
            end
        val Tc = getNonTrivialExpressionTypes (QPropertySet.map QProperty.withoutImportance C)
        val Tp = QPropertySet.map (Property.getTypeOfValue o QProperty.withoutImportance) P
    in real (List.length (List.removeDuplicates (Tc @ Tp)))
    end;


fun numberOfPatterns qT =
    let val P = QPropertySet.map QProperty.withoutImportance (collectOfKindPresentInQ qT Kind.Pattern)
        val C = QPropertySet.map QProperty.withoutImportance (collectOfKindPresentInQ qT Kind.Token)
        val nonTrivialC = List.filter (fn c => Pattern.arity c <> 0) C
    in List.sumIndexed (#2 o (Property.getNumFunction "occurrences")) (P @ nonTrivialC)
    end;



fun varietyOfExpressions qT =
    let val P = collectOfKindPresentInQ qT Kind.Pattern
    in real (QPropertySet.size P)
    end;


fun subRSVariety rT = real (PropertySet.size (PropertySet.collectOfKind rT Kind.Mode));


fun tokenRegistration qT =
    let val C = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Token)
        val P = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Pattern)
        val nonTrivialTokens = List.filter (fn c => Pattern.arity (QProperty.withoutImportance c) <> 0) C
        val patterns = P @ (map Pattern.fromQToken nonTrivialTokens)
        fun getReg c = #2 (Property.getNumFunction "token_registration" c) handle Property.NoAttribute _ => 1.0
        val patternNorm = List.sumIndexed logGravity patterns

        (* next functions takes a pattern, returns the pattern's tokens/hole types with the registration,
          modulated by the pattern's importance and occurrences*)
        fun tokensWithRegistration p =
            let val p' = QProperty.withoutImportance p
                val reg = Math.pow(2.0, getReg p' - 1.0) * (Importance.weight (QProperty.importanceOf p)) (*)* logGravity p / patternNorm*)
            in (Property.getTokens p', reg)
            end
        fun typesWithRegistration p =
            let val p' = QProperty.withoutImportance p
                val reg = Math.pow(2.0, getReg p' - 1.0) * (Importance.weight (QProperty.importanceOf p)) (*)* logGravity p / patternNorm*)
            in (Property.getHoles p', reg)
            end
        val tkregs = map tokensWithRegistration patterns
        val typregs = map typesWithRegistration patterns
        fun regOfTyp [] t = (1.0,0)
          | regOfTyp ((T,r)::L) t = if Property.contains T t
                                    then let val (ps,n) = regOfTyp L t in (r + ps,n+1) end
                                    else regOfTyp L t
        fun regOfTk' [] x = (0.0,0)
          | regOfTk' (([],_)::L) x = regOfTk' L x
          | regOfTk' ((s::S,r)::L) x = if Property.LabelOf x = s
                                       then let val (ps,n) = regOfTk' L x in (r + ps, n+1) end
                                       else regOfTk' ((S,r)::L) x;
        fun regOfTk L x = let val (sr,n) = regOfTk' L x
                          in if n > 0 then sr / real n
                             else (let val (sr',n') = regOfTyp typregs (Property.getTypeOfValue x)
                                    in (if n' > 0 then sr' / real n' else 2.0)
                                    end)
                              (* if token does not appear directly on patterns, but a hole with
                              the type of the token does, approximate it with that*)
                          end
        val importanceNorm = List.sumIndexed (fn x => Importance.weight (QProperty.importanceOf x)) C
        fun weighing tk = gravity tk / importanceNorm
        val totalSum = List.weightedSumIndexed gravity ((regOfTk tkregs) o QProperty.withoutImportance) C
    in totalSum
    end;


fun numberOfPatternsModulated qT =
    let val P = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Pattern)
        val C = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Token)
        val nonTrivialC = List.filter (fn c => Pattern.arity (QProperty.withoutImportance c) <> 0) C
        val importanceNorm = 1.0 + List.sumIndexed (fn x => Importance.weight (QProperty.importanceOf x)) P
        fun weighing x = gravity x / importanceNorm
    in List.sumIndexed gravity (P @ nonTrivialC)
    end;

fun numberOfPatternsModulatedSquared qT =
    let val P = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Pattern)
        val C = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Token)
        val nonTrivialC = List.filter (fn c => Pattern.arity (QProperty.withoutImportance c) <> 0) C
        val importanceNorm = 1.0 + List.sumIndexed (fn x => Importance.weight (QProperty.importanceOf x)) P
        fun weighing x = gravity x / importanceNorm
    in List.sumIndexed (fn x => Math.pow(gravity x,2.0)) (P @ nonTrivialC)
    end;

fun numberOfObjectsModulated qT =
    let val P = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Pattern)
        val C = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Token)
        val importanceNorm = 1.0 + List.sumIndexed (fn x => Importance.weight (QProperty.importanceOf x)) (P @ C)
        fun weighing x = gravity x / importanceNorm
    in List.sumIndexed gravity (P @ C)
    end;

fun expressionRegistration qT rT =
    let val M = PropertySet.toList (PropertySet.collectOfKind (QPropertySet.withoutImportances qT) Kind.Mode)
        fun modereg x = if Property.LabelOf x = "grid" then 1.0
                   else if Property.LabelOf x = "containment" then 1.0
                   else if Property.LabelOf x = "axial" then 2.0
                   else if Property.LabelOf x = "sentential" then 2.0
                   else if Property.LabelOf x = "connection" then 3.0
                   else if Property.LabelOf x = "proportional" then 3.0
                   else (print "unknown mode"; raise Match)
    in Math.sqrt(numberOfPatternsModulatedSquared qT) * (List.avgIndexed modereg M)
    end;


fun arity qT =
    let val P = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Pattern)
        val C = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Token)
        val n = numberOfTokens qT / numberOfTokenTypes qT
        fun ar p = let val i = Pattern.arity (QProperty.withoutImportance p)
                       val a = if i = ~3 then n else
                               if i = ~2 then Math.sqrt n else
                               if i = ~1 then Math.ln n else
                                  real i
                    in Math.pow(a,2.0)
                    end

        val nonTrivialTokens = List.filter (fn c => Pattern.arity (QProperty.withoutImportance c) <> 0) C
        val importanceNorm = 1.0 + List.sumIndexed (fn x => Importance.weight (QProperty.importanceOf x)) (nonTrivialTokens @ P)
        val patternNorm = numberOfPatternsModulated qT
        fun weighing x = gravity x / (importanceNorm * patternNorm)

    in Math.sqrt(List.weightedSumIndexed (fn x => Math.pow(logGravity x,2.0)) ar (nonTrivialTokens @ P))
    end


fun expressionComplexity qT =
    let val P = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Pattern)
        val C = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Token)
        val nonTrivialTokens = List.filter (fn c => Pattern.arity (QProperty.withoutImportance c) <> 0) C

        fun findAndUpdateByHoles p [] = [(p,[logGravity p])]
          | findAndUpdateByHoles p ((p',gs)::L) =
            if Property.sameHoles (QProperty.withoutImportance p, QProperty.withoutImportance p') andalso
               Property.sameTokens (QProperty.withoutImportance p, QProperty.withoutImportance p')
            then (p, (logGravity p :: gs)) :: L
            else (p', gs) :: findAndUpdateByHoles p L;
        fun clusterByHoles [] = []
          | clusterByHoles (p::L) = findAndUpdateByHoles p (clusterByHoles L);

        val clusteredPatterns = clusterByHoles ((map Pattern.fromQToken nonTrivialTokens) @ P)

        fun f (p,gs) =
            let val x = QProperty.withoutImportance p
                val _ = print ("\n   " ^ (Property.toString x))
                val (L,(d,b,complexity)) = Pattern.satisfyPattern (QProperty.withoutImportance p)
                                                                  (map QProperty.withoutImportance C)
                                                                  (map QProperty.withoutImportance P)
                                                handle Pattern.Unsatisfiable => ([],(1.0,1.0,1.0))
                val _ = print ("\n       length of final DNF: " ^ (Int.toString (length L)))
                val _ = print ("\n       complexity: " ^ (Real.toString complexity) ^ " ")
                val _ = print ("\n       depth: " ^ (Real.toString d) ^ " ")
                val _ = print ("\n       breadth: " ^ (Real.toString b) ^ " ")
            in  Math.pow(List.sum gs,2.0) * complexity
            end
(*
        val importanceNorm = 1.0 + List.sumIndexed (fn x => Importance.weight (QProperty.importanceOf x)) (nonTrivialTokens @ P)
        val patternNorm = Math.ln(1.0 + numberOfPatternsModulated qT)/Math.ln(2.0)
        fun weighing x = gravity x / (importanceNorm * patternNorm)
*)
    in Math.sqrt(List.sumIndexed f clusteredPatterns)
    end;


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

val corrT = let val corrpaths = filesMatchingPrefix "tables/" "correspondences_"
            in List.concat (map PropertyTables.loadCorrespondenceTable corrpaths)
            end;


fun quantityScale qT =
    let
        val arithpath = filesMatchingPrefix "tables/" "RS_table_realarith"
        val (_,arithT) = PropertyTables.loadRepresentationTable (hd arithpath)

        val corrT' = map (Correspondence.flip 1.0) corrT

        fun tp x = (QPropertySet.toList (PropertyTables.transformQProperty x arithT (corrT @ corrT')),
                    logGravity x handle Property.NoAttribute _ => 1.0)
        fun conc [] = []
          | conc ((t,n)::L) = (map (fn x => (x,n/(real (length t)))) t) @ conc L
        val pL = conc (QPropertySet.map tp qT)

        fun check x L = List.exists (fn y => (Property.LabelOf o QProperty.withoutImportance) x = y handle Property.Error _ => false) L
        fun ordinalCheck (x,_) = check x ["<",">","\\leq","\\geq","max","min","\\max","\\min"]
        fun intervalCheck (x,_) = check x ["+","-","sum","\\sum"]
        fun ratioCheck (x,_) = check x ["*","\\div","\\dvd","\\gcd","\\prod","\\lcm","/","^"]
        fun nominalCheck (x,n) = not (ordinalCheck (x,n) orelse intervalCheck (x,n) orelse ratioCheck (x,n))

        val ratio = List.filter ratioCheck pL
        val interval = List.filter intervalCheck pL
        val ordinal = List.filter ordinalCheck pL
        val nominal = List.filter nominalCheck pL

        val importanceNorm = 1.0 + List.sumIndexed (fn (x,_) => Importance.weight (QProperty.importanceOf x)) pL
        val objectNorm = Math.ln(1.0 + numberOfObjectsModulated qT)/Math.ln(2.0)
        fun f (x,n) = n / (importanceNorm * objectNorm)

        val s = 8.0*(List.sumIndexed #2 ratio)
              + 4.0*(List.sumIndexed #2 interval)
              + 2.0*(List.sumIndexed #2 ordinal)
              + 1.0*(List.sumIndexed #2 nominal)
    in s
    end;


fun conceptMapping kind idealqT qT =
    let fun present x = #2 (Property.getNumFunction "occurrences" (QProperty.withoutImportance x)) > 0.0

        fun findAndInsertByImportance x [] = [(QProperty.importanceOf x, [QProperty.withoutImportance x])]
          | findAndInsertByImportance x ((i,l)::L) =
            if Importance.equal(i,QProperty.importanceOf x)
            then (i,QProperty.withoutImportance x::l) :: L
            else (i,l) :: findAndInsertByImportance x L
        fun clusterByImportance [] = []
          | clusterByImportance (x::L) = findAndInsertByImportance x (clusterByImportance L)

        val qT' = QPropertySet.filter (fn x => Property.kindOf (QProperty.withoutImportance x) = kind andalso present x) qT
        val clusteredqT = clusterByImportance (QPropertySet.toList qT')
        val clusteredIdealqT = clusterByImportance (QPropertySet.toList idealqT)

        val corrT' = map (Correspondence.flip 1.0) corrT

        fun assess_rd p = (* concepts to objects *)
            let fun transformToCluster (imp,X) =
                    (imp,QPropertySet.toList (PropertyTables.transformQProperty p (PropertySet.fromList X) (corrT @ corrT')))
                val T = map transformToCluster clusteredqT
                val x = length (List.concat (map #2 T))
                val i = List.sumIndexed (fn (imp,L) => imp * Math.ln(real (1 + length L))/Math.ln(2.0)) T
                val s = (if x = 1 (* functional *) then 0.0 else
                         if x > 1 (* redundancy *) then 2.0 * i else
                         if x < 1 (* deficit *) then 3.0 else raise Match)
            in s * (Importance.weight (QProperty.importanceOf p))
            end;

        fun assess_oe p = (* objects to concepts *)
            let fun transformToCluster (imp,X) =
                    (imp,QPropertySet.toList (PropertyTables.transformQProperty p (PropertySet.fromList X) (corrT @ corrT')))
                val T = map transformToCluster clusteredIdealqT
                val x = length (List.concat (map #2 T))
                val i = List.sumIndexed (fn (imp,L) => imp * Math.ln(real (1 + length L))/Math.ln(2.0)) T
                val s = (if x = 1 (* injetive & surjective *) then 0.0 else
                         if x > 1 (* overload *) then 4.0 * i else
                         if x < 1 (* excess *) then 1.0 else raise Match)
            in s * (Importance.weight (QProperty.importanceOf p))
            end;

        val rd = List.sumIndexed assess_rd (QPropertySet.toList idealqT)
        val oe = (List.avgIndexed assess_oe (QPropertySet.toList qT')) * (real (QPropertySet.size idealqT))
(*)
        fun globalOE i = (List.avgIndexed (assess_oe) clusteredqT) * real (QPropertySet.size X) handle Empty => 0.0
        val oeL = map globalOE clusteredIdealqT*)

    in rd + oe
    end;

fun tokenConceptMapping idealqT rT = conceptMapping Kind.Token idealqT rT;

fun expressionConceptMapping idealqT rT = conceptMapping Kind.Pattern idealqT rT;


fun inferenceType qT =
    let val T = collectOfKindPresentInQ qT Kind.Tactic;
        fun wt x = (#2 (Property.getNumFunction "uses" (QProperty.withoutImportance x)) * (Importance.weight (QProperty.importanceOf x)),
                    #2 (Property.getStringFunction "inference_type" (QProperty.withoutImportance x)))
        val S = QPropertySet.map wt T
        fun assess (_,s) = if s = "assign" then 1.0
                  else if s = "match" then 2.0
                  else if s = "subst" then 4.0
                  else if s = "calc" then 8.0
                  else if s = "transformation" then 16.0
                  else (print ("Cannot find inference type: " ^ s ^ "\n") ;raise Match)
    in List.weightedAvgIndexed (#1) assess S handle Empty => Real.posInf
    end;


fun problemSpaceBranchingFactor qT rT =
    let val T = PropertySet.collectOfKind (QPropertySet.withoutImportances qT) Kind.Tactic;
        val L = PropertySet.collectOfKind (QPropertySet.withoutImportances qT) Kind.Law;
        fun lawParams t = #2 (Property.getNumFunction "laws" t) handle Property.NoAttribute _ => 0.0
        fun patternParams t = #2 (Property.getNumFunction "patterns" t) handle Property.NoAttribute _ => 0.0
        val bl = PropertySet.size L
        val bp = numberOfPatternsModulated qT
        fun dotProduct [] [] = 0.0
          | dotProduct (h::t) (h'::t') = (h * h') + dotProduct t t'
          | dotProduct  _ _ = raise Match;
        val l = PropertySet.map (fn x => Math.pow(real bl, lawParams x)) T
        val p = PropertySet.map (fn x => Math.pow(bp, patternParams x)) T
        val result = dotProduct l p
    in if Real.==(result,0.0) then Real.posInf else result
    end;

fun solutionDepth qT =
    let val T = PropertySet.toList (PropertySet.collectOfKind (QPropertySet.withoutImportances qT) Kind.Tactic);
        val occs = List.sumIndexed (fn t => #2 (Property.getNumFunction "uses" t) handle Property.NoAttribute _ => 0.0) T
        val trans = List.exists (fn t => #2 (Property.getStringFunction "inference_type" t) = "transformation") T
    in if trans then Real.posInf else occs
    end;

end;
