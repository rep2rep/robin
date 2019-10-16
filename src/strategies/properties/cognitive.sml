import "strategies.properties.tables";
import "strategies.properties.pattern";

signature COGNITIVE_PROPERTIES =
sig

  type qtable = QPropertySet.t QPropertySet.set;
  type rstable = PropertySet.t PropertySet.set;
  type corrtable = Correspondence.correspondence list;
  type userprofile;

  val numberOfTokens : qtable -> real;
  val varietyOfTokens : qtable -> real;

  val numberOfPatterns : qtable -> real;
  val varietyOfExpressions : qtable -> real;

  val subRSVariety : rstable -> real;

  val tokenRegistration : qtable -> real;

  val expressionRegistration : qtable -> rstable -> real;

  val tokenConceptMapping : qtable -> rstable -> real;
  val numberOfTokenTypes : qtable -> real;

  val expressionConceptMapping : qtable -> rstable -> real;
  val numberOfExpressionTypes : qtable -> real;

  val quantityScale : qtable -> real;

  val expressionComplexity : qtable (*-> rstable*) -> real;
  val arity : qtable -> real;
  val inferenceType : qtable -> real;

  val problemSpaceBranchingFactor : qtable -> rstable -> real;
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


(* C and W are meant to be set per property/process, while T is meant to be set
   from the User Profile. *)
fun sigmoid C W T x = 1.0 - (1.0 / (1.0 + Math.pow(C,((x-T)/W))));

fun collectOfKindPresentInQ qS k = QPropertySet.filter (fn x => #2 (Property.getNumFunction "occurrences" (QProperty.withoutImportance x)) > 0.0) (QPropertySet.collectOfKind qS k)


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

fun numberOfPatternsModulated qT =
    let val P = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Pattern)
        val C = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Token)
        val nonTrivialC = List.filter (fn c => Pattern.arity (QProperty.withoutImportance c) <> 0) C
    in List.sumIndexed (fn x => (Importance.weight (QProperty.importanceOf x)) * #2 (Property.getNumFunction "occurrences" (QProperty.withoutImportance x))) (P @ nonTrivialC)
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
        fun tokensWithRegistration x =
            let val c = QProperty.withoutImportance x
                val tks = Property.getTokens c handle Property.NoAttribute _ => [Property.LabelOf c]
                val i = Importance.weight (QProperty.importanceOf x)
                val reg = i * #2 (Property.getNumFunction "token_registration" c) handle Property.NoAttribute _ => i
            in (tks,reg)
            end
        fun typesWithRegistration x =
            let val c = QProperty.withoutImportance x
                val typs = Property.getHoles c handle Property.NoAttribute _ => Property.HolesfromList (#1 (Type.getInOutTypes (Property.getTypeOfValue c)))
                val i = Importance.weight (QProperty.importanceOf x)
                val reg = i * #2 (Property.getNumFunction "token_registration" c) handle Property.NoAttribute _ => i
            in (typs,reg)
            end
        val tkregs = map tokensWithRegistration (P @ nonTrivialTokens)
        val typregs = map typesWithRegistration (P @ nonTrivialTokens)
        fun regOfTyp [] t = 1.0
          | regOfTyp ((T,r)::L) t = if Property.contains T t then Real.max (r, regOfTyp L t) else regOfTyp L t
        fun regOfTk' [] x = (1.0,false)
          | regOfTk' (([],_)::L) x = regOfTk' L x
          | regOfTk' ((s::S,r)::L) x = if Property.LabelOf x = s
                                      then (Real.max (r, #1 (regOfTk' L x)), true)
                                      else regOfTk' ((S,r)::L) x;
        fun regOfTk L x = let val (sr,found) = regOfTk' L x
                          in (if found then sr
                              else regOfTyp typregs (Property.getTypeOfValue x))
                              * (#2 (Property.getNumFunction "occurrences" x))
                              (* if token does not appear directly on patterns, but a hole with the type of the token does, approximate it with that*)
                          end
        val total = List.sumIndexed ((regOfTk tkregs) o QProperty.withoutImportance) C
    in total
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
    in (numberOfPatternsModulated qT) * (List.avgIndexed modereg M)
    end;


fun arity qT =
    let val P = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Pattern)
        val C = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Token)
        val n = numberOfTokens qT / numberOfTokenTypes qT
        fun a p = let val x = QProperty.withoutImportance p
                      val i = Pattern.arity x
                  in if i = ~3 then n else
                     if i = ~2 then Math.sqrt n else
                     if i = ~1 then Math.ln n else
                         real i
                  end
        fun weighing x = (#2 (Property.getNumFunction "occurrences" (QProperty.withoutImportance x)))
                          * (Importance.weight (QProperty.importanceOf x))
        val nonTrivialTokens = List.filter (fn c => Pattern.arity (QProperty.withoutImportance c) <> 0) C
    in Math.sqrt (List.weightedSumIndexed weighing (fn x => Math.pow(a x,2.0)) (nonTrivialTokens @ P))
    end


fun expressionComplexity qT =
    let val P = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Pattern)
        val C = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Token)
        val n = numberOfTokens qT / numberOfTokenTypes qT
        fun f p =
            let val x = QProperty.withoutImportance p
                val _ = print ("\n   " ^ (Property.toString x))
                val (L,(d,b)) = Pattern.satisfyPattern x (map QProperty.withoutImportance C) (map QProperty.withoutImportance P)
                val _ = print ("\n       length of final DNF: " ^ (Int.toString (length L)))
                val depth = (print ("\n       depth:" ^ (Real.toString d) ^ " "); d) (*Pattern.avgDepth trees*)
                val breadth = (print ("\n       breadth:" ^ (Real.toString b) ^ " "); b)
            in (depth * breadth)
            end
        fun weighing x = (#2 (Property.getNumFunction "occurrences" (QProperty.withoutImportance x)))
                          * (Importance.weight (QProperty.importanceOf x))
        val nonTrivialTokens = List.filter (fn c => Pattern.arity (QProperty.withoutImportance c) <> 0) C
    in Math.sqrt (List.weightedSumIndexed weighing f (nonTrivialTokens @ P))
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

        val pT = PropertyTables.transformQPropertySet qT arithT (corrT @ corrT')
        fun check x L = List.exists (fn y => (Property.LabelOf o QProperty.withoutImportance) x = y handle Property.Error _ => false) L
        fun ordinalFun x = check x ["<",">","\\leq","\\geq","max","min","\\max","\\min"]
        fun intervalFun x = check x ["+","-","sum","\\sum"]
        fun ratioFun x = check x ["*","\\div","\\dvd","\\gcd","\\prod","\\lcm","/","^"]
        fun nominalFun x = not (ordinalFun x orelse intervalFun x orelse ratioFun x)

        val pL = QPropertySet.toList pT

        val ratio = List.filter ratioFun pL
        val interval = List.filter intervalFun pL
        val ordinal = List.filter ordinalFun pL
        val nominal = List.filter nominalFun pL

        val s = 4.0*(List.sumIndexed (Importance.weight o QProperty.importanceOf) ratio)
              + 3.0*(List.sumIndexed (Importance.weight o QProperty.importanceOf) interval)
              + 2.0*(List.sumIndexed (Importance.weight o QProperty.importanceOf) ordinal)
              + (List.sumIndexed (Importance.weight o QProperty.importanceOf) nominal)
    in s
    end;


fun conceptMapping kind idealqT rT =
    let fun present x = #2 (Property.getNumFunction "occurrences" (x)) > 0.0
        val C = (*QPropertySet.filter (fn x => present x handle Property.NoAttribute _ => true)*) idealqT
        val C1 = QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.High) C
        val C2 = QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.Medium) C
        val C3 = QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.Low) C

        val corrT' = map (Correspondence.flip 1.0) corrT

        val rT' = PropertySet.filter (fn x => Property.kindOf x = kind andalso present x (*Property.kindOf x = Kind.Token orelse Property.kindOf x = Kind.Pattern*)) rT
        fun assess_rd p =
            let val T = PropertyTables.transformQProperty p rT' (corrT @ corrT')
                val x = QPropertySet.size T
                val s = if x = 1 (* functional *) then 0.0 else
                         if x > 1 (* redundancy *) then 2.0 * Math.ln(real x)/Math.ln(2.0) else
                         if x < 1 (* deficit *) then 3.0 else raise Match
            in s
            end;

        fun assess_oe C' r =
            let val T = PropertyTables.transformQProperty (QProperty.fromPair (r,Importance.High)) (QPropertySet.withoutImportances C') (corrT @ corrT')
                val x = QPropertySet.size T
                val s = if x = 1 (* injetive & surjective *) then 0.0 else
                         if x > 1 (* overload *) then 4.0 * Math.ln(real x)/Math.ln(2.0) else
                         if x < 1 (* excess *) then 1.0 else raise Match
            in s
            end;

        val rd1 = List.sumIndexed assess_rd (QPropertySet.toList C1)
        val rd2 = List.sumIndexed assess_rd (QPropertySet.toList C2)
        val rd3 = List.sumIndexed assess_rd (QPropertySet.toList C3)
        val _ = print ((Real.toString rd2) ^ " \n")

        fun globalOE X = (List.avgIndexed (assess_oe X) (PropertySet.toList rT')) * real (QPropertySet.size X) handle Empty => 0.0
        val oe1 = globalOE C1
        val oe2 = globalOE C2
        val oe3 = globalOE C3
        val _ = print ((Real.toString oe2) ^ "\n")
        val _ = print ("\n")

    in   (Importance.weight Importance.High) * (rd1 + oe1)
       + (Importance.weight Importance.Medium) * (rd2 + oe2)
       + (Importance.weight Importance.Low) * (rd3 + oe3)
    end;

fun tokenConceptMapping idealqT rT = conceptMapping Kind.Token idealqT rT;

fun expressionConceptMapping idealqT rT = conceptMapping Kind.Pattern idealqT rT;


fun inferenceType qT =
    let val T = PropertySet.collectOfKind (QPropertySet.withoutImportances qT) Kind.Tactic;
        val S = PropertySet.map (fn x => #2 (Property.getStringFunction "inference_type" x)) T
        fun assess s = if s = "assign" then 1.0
                  else if s = "match" then 2.0
                  else if s = "subst" then 3.0
                  else if s = "calc" then 4.0
                  else (print ("Cannot find inference type: " ^ s ^ "\n") ;raise Match)
    in List.avgIndexed assess S handle Empty => Real.posInf
    end;


fun problemSpaceBranchingFactor qT rT =
    let val T = PropertySet.collectOfKind (QPropertySet.withoutImportances qT) Kind.Tactic;
        val L = PropertySet.collectOfKind (QPropertySet.withoutImportances qT) Kind.Law;
        val P = QPropertySet.withoutImportances (QPropertySet.collectOfKind qT Kind.Pattern);
        fun lawParams t = #2 (Property.getNumFunction "laws" t) handle Property.NoAttribute _ => 0.0
        fun patternParams t = #2 (Property.getNumFunction "patterns" t) handle Property.NoAttribute _ => 0.0
        val bl = PropertySet.size L
        val bp = List.sumIndexed (#2 o (Property.getNumFunction "occurrences")) (PropertySet.toList P)
        fun dotProduct [] [] = 0.0
          | dotProduct (h::t) (h'::t') = (h * h') + dotProduct t t'
          | dotProduct  _ _ = raise Match;
        val l = PropertySet.map (fn x => Math.pow(real bl, lawParams x)) T
        val p = PropertySet.map (fn x => Math.pow(bp, patternParams x)) T
        val result = dotProduct l p
    in if Real.==(result,0.0) then Real.posInf else result
    end;


end;
