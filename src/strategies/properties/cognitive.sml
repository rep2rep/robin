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

(*
val _ = registerPropertyReaders
            PropertyTables.setQGenerators
            PropertyTables.setRSGenerators;
*)

(* C and W are meant to be set per property/process, while T is meant to be set
   from the User Profile. *)
fun sigmoid C W T x = 1.0 - (1.0 / (1.0 + Math.pow(C,((x-T)/W))));

fun collectOfKindPresentInQ qS k = QPropertySet.filter (fn x => #2 (Property.getNumFunction "occurrences" (QProperty.withoutImportance x)) > 0.0) (QPropertySet.collectOfKind qS k)

(* Cognitive property 1a *)
fun numberOfTokens qT =
    let val C = QPropertySet.toList (QPropertySet.collectOfKind qT Kind.Token)
    in List.sumIndexed (#2 o (Property.getNumFunction "occurrences") o QProperty.withoutImportance) C
    end;

(* Cognitive property 1b *)
fun varietyOfTokens qT =
    let val C = QPropertySet.toList (QPropertySet.collectOfKind qT Kind.Token)
        fun f x = if #2 (Property.getNumFunction "occurrences" (QProperty.withoutImportance x)) > 0.0 then 1.0 else 0.0
    in List.sumIndexed f C
    end;


(* Cognitive property 2 *)
fun numberOfTokenTypes qT =
    let val C = collectOfKindPresentInQ qT Kind.Token
        val T = QPropertySet.map (Property.getTypeOfValue o QProperty.withoutImportance) C
    in real (List.length (List.removeDuplicates T))
    end;

(* Cognitive property 3a *)
(* Notice this is not number of expressions, because it's not clear how this can be calculated at all*)
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

(* Cognitive property 3b *)
(* Notice this is not number of expressions, because it's not clear how this can be calculated at all*)
fun numberOfPatterns qT =
    let val P = QPropertySet.toList (QPropertySet.collectOfKind qT Kind.Pattern)
    in List.sumIndexed (#2 o (Property.getNumFunction "occurrences") o QProperty.withoutImportance) P
    end;

(* Cognitive property 3 *)
fun varietyOfExpressions qT =
    let val P = collectOfKindPresentInQ qT Kind.Pattern
    in real (QPropertySet.size P)
    end;

(* Cognitive property 4 *)
fun subRSVariety rT = real (PropertySet.size (PropertySet.collectOfKind rT Kind.Mode));

(* Cognitive property 5 *)
(* Collects the token registration from the patterns where the tokens appear.
Registration is linearly influenced by number of tokens, because each token's
registration is multiplied by its number of occurrences  *)
fun tokenRegistration qT =
    let val C = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Token)
        val P = QPropertySet.withoutImportances (collectOfKindPresentInQ qT Kind.Pattern)
  (*    fun importance_filter i X =
            map QProperty.withoutImportance (List.filter (fn x => QProperty.importanceOf x = i) X)
        val C1 = importance_filter Importance.High C
        val C2 = importance_filter Importance.Medium C
        val C3 = importance_filter Importance.Low C
        val C4 = importance_filter Importance.Zero C
        val C5 = importance_filter Importance.Noise C*)
        fun tokensFromPattern x = (Property.getTokens x, #2 (Property.getNumFunction "token_registration" x))
                                    handle Property.NoAttribute _ => (print ("exception with pattern: " ^ (Property.toString x)); ([],1.0))
        fun typesFromPattern x = (Property.getHoles x, #2 (Property.getNumFunction "token_registration" x))
                                    handle Property.NoAttribute _ => (print ("exception with pattern: " ^ (Property.toString x)); (Property.getHoles x,1.0))
        val tokenswithreg = PropertySet.map tokensFromPattern P
        val typeswithreg = PropertySet.map typesFromPattern P
        fun typereg [] t = 1.0
          | typereg ((T,r)::L) t = if Property.contains T t then Real.max (r, typereg L t) else typereg L t
        fun symbreg' [] x = (1.0,false)
          | symbreg' (([],_)::L) x = symbreg' L x
          | symbreg' ((s::S,r)::L) x = if Property.LabelOf x = s
                                      then (Real.max (r, #1 (symbreg' L x)), true)
                                      else symbreg' ((S,r)::L) x;
        fun symbreg L x = let val (sr,found) = symbreg' L x
                          in (if found then sr
                              else typereg typeswithreg (Property.getTypeOfValue x))
                              * (#2 (Property.getNumFunction "occurrences" x))
                              (* if token does not appear directly on patterns, but a hole with the type of the token does, approximate it with that*)
                          end
    (*)    val S = map (map (symbreg tokenswithreg)) [C1,C2,C3,C4,C5] (*this extracted the token_registration from the patterns for each token, stratified by importances*)
        val [s1,s2,s3,s4,s5] = map (List.sum) S
        val [w1,w2,w3] = map (Importance.weight) [Importance.High, Importance.Medium, Importance.Low]
        val total = (s1 + s2 + s3 + s4 + s5)*)

        val S = map ((symbreg tokenswithreg) o QProperty.withoutImportance) C
        val total = List.sum S
          (* thought about multiplying by a factor of all vs important,
            but maybe importance doesn't belong in registration; being a semantic property *)
    in total
    end;

(* Cognitive property 6 *)
(* instrumental patterns divided by all patterns, times the average
value for the expression attributes of types*)
fun expressionRegistration qT rT =
    let val M = PropertySet.toList (PropertySet.collectOfKind (QPropertySet.withoutImportances qT) Kind.Mode)
  (*    val P = (QPropertySet.collectOfKind qT Kind.Pattern)
        val p1 = QPropertySet.size (QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.High) P)
        val p2 = QPropertySet.size (QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.Medium) P)
        val p3 = QPropertySet.size (QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.Low) P) *)
        fun modereg x = if Property.LabelOf x = "grid" then 1.0
                   else if Property.LabelOf x = "containment" then 1.0
                   else if Property.LabelOf x = "axial" then 2.0
                   else if Property.LabelOf x = "sentential" then 2.0
                   else if Property.LabelOf x = "connection" then 3.0
                   else if Property.LabelOf x = "proportional" then 3.0
                   else (print "unknown mode"; raise Match)
  (*    val [w1,w2,w3] = map (Importance.weight) [Importance.High, Importance.Medium, Importance.Low]*)
    in (numberOfPatterns qT) * (List.avgIndexed modereg M)
    end;

(* Cognitive property 7 *)
fun expressionComplexity qT (*rT *)=
    let val P = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Pattern)
        val C = QPropertySet.toList (collectOfKindPresentInQ qT Kind.Token)
        val n = numberOfTokens qT / numberOfTokenTypes qT
        fun f p =
            let val x = QProperty.withoutImportance p
                val _ = print ("\n   " ^ (Property.toString x))
                val (_,(d,b)) = Pattern.satisfyPattern x (map QProperty.withoutImportance C) (map QProperty.withoutImportance P)
                val depth = (print ("\n       depth:" ^ (Real.toString d) ^ " "); d) (*Pattern.avgDepth trees*)
                val breadth = (print ("\n       breadth:" ^ (Real.toString b) ^ " "); b)
                val arity = let val i = Pattern.arity x
                            in if i = ~3 then n else
                               if i = ~2 then Math.sqrt n else
                               if i = ~1 then Math.ln n else
                                   real i
                            end
                val _ = print ("\n       arity:" ^ (Real.toString arity) ^ " ")
            in Math.sqrt (Math.pow(arity,2.0) + (depth * breadth))
            end
        fun weighing x = (*(#2 (Property.getNumFunction "occurrences" (QProperty.withoutImportance x)))
                          **) (Importance.weight (QProperty.importanceOf x))
        val nonTrivialTokens = List.filter (fn c => Pattern.arity (QProperty.withoutImportance c) <> 0) C
    in List.weightedAvgIndexed weighing f (nonTrivialTokens @ P)
    end;


fun quantityScale qT =
    let
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
        val arithpath = filesMatchingPrefix "tables/" "RS_table_realarith"
        val (_,arithT) = PropertyTables.loadRepresentationTable (hd arithpath)

        (* this will probably change one I incorporate these calculations
        into the stream of representation selection and the tables are handed
        over by the main function *)
        val corrpaths = filesMatchingPrefix "tables/" "correspondences_"
        val corrT = List.concat (map PropertyTables.loadCorrespondenceTable corrpaths)
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

fun tokenConceptMapping qT idealT =
    let val C = collectOfKindPresentInQ qT Kind.Token
        val C1 = QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.High) C
        val C2 = QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.Medium) C
        val C3 = QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.Low) C
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
        (* this will probably change one I incorporate these calculations
        into the stream of representation selection and the tables are handed
        over by the main function *)
        val corrpaths = filesMatchingPrefix "tables/" "correspondences_"
        val corrT = List.concat (map PropertyTables.loadCorrespondenceTable corrpaths)
        val corrT' = map (Correspondence.flip 1.0) corrT

        fun assess_rd p =
            let val T = PropertyTables.transformQProperty p idealT (corrT @ corrT')
                val x = QPropertySet.size T
            in if x = 1 then 1.0 else (* functional *)
               if x > 1 then 3.0 else (* redundancy *)
               if x < 1 then 4.0 else 0.0  (* deficit *)
            end;

        fun assess_oe r =
            let val C' = QPropertySet.withoutImportances C
                val T = PropertyTables.transformQProperty (QProperty.fromPair (r,Importance.High)) C' (corrT @ corrT')
                val x = QPropertySet.size T
            in if x = 1 then 0.0 else (* injetive & surjective *)
               if x > 1 then 5.0 else (* overload *)
               if x < 1 then 2.0 else 0.0 (* excess *)
            end;
        val s1 = List.sumIndexed assess_rd (QPropertySet.toList C1)
        val s2 = List.sumIndexed assess_rd (QPropertySet.toList C2)
        val s3 = List.sumIndexed assess_rd (QPropertySet.toList C3)
        val oe = List.sumIndexed assess_oe (PropertySet.toList idealT)
(* note that overload and excess don't take importance into account precisely
because they are properties of the target RS and not of the source Q,
so it's not possible to assess the improtance of such overload or excess.
This is debatable for overload, as it does involve q properties. *)
    in   (Importance.weight Importance.High) * s1
       + (Importance.weight Importance.Medium) * s2
       + (Importance.weight Importance.Low) * s3
       + oe
    end;

(*  *)
fun expressionConceptMapping qT idealT =
    let val C = collectOfKindPresentInQ qT Kind.Pattern
        val C1 = QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.High) C
        val C2 = QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.Medium) C
        val C3 = QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.Low) C
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
        (* this will probably change one I incorporate these calculations
        into the stream of representation selection and the tables are handed
        over by the main function *)
        val corrpaths = filesMatchingPrefix "tables/" "correspondences_"
        val corrT = List.concat (map PropertyTables.loadCorrespondenceTable corrpaths)
        val corrT' = map (Correspondence.flip 1.0) corrT

        fun assess_rd p =
            let val T = PropertyTables.transformQProperty p idealT (corrT @ corrT')
                val x = QPropertySet.size T
            in if x = 1 then 0.0 else (* functional *)
               if x > 1 then 3.0 else (* redundancy *)
               if x < 1 then 4.0 else (* deficit *)
                  0.0
            end;

        fun assess_oe r =
            let val C' = QPropertySet.withoutImportances C
                val T = PropertyTables.transformQProperty (QProperty.fromPair (r,Importance.High)) C' (corrT @ corrT')
                val x = QPropertySet.size T
            in if x = 1 then 1.0 else (* injetive & surjective *)
               if x > 1 then 5.0 else (* overload *)
               if x < 1 then 2.0 else (* excess *)
                  0.0
            end;
            val s1 = List.sumIndexed assess_rd (QPropertySet.toList C1)
            val s2 = List.sumIndexed assess_rd (QPropertySet.toList C2)
            val s3 = List.sumIndexed assess_rd (QPropertySet.toList C3)
        val oe = List.sumIndexed assess_oe (PropertySet.toList idealT)
(* note that overload and excess don't take importance into account precisely
because they are properties of the target RS and not of the source Q,
so it's not possible to assess the improtance of such overload or excess.
This is debatable for overload, as it does have q properties. *)
    in   (Importance.weight Importance.High) * s1
       + (Importance.weight Importance.Medium) * s2
       + (Importance.weight Importance.Low) * s3
       + oe
    end;

fun problemSpaceBranchingFactor qT rT =
    let val T = PropertySet.collectOfKind (QPropertySet.withoutImportances qT) Kind.Tactic;
        val _ = print (Int.toString (PropertySet.size T))
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
