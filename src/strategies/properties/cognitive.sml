import "strategies.properties.tables";
import "strategies.properties.pattern";

signature COGNITIVE_PROPERTIES =
sig

  type qtable = QPropertySet.t QPropertySet.set;
  type rstable = PropertySet.t PropertySet.set;
  type correspondencetable = Correspondence.correspondence list;
  type userprofile;

  val numberOfTokens : qtable -> int;
  val varietyOfTokens : qtable -> int;
  val numberOfTokenTypes : qtable -> int;
  val varietyOfExpressions : qtable -> real;
  val numberOfExpressionTypes : qtable -> int;
  val subRSVariety : rstable -> int;
  val tokenRegistration : qtable -> real;
  val expressionRegistration : qtable -> rstable -> real;
  val expressionComplexity : qtable -> rstable -> real;
  val quantityScale : qtable -> real;
  val tokenConceptMapping : qtable -> rstable -> real;
  val expressionConceptMapping : qtable -> rstable -> real;
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
type correspondencetable = Correspondence.correspondence list;
type userprofile = (string * real) list;

val _ = registerPropertyReaders
            PropertyTables.setQGenerators
            PropertyTables.setRSGenerators;


val propertyTableRep' = ref (TableDict.empty ());
val correspondingTable' = ref [];
val propertyTableQ' = ref (TableDict.empty ());


(* C and W are meant to be set per property/process, while T is meant to be set
   from the User Profile. *)
fun sigmoid C W T x = 1.0 - (1.0 / (1.0 + Math.pow(C,((x-T)/W))));


(* Cognitive property 1a *)
fun numberOfTokens qT =
    Property.NumberOf
      ((#1 o QProperty.toPair o QPropertySet.getFirst)
          (QPropertySet.collectOfKind qT Kind.NumTokens)) ;

(* Cognitive property 1b *)
fun varietyOfTokens qT = QPropertySet.size (QPropertySet.collectOfKind qT Kind.Token);

(* Cognitive property 2 *)
fun numberOfTokenTypes qT =
    let val C = QPropertySet.collectOfKind qT Kind.Token
        val T = QPropertySet.map (Property.getTypeOfValue o QProperty.withoutImportance) C
    in List.length T
    end;

(* Cognitive property 3a *)
(* Notice this is not number of expressions, because it's not clear how this can be calculated at all*)
fun numberOfExpressionTypes qT =
    let val P = QPropertySet.collectOfKind qT Kind.Pattern
        val T = QPropertySet.map (Property.getTypeOfValue o QProperty.withoutImportance) P
    in List.length T
    end;

(* Cognitive property 3b *)
(* Notice this is not number of expressions, because it's not clear how this can be calculated at all*)
fun numberOfPatterns qT =
    let val P = QPropertySet.toList (QPropertySet.collectOfKind qT Kind.Pattern)
    in List.sumIndexed (#2 o (Property.getNumFunction "occurrences") o QProperty.withoutImportance) P
    end;

(* Cognitive property 3c *)
(* Notice this is not number of expressions, because it's not clear how this can be calculated at all*)
fun numberOfDistinctPatterns qT = real (QPropertySet.size (QPropertySet.collectOfKind qT Kind.Pattern));

(* Cognitive property 3 *)
fun varietyOfExpressions qT = numberOfPatterns qT + numberOfDistinctPatterns qT;

(* Cognitive property 4 *)
fun subRSVariety rT = PropertySet.size (PropertySet.collectOfKind rT Kind.Mode);

(* Cognitive property 5 *)
(* Collects the token registration from the patterns where the tokens appear *)
fun tokenRegistration qT =
    let val C = QPropertySet.toList (QPropertySet.collectOfKind qT Kind.Token)
        val P = QPropertySet.toList (QPropertySet.collectOfKind qT Kind.Pattern)
        val C1 = map QProperty.withoutImportance (List.filter (fn x => QProperty.importanceOf x = Importance.High) C)
        val C2 = map QProperty.withoutImportance (List.filter (fn x => QProperty.importanceOf x = Importance.Medium) C)
        val C3 = map QProperty.withoutImportance (List.filter (fn x => QProperty.importanceOf x = Importance.Low) C)
        val C4 = map QProperty.withoutImportance (List.filter (fn x => QProperty.importanceOf x = Importance.Zero) C)
        val C5 = map QProperty.withoutImportance (List.filter (fn x => QProperty.importanceOf x = Importance.Noise) C)
        val tokenswithreg = map ((fn x => (Property.getTokens x, #2 (Property.getNumFunction "token_registration" x))) o QProperty.withoutImportance) P
        fun symbreg [] x = (print ("no token_registration attribute for " ^ (Property.toString x)); (x,1.0))
          | symbreg (([],_)::L) x = symbreg L x
          | symbreg ((s::S,r)::L) x = if Property.LabelOf x = s then (x, Real.max (r,#2 (symbreg L x))) else symbreg ((S,r)::L) x;
        val S = map (map (symbreg tokenswithreg)) [C1,C2,C3,C4,C5] (*this extracted the token_registration from the patterns for each token, stratified by importances*)
        val [s1,s2,s3,s4,s5] = map (List.sumIndexed #2) S
        val total = s1+s2+s3+s4+s5
        val [w1,w2,w3] = map (Importance.weight) [Importance.High, Importance.Medium, Importance.Low]
    in total
    end;
(* Cognitive property 6 *)
(* instrumental patterns divided by all patterns, times the average
value for the expression attributes of types*)
fun expressionRegistration qT rT =
    let val M = PropertySet.toList (PropertySet.collectOfKind rT Kind.Mode)
        val P = (QPropertySet.collectOfKind qT Kind.Pattern)
        val p1 = QPropertySet.size (QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.High) P)
        val p2 = QPropertySet.size (QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.Medium) P)
        val p3 = QPropertySet.size (QPropertySet.filter (fn x => QProperty.importanceOf x = Importance.Low) P)
        fun modereg x = if Property.LabelOf x = "grid" then 1.0
                   else if Property.LabelOf x = "containment" then 1.0
                   else if Property.LabelOf x = "axial" then 2.0
                   else if Property.LabelOf x = "sentential" then 2.0
                   else if Property.LabelOf x = "connection" then 3.0
                   else if Property.LabelOf x = "proportional" then 3.0
                   else (print "unknown mode"; raise Match)
        val [w1,w2,w3] = map (Importance.weight) [Importance.High, Importance.Medium, Importance.Low]
    in (numberOfPatterns qT) * (List.avgIndexed modereg M)
    end;

(* Cognitive property 7 *)
fun expressionComplexity qT rT =
    let val P = QPropertySet.toList (QPropertySet.collectOfKind qT Kind.Pattern)
        val C = PropertySet.toList (PropertySet.collectOfKind rT Kind.Token)
        fun f p =
            let val x = QProperty.withoutImportance p
                val depth = Pattern.depth C x
                val breadth = Pattern.breadth C x
                val typeArity = Pattern.typeArity x
                val distinctArity = Pattern.distinctArity x
            in depth + breadth + (typeArity + distinctArity) / 2.0
            end
    in List.weightedSumIndexed (Importance.weight o QProperty.importanceOf) f P
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
        fun propertiesRS rep =
            TableDict.get (!propertyTableRep') (rep, rep)
            handle TableDict.KeyError =>
                   (Logging.error ("ERROR: representation '" ^ rep ^ "' not found!\n");
                   raise TableDict.KeyError);
        val arithT = propertiesRS "algebra"

        (* this will probably change one I incorporate these calculations
        into the stream of representation selection and the tables are handed
        over by the main function *)
        val corrpaths = filesMatchingPrefix "tables/" "correspondences_"
        val corrT = List.concat (map PropertyTables.loadCorrespondenceTable corrpaths)

        val ((_,_), pT) = PropertyTables.computePseudoQuestionTable (("",""),qT) ("algebra",arithT) corrT

        fun check x L = List.exists (fn y => (Property.LabelOf o QProperty.withoutImportance) x = y) L
        fun ordinalFun x = check x ["<",">","\\leq","\\geq","max","min","\\max","\\min"]
        fun intervalFun x = check x ["+","-","sum","\\sum"]
        fun ratioFun x = check x ["*","\\div","\\dvd","\\gcd","\\lcm","/"]
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
    in s / real (varietyOfTokens (qT))
    end;


fun tokenConceptMapping qT rT =
    let val C = QPropertySet.toList (QPropertySet.collectOfKind qT Kind.Token)
        val C1 = (List.filter (fn x => QProperty.importanceOf x = Importance.High) C)
        val C2 = (List.filter (fn x => QProperty.importanceOf x = Importance.Medium) C)
        val C3 = (List.filter (fn x => QProperty.importanceOf x = Importance.Low) C)
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

        fun assess_ird p =
            let val T = #2 (PropertyTables.computePseudoQuestionTable (("",""),QPropertySet.fromList [p]) ("x",rT) corrT)
                val x = QPropertySet.size T
            in if x = 1 then 1.0 else (* isomorphism *)
               if x > 1 then 3.0 else (* redundancy *)
               if x < 1 then 4.0 else 0.0  (* deficit *)
            end;

        val corrT' = map (fn c => case c of (a,b,s) => (b,a,s)) corrT
        fun assess_oe r =
            let val C' = PropertySet.fromList (map QProperty.withoutImportance C)
                val T = PropertyTables.computePseudoQuestionTable  (("",""),QPropertySet.fromList [QProperty.fromPair (r,Importance.High)]) ("x",C') corrT'
                val x = QPropertySet.size (#2 T)
            in if x = 1 then 0.0 else (* isomorphism *)
               if x > 1 then 5.0 else (* overload *)
               if x < 1 then 2.0 else 0.0 (* excess *)
            end;
        val s1 = List.sumIndexed assess_ird C1
        val s2 = List.sumIndexed assess_ird C2
        val s3 = List.sumIndexed assess_ird C3
        val oe = List.sumIndexed assess_oe (PropertySet.toList (rT))
(* note that overload and excess don't take importance into account precisely
because they are properties of the target RS and not of the source Q,
so it's not possible to assess the improtance of such overload or excess.
This is debatable for overload, as it does have q properties. *)
    in   (Importance.weight Importance.High) * s1
       + (Importance.weight Importance.Medium) * s2
       + (Importance.weight Importance.Low) * s3
       + oe
    end;


fun expressionConceptMapping qT rT =
    let val C = QPropertySet.toList (QPropertySet.collectOfKind qT Kind.Pattern)
        val C1 = (List.filter (fn x => QProperty.importanceOf x = Importance.High) C)
        val C2 = (List.filter (fn x => QProperty.importanceOf x = Importance.Medium) C)
        val C3 = (List.filter (fn x => QProperty.importanceOf x = Importance.Low) C)
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

        fun assess_ird p =
            let val T = #2 (PropertyTables.computePseudoQuestionTable (("",""),QPropertySet.fromList [p]) ("x",rT) corrT)
                val x = QPropertySet.size T
            in if x = 1 then 1.0 else (* isomorphism *)
               if x > 1 then 3.0 else (* redundancy *)
               if x < 1 then 4.0 else 0.0  (* deficit *)
            end;

        val corrT' = map (fn c => case c of (a,b,s) => (b,a,s)) corrT
        fun assess_oe r =
            let val C' = PropertySet.fromList (map QProperty.withoutImportance C)
                val T = PropertyTables.computePseudoQuestionTable  (("",""),QPropertySet.fromList [QProperty.fromPair (r,Importance.High)]) ("x",C') corrT'
                val x = QPropertySet.size (#2 T)
            in if x = 1 then 0.0 else (* isomorphism *)
               if x > 1 then 5.0 else (* overload *)
               if x < 1 then 2.0 else 0.0(* excess *)
            end;
        val s1 = List.sumIndexed assess_ird C1
        val s2 = List.sumIndexed assess_ird C2
        val s3 = List.sumIndexed assess_ird C3
        val oe = List.sumIndexed assess_oe (PropertySet.toList (rT))
(* note that overload and excess don't take importance into account precisely
because they are properties of the target RS and not of the source Q,
so it's not possible to assess the improtance of such overload or excess.
This is debatable for overload, as it does have q properties. *)
    in   (Importance.weight Importance.High) * s1
       + (Importance.weight Importance.Medium) * s2
       + (Importance.weight Importance.Low) * s3
       + oe
    end;

fun problemSpaceBranchingFactor qT rT = 0.0;


end;
