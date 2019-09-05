import "strategies.properties.tables"

signature COGNITIVE_PROPERTIES =
sig

  type qtable = (string * string) * QPropertySet.t QPropertySet.set;
  type rstable = string * PropertySet.t PropertySet.set;
  type correspondencetable = Correspondence.correspondence list;
  type userprofile;

  val sigmoid : real -> real -> real -> real -> real;

  val numberOfTokens : qtable -> real;
  val varietyOfTokens : qtable -> real;
  val numberOfTokenTypes : qtable -> real;
  val varietyOfExpressions : qtable -> real;
  val numberOfExpressionTypes : qtable -> real;
  val subRSVariety : rstable -> real;
  val tokenRegistration : qtable -> real;
  val expressionRegistration : qtable -> rstable -> real;
  val expressionComplexity : qtable -> rstable -> real;
  val quantityScale : questiontable -> real;
  val tokenConceptMapping : qTable -> rstable -> real;
  val expressionConceptMapping : qTable -> rstable -> real;
  val problemSpaceBranchingFactor : qTable -> rstable -> real;

  val numberOfTokensCost : userprofile -> qtable -> real;
  val varietyOfTokensCost : userprofile -> qtable -> real;
  val numberOfTokenTypesCost : userprofile -> qtable -> real;
  val varietyOfExpressionsCost : userprofile -> qtable -> real;
  val numberOfExpressionTypesCost : userprofile -> qtable -> real;
  val subRSVarietyCost : userprofile -> rstable -> real;
  val tokenRegistrationCost : userprofile -> qtable -> real;
  val expressionRegistrationCost : userprofile -> qtable -> rstable -> real;
  val expressionComplexityCost : userprofile -> qtable -> rstable -> real;
  val quantityScaleCost : userprofile -> questiontable -> real;
  val tokenConceptMappingCost : userprofile -> qTable -> rstable -> real;
  val expressionConceptMappingCost : userprofile -> qTable -> rstable -> real;
  val problemSpaceBranchingFactorCost : userprofile -> qTable -> rstable -> real;
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


type questiontable = (string * string) * QPropertySet.t QPropertySet.set;
type representationtable = string * PropertySet.t PropertySet.set;
type correspondencetable = Correspondence.correspondence list;

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
          (QPropertySet.collectOfKind qT "num_tokens")) ;
fun numberOfTokensCost uP qT =
    let val C = 2.0 ;
        val W = 20.0 ;
        fun userTh table profile = 5.0 ;
        val T = userTh qT uP;
        val x = real (numberOfTokens qT)
    in sigmoid C W T x
    end;

(* Cognitive property 1b *)
fun varietyOfTokens qT = QPropertySet.size (QPropertySet.collectOfKind qT "token");
  (*)  Property.NumberOf
      ((#1 o QProperty.toPair o QPropertySet.getFirst)
          (QPropertySet.collectOfKind qT "num_distinct_tokens")) ;*)
fun varietyOfTokensCost uP qT =
    let val C = 2.0 ;
        val W = 20.0 ;
        fun userTh table profile = 5.0 ;
        val T = userTh qT uP;
        val x = real (numberOfTokens qT)
    in sigmoid C W T x
    end;

(* Cognitive property 2 *)
fun numberOfTokenTypes qT =
    let val C = QPropertySet.collectOfKind qT "token"
        val T = QPropertySet.map (Property.getTypeOfValue o Importance.withoutImportance) C
    in QPropertySet.size T
    end;
fun numberOfTokenTypesCost uP qT =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0 ;
        val T = userTh qT uP;
        val x = real (numberOfTokenTypes qT)
    in sigmoid C W T x
    end;

(* Cognitive property 3a *)
(* Notice this is not number of expressions, because it's not clear how this can be calculated at all*)
fun numberOfExpressionTypes qT =
    let val P = QPropertySet.collectOfKind qT "pattern"
        val T = QPropertySet.map (Property.getTypeOfValue o Importance.withoutImportance) P
    in QPropertySet.size T
    end;

(* Cognitive property 3b *)
(* Notice this is not number of expressions, because it's not clear how this can be calculated at all*)
fun numberOfPatterns qT =
    let val P = QPropertySet.toList (QPropertySet.collectOfKind qT "pattern")
    in List.sumIndexed (#2 o (Property.getNumFunction "occurrences")) P
    end;
fun numberOfPatternsCost qT rT uP =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0 ;
        val T = userTh qT rT uP;
        val x = real (numberOfPatterns qT)
    in sigmoid C W T x
    end;

(* Cognitive property 3c *)
(* Notice this is not number of expressions, because it's not clear how this can be calculated at all*)
fun numberOfDistinctPatterns qT = QPropertySet.size (QPropertySet.collectOfKind qT "pattern");
fun numberOfDistinctPatternsCost uP qT =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0 ;
        val T = userTh qT uP;
        val x = real (numberOfDistinctPatterns qT)
    in sigmoid C W T x
    end;

(* Cognitive property 3 *)
fun varietyOfExpressions qT = numberOfPatterns qT + numberOfDistinctPatterns qT
fun varietyOfExpressionsCost uP qT =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0 ;
        val T = userTh qT uP;
        val x = real (varietyOfExpressions qT)
    in sigmoid C W T x
    end;

(* Cognitive property 4 *)
fun subRSvariety rT = PropertySet.size (PropertySet.collectOfKind rT "mode");
fun subRSvarietyCost rT uP =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0;
        val T = userTh qT uP;
        val x = subRSvariety rT;
    in sigmoid C W T x
    end;

(* Cognitive property 5 *)
(* Collects the token registration from the patterns where the tokens appear *)
fun tokenRegistration qT =
    let val C = QPropertySet.toList (QPropertySet.collectOfKind qT "token")
        val P = QPropertySet.toList (QPropertySet.collectOfKind qT "pattern")
        val C1 = map Importance.withoutImportance (List.filter (fn x => Importance.importanceOf x = Importance.High) C)
        val C2 = map Importance.withoutImportance (List.filter (fn x => Importance.importanceOf x = Importance.Medium) C)
        val C3 = map Importance.withoutImportance (List.filter (fn x => Importance.importanceOf x = Importance.Low) C)
        val C4 = map Importance.withoutImportance (List.filter (fn x => Importance.importanceOf x = Importance.Zero) C)
        val C5 = map Importance.withoutImportance (List.filter (fn x => Importance.importanceOf x = Importance.Noise) C)
        val tokenswithreg = map (fn x => (Property.getTokens x, #2 o (Property.getNumFunction "token_registration" x))) P
        fun symbreg [] x = (print "no token_registration attribute for " ^ (Property.toString x); (x,1.0))
          | symbreg (([],_)::L) x = symbreg L x
          | symbreg ((s::S,r)::L) x = if Property.LabelOf x = s then (x, Real.max (r,#2 (symbreg L x))) else symbreg x ((S,r)::L);
        val S = map (map (symbreg tokenswithreg)) [C1,C2,C3,C4,C5] (*this extracted the token_registration from the patterns for each token, stratified by importances*)
        val [s1,s2,s3,s4,s5] = map (List.sumIndexed #2) S
        val total = s1+s2+s3+s4+s5
        val [w1,w2,w3] = map (Importance.weight) [Importance.High, Importance.Medium, Importance.Low]
    in total * total / (w1*s1 + w2*s2 + w3*s3)
    end;
fun registartionTokenCost uP qT =
    let val C = 2.0 ;
        val W = 2.0;
        fun userTh table profile = 2.0;
        val T = userTh qT uP;
        val x = tokenRegistration qT;
    in sigmoid C W T x
    end;

(* Cognitive property 6 *)
(* instrumental patterns divided by all patterns, times the average
value for the expression attributes of types*)
fun expressionRegistration qT rT =
    let val M = QPropertySet.toList (QPropertySet.collectOfKind rT "mode")
        val P = (QPropertySet.collectOfKind qT "pattern")
        val p1 = QPropertySet.size (QPropertySet.filter (fn x => Importance.importanceOf x = Importance.High) P)
        val p2 = QPropertySet.size (QPropertySet.filter (fn x => Importance.importanceOf x = Importance.Medium) P)
        val p3 = QPropertySet.size (QPropertySet.filter (fn x => Importance.importanceOf x = Importance.Low) P)
        fun modereg x = if Property.labelOf x = "grid" then 1.0
                   else if Property.labelOf x = "containment" then 1.0
                   else if Property.labelOf x = "axial" then 2.0
                   else if Property.labelOf x = "sentential" then 2.0
                   else if Property.labelOf x = "connection" then 3.0
                   else if Property.labelOf x = "proportional" then 3.0
                   else (print "unknown mode"; raise Match)
        val [w1,w2,w3] = map (Importance.weight) [Importance.High, Importance.Medium, Importance.Low]
    in (numberOfPatterns qT) * (List.avgIndexed modereg M) / (w1*p1 + w2*p2 + w3*p3)
    end;
fun expressionRegistrationCost qT rT uP =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0 ;
        val T = userTh qT uP;
        val x = expressionRegistration qT;
    in sigmoid C W T x
    end;

(* Cognitive property 7 *)
fun expressionComplexity qT rT =
    let val P = QPropertySet.toList (QPropertySet.collectOfKind qT "pattern")
        val C = QPropertySet.collectOfKind rT "token"
        fun f p =
            let val x = QProperty.withoutImportance p
                val depth = Pattern.depth C x
                val breadth = map Pattern.breadth C x
                val typeArity = map Pattern.typeArity x
                val distinctArity = map Pattern.distinctArity x
            in depth + breadth + (typeArity + distinctArity) / 2
            end
    in List.weightedSumIndexed (Importance.weight o QProperty.importanceOf) f P
    end;
fun expressionComplexityCost qT rT uP =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0;
        val T = userTh qT uP;
        val x = expressionComplexity qT rT
    in sigmoid C W T x
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

        val ((_,_), pT) = PropertyTables.computePseudoQuestionTable qT arithT corrT

        fun check x L = List.exists (fn y => (Property.LabelOf o QProperty.withoutImportance) x = y) L
        fun ordinalFun x = check x ["<",">","\\leq","\\geq","max","min","\\max","\\min"]
        fun intervalFun x = check x ["+","-","sum","\\sum"]
        fun ratioFun x = check x ["*","\\div","\\dvd","\\gcd","\\lcm","/"]
        fun nominalFun x = not (ordinalFun x orelse intervalFun x orelse ratioFun x)

        val pL = QPropertySet.toPairList pT

        val ratio = List.filter ratioFun pL
        val interval = List.filter intervalFun pL
        val ordinal = List.filter ordinalFun pL
        val nominal = List.filter nominalFun pL

        val s = 4.0*(List.sumIndexed QProperty.importanceOf ratio)
              + 3.0*(List.sumIndexed QProperty.importanceOf interval)
              + 2.0*(List.sumIndexed QProperty.importanceOf ordinal)
              + (List.sumIndexed QProperty.importanceOf nominal)
    in s / (varietyOfTokens qT)
    end;

fun quantityScaleCost uP qT =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0 ;
        val T = userTh qT uP;
    in sigmoid C W T x
    end;


fun tokenConceptMapping qT =  ;
fun tokenConceptMappingCost uP qT =
    let val C = 2.0 ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP;
    in sigmoid C W T x
    end;

fun expressionConceptMapping qT =  ;
fun expressionConceptMappingCost uP qT =
    let val C = 2.0 ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP;
    in sigmoid C W T x
    end;

fun problemSpaceBranchingFactor qT = ;
fun problemSpaceBranchingFactorCost qT iP =
    let val C = 2.0 ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP;
    in sigmoid C W T x
    end;


end;
