signature COGNITIVE_PROPERTIES =
sig
  val sigmoid : real -> real -> real -> real -> real;
end;

structure CognitiveProperties : COGNITIVE_PROPERTIES =
struct

(* C and W are meant to be set per property/process, while T is meant to be set
   from the User Profile. *)
fun sigmoid C W T x = 1.0 - (1.0 / (1.0 + Math.pow(C,((x-T)/W))));


(* Cognitive property 1a *)
fun numberOfTokens qT =
    Property.NumberOf
      ((#1 o QProperty.toPair o QPropertySet.getFirst)
          (QPropertySet.collectOfKind qT "num_tokens")) ;
fun numberOfTokensCost qT uP =
    let val C = 2.0 ;
        val W = 20.0 ;
        fun userTh table profile = 5.0 ;
        val T = userTh qT uP;
        val x = real (numberOfTokens qT)
    in sigmoid C W T x
    end;

(* Cognitive property 1b *)
fun numberOfDistinctTokens qT =
    Property.NumberOf
      ((#1 o QProperty.toPair o QPropertySet.getFirst)
          (QPropertySet.collectOfKind qT "num_distinct_tokens")) ;
fun numberOfDistinctTokensCost qT uP =
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
fun numberOfTokenTypesCost qT uP =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0 ;
        val T = userTh qT uP;
        val x = real (numberOfTokenTypes qT)
    in sigmoid C W T x
    end;

(* Cognitive property 3a *)
(* Notice this is not number of expressions, because it's not clear how this can be calculated at all*)
fun numberOfPatternTypes qT =
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
fun numberOfDistinctPatternsCost qT uP =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0 ;
        val T = userTh qT uP;
        val x = real (numberOfDistinctPatterns qT)
    in sigmoid C W T x
    end;

(* Cognitive property 3 *)
fun expressionVariety qT = numberOfPatterns qT + numberOfDistinctPatterns qT
fun expressionVarietyCost qT uP =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0 ;
        val T = userTh qT uP;
        val x = real (expressionVariety qT)
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
fun registartionTokenCost qT uP =
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
fun patternComplexity rT qT =
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
fun patternComplexityCost qT rT uP =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0;
        val T = userTh qT uP;
        val x = patternComplexity rT qT
    in sigmoid C W T x
    end;


fun quantityScale qT =  ;
fun quantityScaleCost qT uP =
    let val C = 2.0 ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP;
    in sigmoid C W T x
    end;


fun tokenConceptMapping qT =  ;
fun tokenConceptMappingCost qT uP =
    let val C = 2.0 ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP;
    in sigmoid C W T x
    end;

fun expressionConceptMapping qT =  ;
fun expressionConceptMappingCost qT uP =
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
