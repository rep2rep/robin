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
fun numberOfSymbols qT =
    Property.NumberOf
      ((#1 o QProperty.toPair o QPropertySet.getFirst)
          (QPropertySet.collectOfKind qT "num_tokens")) ;
fun numberOfSymbolsCost qT uP =
    let val C = 2.0 ;
        val W = 20.0 ;
        fun userTh table profile = 5.0 ;
        val T = userTh qT uP;
        val x = Real.fromInt (numberOfSymbols qT)
    in sigmoid C W T x
    end;

(* Cognitive property 1b *)
fun numberOfDistinctSymbols qT =
    Property.NumberOf
      ((#1 o QProperty.toPair o QPropertySet.getFirst)
          (QPropertySet.collectOfKind qT "num_distinct_tokens")) ;
fun numberOfDistinctSymbolsCost qT uP =
    let val C = 2.0 ;
        val W = 20.0 ;
        fun userTh table profile = 5.0 ;
        val T = userTh qT uP;
        val x = Real.fromInt (numberOfSymbols qT)
    in sigmoid C W T x
    end;

(* Cognitive property 2 *)
fun numberOfSymbolTypes qT = QPropertySet.size (QPropertySet.collectOfKind qT "type");
fun numberOfSymbolTypesCost qT uP =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0 ;
        val T = userTh qT uP;
        val x = Real.fromInt (numberOfSymbolTypes qT)
    in sigmoid C W T x
    end;

(* Cognitive property 3 *)
(* Notice this is not number of expressions, because it's not clear how this can be calculated at all*)
fun numberOfPatterns qT = QPropertySet.size (QPropertySet.collectOfKind qT "pattern");
fun numberOfPatternsCost qT uP =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0 ;
        val T = userTh qT uP;
        val x = Real.fromInt (numberOfPatterns qT)
    in sigmoid C W T x
    end;

(* Cognitive property 4 *)
fun subRSvariety rT = PropertySet.size (PropertySet.collectOfKind rT "mode");
fun subRSvarietyCost qT uP =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0;
        val T = userTh qT uP;
        val rT = RSTableOfQ qT;
        val x = subRSvariety rT
    in sigmoid C W T x
    end;

fun patternComplexity rT =
    let fun mix b d a = b + d + a

        val virtual_trees = map Pattern.
    in
    end;
fun patternComplexityCost qT uP =
    let val C = 2.0 ;
        val W = 2.0 ;
        fun userTh table profile = 2.0;
        val T = userTh qT uP;
        val rT = RSTableOfQ qT;
        val x = patternComplexity rT
    in sigmoid C W T x
    end;

fun symbolConceptMapping qT =  ;
fun symbolConceptMappingCost qT uP =
    let val C = 2.0 ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP;
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

fun inferenceType qT =  ;
fun inferenceTypeCost qT uP =
    let val C = 2.0 ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP;
    in sigmoid C W T x
    end;

fun relationArity qT =  ;
fun relationArityCost qT uP =
    let val C = 2.0 ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP;
    in sigmoid C W T x
    end;

(* (relevant+) squared divided by all tokens, times the average
value for the symbol attributes of types*)
fun registrationSymbol qT = ;
fun registartionSymbolCost qT uP =
    let val C = 2.0 ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP;
    in sigmoid C W T x
    end;

(* instrumental patterns divided by all patterns, times the average
value for the expression attributes of types*)
fun registrationExpression qT = ;
fun registartionExpressionCost qT uP =
    let val C = 2.0 ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP;
    in sigmoid C W T x
    end;

end;
