import "strategies.property_analysis";

signature COGNITIVE_PROPERTIES =
sig
  val sigmoid : real -> real -> real -> real -> real;
end;

structure CognitiveProperties : COGNITIVE_PROPERTIES =
struct

fun sigmoid C W T x = 1.0 - (1.0/(1.0+Math.pow(C,((x-T)/W))));

fun numberOfSymbols qT =  ;
fun numberOfSymbolsCost qT uP =
    let val C =  ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP
    in sigmoid C W T x
    end;

fun numberOfSymbolTypes qT =  ;
fun numberOfSymbolTypesCost qT uP =
    let val C =  ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP
    in sigmoid C W T x
    end;

fun subRSvariety qT = PropertySet.collectOfKind qT "mode" ;
fun subRSvarietyCost qT uP =
    let val C =  ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP
    in sigmoid C W T x
    end;

fun symbolConceptMapping qT =  ;
fun symbolConceptMappingCost qT uP =
    let val C =  ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP
    in sigmoid C W T x
    end;

fun quantityScale qT =  ;
fun quantityScaleCost qT uP =
    let val C =  ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP
    in sigmoid C W T x
    end;

fun inferenceType qT =  ;
fun inferenceTypeCost qT uP =
    let val C =  ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP
    in sigmoid C W T x
    end;

fun relationArity qT =  ;
fun relationArityCost qT uP =
    let val C =  ;
        val W =  ;
        fun userTh table profile =  ;
        val T = userTh qT uP
    in sigmoid C W T x
    end;

end;
