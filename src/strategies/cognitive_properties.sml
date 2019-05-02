import "strategies.property_analysis";

signature COGNITIVE_PROPERTIES =
sig
  val sigmoid : real -> real -> real -> real -> real;
end;

structure CognitiveProperties : COGNITIVE_PROPERTIES =
struct

(* C and W are meant to be set per property/process, while T is meant to be set
   from the User Profile. *)
fun sigmoid C W T x = 1.0 - (1.0 / (1.0 + Math.pow(C,((x-T)/W))));


(* Cognitive property 1 *)
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

end;
