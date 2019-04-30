import "util/type";
import "strategies/property_tables";

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



end;
