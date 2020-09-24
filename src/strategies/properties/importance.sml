signature IMPORTANCE =
sig

    type t = real;

    val High : t;
    val Medium : t;
    val Low : t;
    val Zero : t;
    val Noise : t;


    val equal : (t * t) -> bool;
    val compare : (t * t) -> order;
    val weight : t -> real;
    val modulate : t -> real -> real;
    val fromString : string -> t;
    val toString : t -> string;

    val max : (t * t) -> t;
    val min : (t * t) -> t;

end;

structure Importance : IMPORTANCE =
struct

type t = real;

val Noise = ~1.0;
val Zero = 0.0;
val Low = 0.2;
val Medium = 0.6;
val High = 1.0;

val equal = Real.==;
val compare = Real.compare;
fun weight x = Real.max(0.0,x);
fun modulate x r = (weight x) * r;
fun fromString "Noise" = Noise
  | fromString "Zero" = Zero
  | fromString "Low" = Low
  | fromString "Medium" = Medium
  | fromString "High" = High
  | fromString x = (print ("bad importance string: " ^ x ^ "\n "); raise Match);
fun toString x = if x < 0.0 then "Noise"
            else if Real.== (x, 0.0) then "Zero"
            else if x <= 1.0/3.0 then "Low"
            else if x <= 2.0/3.0 then "Medium"
            else if x <= 1.0 then "High"
            else "Super High (thanks logGravity!)";

val max = Real.max;
val min = Real.min;

end;
