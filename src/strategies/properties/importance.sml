signature IMPORTANCE =
sig

    type importance = real;

    val High : importance;
    val Medium : importance;
    val Low : importance;
    val Zero : importance;
    val Noise : importance;


    val equal : (importance * importance) -> bool;
    val compare : (importance * importance) -> order;
    val weight : importance -> real;
    val modulate : importance -> real -> real;
    val fromString : string -> importance;
    val toString : importance -> string;

    val max : (importance * importance) -> importance;
    val min : (importance * importance) -> importance;

end;

structure Importance : IMPORTANCE =
struct

type importance = real;

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
            else if x > 1.0 then "Super High (thanks logGravity)"
            else (print "bad importance value"; raise Match);

val max = Real.max;
val min = Real.min;

end;
