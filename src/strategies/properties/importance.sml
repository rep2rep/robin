signature IMPORTANCE =
sig

    (*datatype importance = Noise | Zero | Low | Medium | High;*)
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
  (*)  val fromReal : real -> importance;*)

end;

structure Importance : IMPORTANCE =
struct

type importance = real;

val Noise = ~1.0;
val Zero = 0.0;
val Low = 0.2;
val Medium = 0.6;
val High = 1.0;
(*
datatype importance = Noise | Zero | Low | Medium | High;*)
val equal = Real.==;
val compare = Real.compare;
fun weight x = Real.max(0.0,x);
fun modulate x r = x * r;
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
            else (print "bad importance value"; raise Match);

val max = Real.max;
val min = Real.min;
(*)
fun compare (a, b) = let
    fun ordify Noise = ~1
      | ordify Zero = 0
      | ordify Low = 1
      | ordify Medium = 2
      | ordify High = 3;
in
    Int.compare (ordify a, ordify b)
end;*)
(*
fun weight Noise = 0.0
  | weight Zero = 0.0
  | weight Low = 0.2
  | weight Medium = 0.6
  | weight High = 1.0;

fun modulate Noise x = 0.0
  | modulate Zero x = 0.0
  | modulate Low x = 0.2 * x
  | modulate Medium x = 0.6 * x
  | modulate High x = x;

fun fromString "Noise" = SOME Noise
  | fromString "Zero" = SOME Zero
  | fromString "Low" = SOME Low
  | fromString "Medium" = SOME Medium
  | fromString "High" = SOME High
  | fromString _ = NONE;

fun toString Noise = "Noise"
  | toString Zero = "Zero"
  | toString Low = "Low"
  | toString Medium = "Medium"
  | toString High = "High";

fun fromReal r = if r < 0.0 then Noise
            else if r >= 0.0 andalso r <= 0.0 then Zero
            else if r > 0.0 andalso r <= 0.3 then Low
            else if r > 0.3 andalso r <= 0.7 then Medium
            else High;
*)
end;
