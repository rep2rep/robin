signature IMPORTANCE =
sig

    datatype importance = Noise | Zero | Low | Medium | High;

    val compare : (importance * importance) -> order;
    val weight : importance -> real;
    val modulate : importance -> real -> real;
    val fromString : string -> importance option;
    val toString : importance -> string;

end;

structure Importance : IMPORTANCE =
struct

datatype importance = Noise | Zero | Low | Medium | High;

fun compare (a, b) = let
    fun ordify Noise = ~1
      | ordify Zero = 0
      | ordify Low = 1
      | ordify Medium = 2
      | ordify High = 3;
in
    Int.compare (ordify a, ordify b)
end;

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



end;
