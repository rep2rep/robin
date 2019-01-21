signature IMPORTANCE =
sig

    datatype importance = Noise | Zero | Low | Medium | High;

    val compare : (importance * importance) -> order;
    val fromString : string -> importance option;
    val toString : importance -> string;

end;

structure Importance : IMPORTANCE =
struct

datatype importance = Noise | Zero | Low | Medium | High;

fun compare (a, b) = if a = b then EQUAL
                     else case (a, b) of
                              (Noise, _) => LESS
                            | (_, Noise) => GREATER
                            | (Zero, _) => LESS
                            | (_, Zero) => GREATER
                            | (Low, _) => LESS
                            | (_, Low) => GREATER
                            | (Medium, _) => LESS
                            | (_, Medium) => GREATER
                            | (High, _) => GREATER;

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
