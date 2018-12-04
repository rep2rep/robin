signature IMPORTANCE =
sig

    datatype importance = Zero | Low | Medium | High;

    val compare : (importance * importance) -> order;
    val fromString : string -> importance option;
    val toString : importance -> string;

end;

structure Importance : IMPORTANCE =
struct

datatype importance = Zero | Low | Medium | High;

fun compare (a, b) = if a = b then EQUAL
                     else case (a, b) of
                              (Zero, _) => LESS
                            | (Low, Zero) => GREATER
                            | (Low, _) => LESS
                            | (Medium, Zero) => GREATER
                            | (Medium, Low) => GREATER
                            | (Medium, _) => LESS
                            | (High, _) => GREATER;

fun fromString "Zero" = SOME Zero
  | fromString "Low" = SOME Low
  | fromString "Medium" = SOME Medium
  | fromString "High" = SOME High
  | fromString _ = NONE;

fun toString Zero = "Zero"
  | toString Low = "Low"
  | toString Medium = "Medium"
  | toString High = "High";

end;
