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

fun compare (a, b) = let
    fun ordify Zero = 0
      | ordify Low = 1
      | ordify Medium = 2
      | ordify High = 3;
in
    Int.compare (ordify a, ordify b)
end;

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
