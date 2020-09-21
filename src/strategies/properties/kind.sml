signature KIND =
sig
    exception KindError;
    eqtype t;

    val Dummy : t;
    val Token : t;
    val Type : t;
    val Law : t;
    val Tactic : t;
    val Pattern : t;
    val Mode : t;
    val Import : t;
    val Rigorous : t;
    val ErrorAllowed : t;
    val DimensionUse : t;
    val GrammaticalComplexity : t;
    val InferentialComplexity : t;
    val NumTokens : t;
    val NumDistinctTokens : t;

    val allKinds : t list;

    val toString : t -> string;
    val fromString : string -> t;

    val compare : t * t -> order;

end;

structure Kind :> KIND =
struct

exception KindError;
type t = string;

val Dummy = "__";
val Token = "token";
val Type = "type";
val Law = "law";
val Tactic = "tactic";
val Pattern = "pattern";
val Mode = "mode";
val Import = "import";
val Rigorous = "rigorous";
val ErrorAllowed = "error_allowed";
val DimensionUse = "physical_dimension_use";
val GrammaticalComplexity = "grammatical_complexity";
val InferentialComplexity = "inferential_complexity";
val NumTokens = "num_tokens";
val NumDistinctTokens = "num_distinct_tokens";

structure StringSet = Set(struct
                           type t = string;
                           val compare = String.compare;
                           val fmt = fn s => s;
                           end);

val allKinds' = StringSet.fromList [Token,
                                    Type,
                                    Law,
                                    Tactic,
                                    Pattern,
                                    Mode,
                                    Import,
                                    Rigorous,
                                    ErrorAllowed,
                                    DimensionUse,
                                    GrammaticalComplexity,
                                    InferentialComplexity,
                                    NumTokens,
                                    NumDistinctTokens
                                   ];
val allKinds = StringSet.toList allKinds';

fun toString k = k;

fun fromString k = if StringSet.contains allKinds' k then k
                   else raise KindError;

fun compare (k, k') = String.compare (k, k');

end;
