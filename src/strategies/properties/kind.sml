signature KIND =
sig
    exception KindError;
    eqtype kind;

    val Token : kind;
    val Type : kind;
    val Law : kind;
    val Tactic : kind;
    val Pattern : kind;
    val Mode : kind;
    val Import : kind;
    val Rigorous : kind;
    val ErrorAllowed : kind;
    val DimensionUse : kind;
    val GrammaticalComplexity : kind;
    val InferentialComplexity : kind;
    val NumTokens : kind;
    val NumDistinctTokens : kind;

    val allKinds : kind list;

    val toString : kind -> string;
    val fromString : string -> kind;

    val compare : kind * kind -> order;

end;

structure Kind :> KIND =
struct

exception KindError;
type kind = string;

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
