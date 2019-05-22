signature KIND =
sig
    exception KindError;
    eqtype kind;

    val Token : kind;
    val Type : kind;
    val Fact : kind;
    val Tactic : kind;
    val Pattern : kind;
    val Quantifier : kind;
    val Mode : kind;
    val Import : kind;
    val Rigorous : kind;
    val LogicalOrder : kind;
    val ErrorAllowed : kind;
    val DimensionUse : kind;
    val GrammaticalDimensionality : kind;
    val GrammaticalGranularity : kind;
    val GrammaticalComplexity : kind;
    val InferentialComplexity : kind;
    val StandardAccessibilityManipulations : kind;
    val AccessibleGrammaticalConstructors : kind;
    val AccessibleTactics : kind;
    val AccessibleFacts : kind;
    val KnowledgeManipulationSystem : kind;
    val EditableExternalMemory : kind;
    val BranchingFactor : kind;
    val SolutionDepth : kind;
    val LimitConstructionSize : kind;
    val ParseGenerateStructures : kind;
    val ParseGenerateMapping : kind;
    val PrDistinctStateChange : kind;
    val PrValidStateChange : kind;
    val NumTokens : kind;
    val NumDistinctTokens : kind;
    val NumStatements : kind;

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
val Fact = "fact";
val Tactic = "tactic";
val Pattern = "pattern";
val Quantifier = "quantifier";
val Mode = "mode";
val Import = "import";
val Rigorous = "rigorous";
val LogicalOrder = "logical_order";
val ErrorAllowed = "error_allowed";
val DimensionUse = "physical_dimension_use";
val GrammaticalDimensionality = "grammatical_dimensionality";
val GrammaticalGranularity = "grammatical_granularity";
val GrammaticalComplexity = "grammatical_complexity";
val InferentialComplexity = "inferential_complexity";
val StandardAccessibilityManipulations = "standard_accessibility_manipulation";
val AccessibleGrammaticalConstructors = "accessible_grammatical_constructors";
val AccessibleTactics = "accessible_tactics";
val AccessibleFacts = "accessible_facts";
val KnowledgeManipulationSystem = "knowledge_manipulation_system";
val EditableExternalMemory = "editable_external_memory";
val BranchingFactor = "mean_branching_factor";
val SolutionDepth = "mean_solution_depth";
val LimitConstructionSize = "limit_construction_size";
val ParseGenerateStructures = "parse_generate_structures";
val ParseGenerateMapping = "parse_generate_mapping";
val PrDistinctStateChange = "pr_distinct_state_change";
val PrValidStateChange = "pr_valid_state_change";
val NumTokens = "num_tokens";
val NumDistinctTokens = "num_distinct_tokens";
val NumStatements = "num_statements";

structure StringSet = Set(struct
                           type t = string;
                           val compare = String.compare;
                           val fmt = fn s => s;
                           end);

val allKinds' = StringSet.fromList [Token,
                                    Type,
                                    Fact,
                                    Tactic,
                                    Pattern,
                                    Quantifier,
                                    Mode,
                                    Import,
                                    Rigorous,
                                    LogicalOrder,
                                    ErrorAllowed,
                                    DimensionUse,
                                    GrammaticalDimensionality,
                                    GrammaticalGranularity,
                                    GrammaticalComplexity,
                                    InferentialComplexity,
                                    StandardAccessibilityManipulations,
                                    AccessibleGrammaticalConstructors,
                                    AccessibleTactics,
                                    AccessibleFacts,
                                    KnowledgeManipulationSystem,
                                    EditableExternalMemory,
                                    BranchingFactor,
                                    SolutionDepth,
                                    LimitConstructionSize,
                                    ParseGenerateStructures,
                                    ParseGenerateMapping,
                                    PrDistinctStateChange,
                                    PrValidStateChange,
                                    NumTokens,
                                    NumDistinctTokens
                                   ];
val allKinds = StringSet.toList allKinds';

fun toString k = k;

fun fromString k = if StringSet.contains allKinds' k then k
                   else raise KindError;

fun compare (k, k') = String.compare (k, k');

end;
