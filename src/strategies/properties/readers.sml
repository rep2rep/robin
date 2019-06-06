import "util.parser";
import "strategies.properties.importance";
import "strategies.properties.property";
import "strategies.properties.kind";

signature PROPERTYREADER =
sig

    exception ReadError of string * string;

    type reader;

    val boolean : reader;
    val number : reader;
    val label : reader;
    val listOf : reader -> reader;
    val dimension : reader;

end;

structure PropertyReader : PROPERTYREADER =
struct

exception ReadError of string * string;

type reader = string -> Property.value list;

fun boolean str =
    if (String.implode (map Char.toLower (String.explode str))) = "true"
    then [Property.Boolean true] else [Property.Boolean false];

fun number str =
    case Int.fromString str of
        SOME n => [Property.Number n]
      | NONE => case str of
                    "\\infty" => [Property.Number ~1]
                  | "na" => [Property.Number ~2]
                  | _ => raise ReadError ("number", str);

fun label str = [Property.Label str];

fun listOf reader str = if str = "NONE" then []
                        else flatmap reader (Parser.splitStrip "," str);

fun collection str = listOf (fn s => [s]) str;
fun dimension str =
    let
        fun parseDimProps s = if s = "{}" then []
                              else Parser.splitStrip ";" (Parser.removeBraces s);
        fun createPairs dimval =
            let
                val parts = Parser.splitStrip ":" dimval;
            in
                case parts of
                    [x, y] => (x, parseDimProps y)
                  | _ => raise ReadError ("dimensions ", dimval)
            end;
        val dimensions = collection str;
        val dimensionsWithValues = map createPairs dimensions;
        val dimensionsSplitOut = map (fn (x, y) =>
                                         map (fn z => x ^ "-" ^ z) y)
                                     dimensionsWithValues;
        val dimensionsNoLabels = map (fn (x, y) =>
                                         map (fn z:string => z) y)
                                     dimensionsWithValues;
    in
        map Property.Label
          (List.concat (dimensionsSplitOut @ dimensionsNoLabels))
    end;

end;


local
    open Importance;
    open PropertyReader;
    fun stripImportance vals = map (fn (l, (f, p, i)) => (l, (f, p))) vals;

    val RSProperties = [
        ("mode",
         (listOf label, Kind.Mode)),
        ("grammar_imports",
         (listOf label, Kind.Import)),
        ("grammatical_complexity",
         (label, Kind.GrammaticalComplexity)),
        ("rigorous",
         (boolean, Kind.Rigorous)),
        ("logical_order",
         (number, Kind.LogicalOrder)),
        ("knowledge_manipulation_system",
         (boolean, Kind.KnowledgeManipulationSystem)),
        ("quantifiers",
         (listOf label, Kind.Quantifier)),
        ("facts",
         (listOf label, Kind.Fact)),
        ("fact_imports",
         (listOf label, Kind.Import)),
        ("tactics",
         (listOf label, Kind.Tactic)),
        ("accessible_tactics",
         (boolean, Kind.AccessibleTactics)),
        ("accessible_facts",
         (boolean, Kind.AccessibleFacts)),
        ("inferential_complexity",
         (number, Kind.InferentialComplexity)),
        ("standard_accessibility_manipulations",
         (listOf label, Kind.StandardAccessibilityManipulations)),
        ("accessible_grammatical_constructors",
         (listOf label, Kind.AccessibleGrammaticalConstructors)),
        ("editable_external_memory",
         (boolean, Kind.EditableExternalMemory)),
        ("physical_dimension_use",
         (dimension, Kind.DimensionUse)),
        ("grammatical_dimensionality",
         (number, Kind.GrammaticalDimensionality)),
        ("grammatical_granularity",
         (label, Kind.GrammaticalGranularity)),
        ("mean_branching_factor",
         (number, Kind.BranchingFactor)),
        ("mean_solution_depth",
         (number, Kind.SolutionDepth)),
        ("limit_construction_size",
         (boolean, Kind.LimitConstructionSize)),
        ("pr_distinct_state_change",
         (label, Kind.PrDistinctStateChange)),
        ("pr_valid_state_change",
         (label, Kind.PrValidStateChange)),
        ("parse_generate_structures",
         (label, Kind.ParseGenerateStructures)),
        ("parse_generate_mapping",
         (label, Kind.ParseGenerateMapping)),
        ("types",
         (listOf label, Kind.Type)),
        ("tokens",
         (listOf label, Kind.Token)),
        ("operators",
         (listOf label, Kind.Token)),
        ("relations",
         (listOf label, Kind.Token)),
        ("patterns",
         (listOf label, Kind.Pattern))
    ];
    val QProperties = [
        ("error_allowed",
         (label, Kind.ErrorAllowed, High)),
        ("answer_type",
         (listOf label, Kind.Type, High)),
        ("instrumental_tokens",
         (listOf label, Kind.Token, Medium)),
        ("instrumental_types",
         (listOf label, Kind.Type, Medium)),
        ("instrumental_patterns",
         (listOf label, Kind.Pattern, Medium)),
        ("instrumental_facts",
         (listOf label, Kind.Fact, Medium)),
        ("instrumental_tactics",
         (listOf label, Kind.Tactic, Medium)),
        ("relevant_tokens",
         (listOf label, Kind.Token, Low)),
        ("relevant_related_tokens",
         (listOf label, Kind.Token, Low)),
        ("num_tokens",
         (number, Kind.NumTokens, Zero)),
        ("num_distinct_tokens",
         (number, Kind.NumDistinctTokens, Zero)),
        ("num_statements",
         (number, Kind.NumStatements, Zero)),
        ("noise_tokens",
         (listOf label, Kind.Token, Noise)),
        ("noise_related_tokens",
         (listOf label, Kind.Token, Noise))
    ];
    val QandRSProperties = [
    ];
in
    fun registerPropertyReaders setQGenerators setRSGenerators =
        (setQGenerators
             (QProperties @ QandRSProperties);
         setRSGenerators
             (RSProperties @ (stripImportance QandRSProperties)))
end;
