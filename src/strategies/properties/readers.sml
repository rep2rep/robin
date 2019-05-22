import "util.parser";
import "strategies.properties.importance";
import "strategies.properties.tables";

signature PROPERTYREADER =
sig

    type reader;

    val boolean : reader;
    val number : reader;
    val label : reader;
    val listOf : reader -> reader;
    val dimension : reader;

end;

structure PropertyReader : PROPERTYREADER =
struct

type reader = string -> Property.value list;

fun boolean str =
    if (String.implode (map Char.toLower (String.explode str))) = "true"
    then [Property.Boolean true] else [Property.Boolean false];

fun number str =
    case Int.fromString str of
        SOME n => [Property.Number n]
      | NONE => if str = "\\infty" then [Property.Number ~1] else raise PropertyTables.TableError ("Unable to read number from " ^ str);

fun label str = [Property.Label str];

fun listOf reader str = if str = "NONE" then []
                        else flatmap reader (Parser.splitStrip "," str);

fun collection str = listOf (fn s => [s]) str;
fun dimension str =
    let
        fun parseDimProps s = if s = "{}" then []
                              else let
                                  fun dropEnds [] = []
                                    | dropEnds [x] = []
                                    | dropEnds [x, y] = []
                                    | dropEnds (x::xs) = List.rev (List.tl (List.rev xs));
                                  val s' = String.implode (dropEnds (String.explode s));
                              in
                                  Parser.splitStrip ";" s'
                              end;
        fun createPairs dimval =
            let
                val parts = Parser.splitStrip ":" dimval;
            in
                case parts of
                    [x, y] => (x, parseDimProps y)
                  | _ => raise PropertyTables.TableError
                               ("Unable to read dimensions from " ^ dimval)
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


let
    open Importance;
    open PropertyReader;
    fun stripImportance vals = map (fn (l, (f, p, i)) => (l, (f, p))) vals;

    val tokenKind = Property.kindOfString "token";
    val typeKind = Property.kindOfString "type";
    val factKind = Property.kindOfString "fact";
    val tacticKind = Property.kindOfString "tactic";
    val patternKind = Property.kindOfString "pattern";
    val modeKind = Property.kindOfString "mode";
    val importKind = Property.kindOfString "import";
    val gComplexityKind = Property.kindOfString "grammatical_complexity";
    val rigorousKind = Property.kindOfString "rigorous";
    val iComplexityKind = Property.kindOfString "inferential_complexity";
    val samKind = Property.kindOfString "standard_accessibility_manipulation";
    val kmsKind = Property.kindOfString "knowledge_manipulation_system";
    val eemKind = Property.kindOfString "editable_external_memory";
    val errorKind = Property.kindOfString "error_allowed";
    val dimensionUseKind = Property.kindOfString "dimension_use";
    val gDimensionalityKind = Property.kindOfString "grammatical_dimensionality";
    val gGranularityKind = Property.kindOfString "grammatical_granularity";
    val branchingFactorKind = Property.kindOfString "mean_branching_factor";
    val prDistinctKind = Property.kindOfString "pr_distinct_state_change";
    val prValidKind = Property.kindOfString "pr_valid_state_change";
    val numTokensKind = Property.kindOfString "num_tokens";
    val numDistinctTokensKind = Property.kindOfString "num_distinct_tokens";

    val RSProperties = [
        ("mode", (listOf label, modeKind)),
        ("grammar_imports", (listOf label, importKind)),
        ("grammatical_complexity", (label, gComplexityKind)),
        ("rigorous", (boolean, rigorousKind)),
        ("knowledge_manipulation_system", (boolean, kmsKind)),
        ("facts", (listOf label, factKind)),
        ("fact_imports", (listOf label, importKind)),
        ("tactics", (listOf label, tacticKind)),
        ("inferential_complexity", (number, iComplexityKind)),
        ("standard_accessibility_manipulations", (listOf label, samKind)),
        ("editable_external_memory", (boolean, eemKind)),
        ("physical_dimension_use", (dimension, dimensionUseKind)),
        ("grammatical_dimensionality", (number, gDimensionalityKind)),
        ("grammatical_granularity", (label, gGranularityKind)),
        ("mean_branching_factor", (number, branchingFactorKind)),
        ("pr_distinct_state_change", (label, prDistinctKind)),
        ("pr_valid_state_change", (label, prValidKind)),
        ("types", (listOf label, typeKind)),
        ("tokens", (listOf label, tokenKind)),
        ("operators", (listOf label, tokenKind)),
        ("relations", (listOf label, tokenKind)),
        ("patterns", (listOf label, patternKind))
    ];
    val QProperties = [
        ("error_allowed",
         (label, errorKind, High)),
        ("answer_type",
         (listOf label, typeKind, High)),
        ("instrumental_tokens",
         (listOf label, tokenKind, Medium)),
        ("instrumental_types",
         (listOf label, typeKind, Medium)),
        ("instrumental_patterns",
         (listOf label, patternKind, Medium)),
        ("instrumental_facts",
         (listOf label, factKind, Medium)),
        ("instrumental_tactics",
         (listOf label, tacticKind, Medium)),
        ("relevant_tokens",
         (listOf label, tokenKind, Low)),
        ("relevant_related_tokens",
         (listOf label, tokenKind, Low)),
        ("num_tokens",
         (number, numTokensKind, Zero)),
        ("num_distinct_tokens",
         (number, numDistinctTokensKind, Zero)),
        ("noise_tokens",
         (listOf label, tokenKind, Noise)),
        ("noise_related_tokens",
         (listOf label, tokenKind, Noise))
    ];
    val QandRSProperties = [
    ];
in
    PropertyTables.setQGenerators
        (QProperties @ QandRSProperties);
    PropertyTables.setRSGenerators
        (RSProperties @ (stripImportance QandRSProperties))
end;
