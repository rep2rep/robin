(*
The following functions handle how properties are generated from the "property"
table. For example, they key "operators" then lists a collection of operators.
To read this, we use the "readCollection" function and will prepend each of the
operators with the string "op-". Compare this with basic labels, which simply
return the one thing that is there, in a list, ready to prepend to. Bools are
simplest, either returning an empty list (false), or a list containing the empty
string (true) to generate either the key, or nothing, as a property.
A concrete example: From the table
    operators        +, -, *, \sqrt
    sentential       true
    logic-power      2
we would generate the properties
    op-+, op--, op-*, op-\sqrt, sentential, logic-power-2
*)

import "util.parser";
import "strategies.properties.importance";
import "strategies.properties.tables";

structure PropertyReader =
struct

fun boolean str =
    if (String.implode (map Char.toLower (String.explode str))) = "true"
    then [Property.Boolean true] else [Property.Boolean false];

fun number str =
    case Int.fromString str of
        SOME n => [Property.Number n]
      | NONE => if str = "\\infty" then [Property.Number ~1] else raise PropertyTables.TableError ("Unable to read number from " ^ str);

fun label str = [Property.Label str];

fun collection str = if str = "NONE" then []
                     else map Property.Label (Parser.splitStrip "," str);

fun collection' str = if str = "NONE" then []
                    else Parser.splitStrip "," str;
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
        val dimensions = collection' str;
        val dimensionsWithValues = map createPairs dimensions;
        val dimensionsSplitOut = map (fn (x, y) =>
                                         map (fn z => x ^ "-" ^ z) y)
                                     dimensionsWithValues;
        val dimensionsNoLabels = map (fn (x, y) =>
                                         map (fn z:string => z) y)
                                     dimensionsWithValues;
    in
        map Property.Label
          (List.foldr (fn (a, b) => a @ b) [] (dimensionsSplitOut @ dimensionsNoLabels))
    end;

end;


let
    open Importance;
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
        ("mode", (PropertyReader.collection, modeKind)),
        ("grammar_imports", (PropertyReader.collection, importKind)),
        ("grammatical_complexity", (PropertyReader.label, gComplexityKind)),
        ("rigorous", (PropertyReader.boolean, rigorousKind)),
        ("knowledge_manipulation_system", (PropertyReader.boolean, kmsKind)),
        ("facts", (PropertyReader.collection, factKind)),
        ("fact_imports", (PropertyReader.collection, importKind)),
        ("tactics", (PropertyReader.collection, tacticKind)),
        ("inferential_complexity", (PropertyReader.number, iComplexityKind)),
        ("standard_accessibility_manipulations", (PropertyReader.collection, samKind)),
        ("editable_external_memory", (PropertyReader.boolean, eemKind)),
        ("physical_dimension_use", (PropertyReader.dimension, dimensionUseKind)),
        ("grammatical_dimensionality", (PropertyReader.number, gDimensionalityKind)),
        ("grammatical_granularity", (PropertyReader.label, gGranularityKind)),
        ("mean_branching_factor", (PropertyReader.number, branchingFactorKind)),
        ("pr_distinct_state_change", (PropertyReader.label, prDistinctKind)),
        ("pr_valid_state_change", (PropertyReader.label, prValidKind)),
        ("types", (PropertyReader.collection, typeKind)),
        ("tokens", (PropertyReader.collection, tokenKind)),
        ("operators", (PropertyReader.collection, tokenKind)),
        ("relations", (PropertyReader.collection, tokenKind)),
        ("patterns", (PropertyReader.collection, patternKind))
    ];
    val QProperties = [
        ("error_allowed",
         (PropertyReader.label, errorKind, High)),
        ("answer_type",
         (PropertyReader.collection, typeKind, High)),
        ("instrumental_tokens",
         (PropertyReader.collection, tokenKind, Medium)),
        ("instrumental_types",
         (PropertyReader.collection, typeKind, Medium)),
        ("instrumental_patterns",
         (PropertyReader.collection, patternKind, Medium)),
        ("instrumental_facts",
         (PropertyReader.collection, factKind, Medium)),
        ("instrumental_tactics",
         (PropertyReader.collection, tacticKind, Medium)),
        ("relevant_tokens",
         (PropertyReader.collection, tokenKind, Low)),
        ("relevant_related_tokens",
         (PropertyReader.collection, tokenKind, Low)),
        ("num_tokens",
         (PropertyReader.number, numTokensKind, Zero)),
        ("num_distinct_tokens",
         (PropertyReader.number, numDistinctTokensKind, Zero)),
        ("noise_tokens",
         (PropertyReader.collection, tokenKind, Noise)),
        ("noise_related_tokens",
         (PropertyReader.collection, tokenKind, Noise))
    ];
    val QandRSProperties = [
    ];
in
    PropertyTables.setQGenerators
        (QProperties @ QandRSProperties);
    PropertyTables.setRSGenerators
        (RSProperties @ (stripImportance QandRSProperties))
end;
