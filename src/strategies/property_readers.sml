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

import "strategies.property_importance";
import "strategies.property_tables";

structure PropertyReader =
struct

fun bool str = if ((String.implode (map Char.toLower (String.explode str))) = "true")
               then [""] else [];

fun label str = [str];

fun collection str = if str = "NONE" then []
                     else map stringTrim (String.tokens (fn c => c = #",") str);

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
                                  map stringTrim (String.tokens (fn c => c = #";") s')
                              end;
        fun createPairs dimval =
            let
                val parts = map stringTrim (String.tokens (fn c => c = #":") dimval);
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
        List.foldr (fn (a, b) => a @ b) [] (dimensionsSplitOut @ dimensionsNoLabels)
    end;
end;

let
    open Importance;
    fun stripImportance vals = map (fn (l, (f, p, i)) => (l, (f, p))) vals;

    val RSProperties = [
        ("grammar-imports",
         (PropertyReader.collection, "import-")),
        ("parse-generate-structures",
         (PropertyReader.label, "parse-generate-structures-")),
        ("parse-generate-mapping",
         (PropertyReader.label, "parse-generate-mapping-")),
        ("limit-construction-size",
         (PropertyReader.bool, "limit-construction-size")),
        ("grammatical-complexity",
         (PropertyReader.label,  "grammatical-complexity-")),
        ("type-sorts",
         (PropertyReader.collection, "type-sort-")),
        ("rigorous",
         (PropertyReader.bool, "rigorous")),
        ("knowledge-manipulation-system",
         (PropertyReader.bool, "knowledge-manipulation-system")),
        ("facts",
         (PropertyReader.collection, "fact-")),
        ("fact-imports",
         (PropertyReader.collection, "import-")),
        ("tactics",
         (PropertyReader.collection, "tactic-")),
        ("logic-power",
         (PropertyReader.label, "logic-power-")),
        ("standard-accessibility-manipulations",
         (PropertyReader.collection, "accessible-manipulation-")),
        ("accessible-facts",
         (PropertyReader.bool, "accessible-facts")),
        ("accessible-tactics",
         (PropertyReader.bool, "accessible-tactics")),
        ("accessible-grammatical-constructors",
         (PropertyReader.bool, "accessible-grammatical-constructors")),
        ("editable-external-memory",
         (PropertyReader.bool, "editable-external-memory")),
        ("physical-dimension-use",
         (PropertyReader.dimension, "dimension-use-")),
        ("grammatical-dimensionality",
         (PropertyReader.label, "grammatical-dimensionality-")),
        ("grammatical-granularity",
         (PropertyReader.label, "grammatical-granularity-")),
        ("mean-branching-factor",
         (PropertyReader.label, "mean-branching-factor-")),
        ("mean-solution-depth",
         (PropertyReader.label, "mean-solution-depth-")),
        ("pr-distinct-state-change",
         (PropertyReader.label, "pr-distinct-state-change-")),
        ("pr-valid-state-change",
         (PropertyReader.label, "pr-valid-state-change-"))
    ];
    val QProperties = [
        ("num-statements",
         (PropertyReader.label, "num-statements-", Zero)),
        ("num-tokens",
         (PropertyReader.label, "num-tokens-", Zero)),
        ("num-distinct-tokens",
         (PropertyReader.label, "num-distinct-tokens-", Zero)),
        ("syntactic-patterns",
         (PropertyReader.collection, "pattern-", Medium)),
        ("homogeneous",
         (PropertyReader.bool, "homogeneous", Zero)),
        ("error-allowed",
         (PropertyReader.label, "error-allowed-", High)),  (*previously rigorous*)
        ("related-facts",
         (PropertyReader.collection, "fact-", Medium)),
  (*      ("related-facts-import",
         (PropertyReader.collection, "import-", Low)),*)
        ("related-types",
         (PropertyReader.collection, "type-", Low)),
        ("related-operators",
         (PropertyReader.collection, "op-", Low)),
        ("useless-related-tokens",
         (PropertyReader.collection, "token-", Noise)),
        ("useless-related-facts",
         (PropertyReader.collection, "fact-", Noise)),
        ("related-tokens",
         (PropertyReader.collection, "token-", Low)),
        ("related-patterns",
         (PropertyReader.collection, "pattern-", Low)),
        ("variables",
         (PropertyReader.collection, "var-", Medium)),
    (*    ("question-kind",
         (PropertyReader.label, "question-kind-", High)),
        ("answer-kind",
         (PropertyReader.label, "question-kind-", High)),*)
        ("top-question-function",
         (PropertyReader.label, "op-", High)), (*previously question-function*)
        ("top-question-function",
         (PropertyReader.label, "token-", High)), (*previously question-function*)
        ("answer-type",
         (PropertyReader.collection, "type-", High)), (*previously question-value-type*)
        ("dependency-type",
         (PropertyReader.collection, "type-", High))
    ];
    val QandRSProperties = [
        ("quantifiers",
         (PropertyReader.collection, "quantifier-", Medium)),
        ("sentential",
         (PropertyReader.bool, "sentential", Zero)),
        ("logical-order",
         (PropertyReader.label, "logical-order-", Low)),
        ("types",
         (PropertyReader.collection, "type-", Medium)),
        ("tokens",
         (PropertyReader.collection, "token-", Medium)),
        ("relations",
         (PropertyReader.collection, "rel-", Medium)),
        ("operators",
         (PropertyReader.collection, "op-", Medium)),
        ("ranges",
         (PropertyReader.collection, "range-", Medium))
    ];
in
    PropertyTables.setQGenerators
        (QProperties @ QandRSProperties);
    PropertyTables.setRSGenerators
        (RSProperties @ (stripImportance QandRSProperties))
end;
