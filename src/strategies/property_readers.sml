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

PropertyTables.setGenerators [
    ("sentential",
     (PropertyReader.bool, "sentential")),
    ("logical-order",
     (PropertyReader.label, "logical-order-")),
    ("quantifiers",
     (PropertyReader.collection, "quantifier-")),
    ("types",
     (PropertyReader.collection, "type-")),
    ("tokens",
     (PropertyReader.collection, "token-")),
    ("relations",
     (PropertyReader.collection, "rel-")),
    ("operators",
     (PropertyReader.collection, "op-")),
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
    ("ranges",
     (PropertyReader.collection, "range-")),
    ("type-sorts",
     (PropertyReader.collection, "type-sort-")),
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
    ("num-statements",
     (PropertyReader.label, "num-statements-")),
    ("num-tokens",
     (PropertyReader.label, "num-tokens-")),
    ("num-distinct-tokens",
     (PropertyReader.label, "num-distinct-tokens-")),
    ("syntactic-patterns",
     (PropertyReader.collection, "pattern-")),
    ("homogeneous",
     (PropertyReader.bool, "homogeneous")),
    ("rigorous",
     (PropertyReader.bool, "rigorous")),
    ("related-facts",
     (PropertyReader.collection, "fact-")),
    ("related-facts-import",
     (PropertyReader.collection, "import-")),
    ("related-types",
     (PropertyReader.collection, "type-")),
    ("related-operators",
     (PropertyReader.collection, "op-")),
    ("related-tokens",
     (PropertyReader.collection, "token-")),
    ("related-patterns",
     (PropertyReader.collection, "pattern-")),
    ("variables",
     (PropertyReader.collection, "var-")),
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
    ("pr-distinct-state-change",
     (PropertyReader.label, "pr-distinct-state-change-")),
    ("pr-valid-state-change",
     (PropertyReader.label, "pr-valid-state-change-")),
    ("mean-solution-depth",
     (PropertyReader.label, "mean-solution-depth-")),
    ("question-kind",
     (PropertyReader.label, "question-kind-")),
    ("answer-kind",
     (PropertyReader.label, "question-kind-")),
    ("question-function",
     (PropertyReader.label, "question-function-")),
    ("question-value-type",
     (PropertyReader.collection, "type-")),
    ("dependency-type",
     (PropertyReader.collection, "type-"))
];
