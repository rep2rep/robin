import "util.parser";
import "strategies.properties.importance";
import "strategies.properties.property";
import "strategies.properties.kind";

signature PROPERTYREADER =
sig

    exception ReadError of string * string;

    type reader;

    val booleanR : reader;
    val numberR : reader;
    val typeR : reader;
    val labelR : reader;
    val listOf : reader -> reader;
    val dimension : reader;

end;

structure PropertyReader : PROPERTYREADER =
struct

exception ReadError of string * string;

type reader = string -> (Property.value * Attribute.T list) list;

fun booleanR str =
    if (String.implode (map Char.toLower (String.explode str))) = "true"
    then [(Property.Boolean true,[])] else [(Property.Boolean false,[])];

fun numberR str =
    case Int.fromString str of
        SOME n => [(Property.Number n,[])]
      | NONE => case str of
                    "\\infty" => [(Property.Number ~1,[])]
                  | "na" => [(Property.Number ~2,[])]
                  | _ => raise ReadError ("number", str);

fun typeR str =
    let val (v,A) = Property.findAttributes str
    in [(Property.Type (Type.fromString v), A)]
    end;

fun labelR str =
    let val (v,A) = Property.findAttributes str
    in [(Property.Label v, A)]
    end;

fun listOf reader str = if str = "NONE" then []
                        else List.flatmap reader (Parser.splitStrip "," str);

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
        map (fn x => (Property.Label x, []))
          (List.concat (dimensionsSplitOut @ dimensionsNoLabels))
    end;

end;


local
    open Importance;
    open PropertyReader;
    fun stripImportance vals = map (fn (l, (f, p, i)) => (l, (f, p))) vals;

    val RSProperties = [
        ("mode",
         (listOf labelR, Kind.Mode)),
        ("token_imports",
         (listOf labelR, Kind.Import)),
        ("grammatical_complexity",
         (labelR, Kind.GrammaticalComplexity)),
        ("rigorous",
         (booleanR, Kind.Rigorous)),
        ("facts",
         (listOf labelR, Kind.Fact)),
        ("fact_imports",
         (listOf labelR, Kind.Import)),
        ("tactics",
         (listOf labelR, Kind.Tactic)),
        ("inferential_complexity",
         (numberR, Kind.InferentialComplexity)),
        ("physical_dimension_use",
         (dimension, Kind.DimensionUse)),
        ("types",
         (listOf typeR, Kind.Type)),
        ("tokens",
         (listOf labelR, Kind.Token)),
        ("patterns",
         (listOf labelR, Kind.Pattern))
    ];
    val QProperties = [
        ("error_allowed",
         (labelR, Kind.ErrorAllowed, High)),
        ("answer_type",
         (listOf typeR, Kind.Type, High)),
        ("instrumental_tokens",
         (listOf labelR, Kind.Token, Medium)),
        ("instrumental_types",
         (listOf typeR, Kind.Type, Medium)),
        ("instrumental_patterns",
         (listOf labelR, Kind.Pattern, Medium)),
        ("instrumental_facts",
         (listOf labelR, Kind.Fact, Medium)),
        ("instrumental_tactics",
         (listOf labelR, Kind.Tactic, Medium)),
        ("relevant_tokens",
         (listOf labelR, Kind.Token, Low)),
        ("relevant_related_tokens",
         (listOf labelR, Kind.Token, Low)),
        ("num_tokens",
         (numberR, Kind.NumTokens, Zero)),
        ("num_distinct_tokens",
         (numberR, Kind.NumDistinctTokens, Zero)),
        ("noise_tokens",
         (listOf labelR, Kind.Token, Noise)),
        ("noise_related_tokens",
         (listOf labelR, Kind.Token, Noise))
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
