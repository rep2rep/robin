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

fun listOf reader str = if str = "NONE" orelse str = "" then []
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
        ("laws",
         (listOf labelR, Kind.Law)),
        ("law_imports",
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


    fun importanceOfPrefix p =
        if p = "essential" orelse p = "answer" then Importance.fromString "High" else
        if p = "instrumental" then Importance.fromString "Medium" else
        if p = "relevant" then Importance.fromString "Low" else
        if p = "circumstantial" then Importance.fromString "Zero" else
        if p = "noise" then Importance.fromString "Noise"
        else Importance.fromString "Zero";

    fun qTagToTriple t =
        let val (p,_,k) = Parser.breakOn "_" t
            val importance = if t = "error_allowed" then Importance.fromString "High" else if t = "mode" then Importance.fromString "Zero" else importanceOfPrefix p
            fun keywordToReaderKind s =
                if String.isPrefix "type" s then (listOf typeR, Kind.Type) else
                if String.isPrefix "token" s then (listOf labelR, Kind.Token) else
                if String.isPrefix "pattern" s then (listOf labelR, Kind.Pattern) else
                if String.isPrefix "law" s then (listOf labelR, Kind.Law) else
                if String.isPrefix "tactic" s then (listOf labelR, Kind.Tactic) else
                if String.isPrefix "related" s then keywordToReaderKind (#3 (Parser.breakOn "_" k)) else
                (* treating related equal to present *)
                  raise Match;

            val (reader,kind) =
                if t = "mode" then (listOf labelR, Kind.Mode)
                else if t = "error_allowed" then (labelR, Kind.ErrorAllowed)
                else keywordToReaderKind k handle Match => (Logging.error ("Error reading: "^ t); raise Match)

        in (reader,kind,importance)
        end;


    (* String functions. Shouldn't really be here but meh *)
    fun addPrefixToAll s (s':: L) = (s ^ s') :: addPrefixToAll s L
      | addPrefixToAll _ [] = [];
    fun stringProduct [] L = []
      | stringProduct L [] = L
      | stringProduct (s::L) L' = addPrefixToAll s L' @ stringProduct L L';

    val generate_property_names = fn _ =>
        let val its = ["tokens", "types", "patterns"]
            val its' = ["laws", "tactics"]
            val imps = ["instrumental_", "relevant_"]
            val imps' = ["noise_"]
        in (stringProduct imps (its @ its')) @ (stringProduct imps' its)
        end;

    (* now the available Q properties are generated systematically.
      No "related tactics" or "related laws", because it's nonsense.
      Also no "noise tactics" or "noise laws". *)
    val QProperties = map (fn s => (s, qTagToTriple s)) (["mode",
                                                          "error_allowed",
                                                          "answer_type"]
                                                        @ generate_property_names ());

    val QandRSProperties = [
    ];
in
    fun registerPropertyReaders setQGenerators setRSGenerators =
        (setQGenerators
             (QProperties @ QandRSProperties);
         setRSGenerators
             (RSProperties @ (stripImportance QandRSProperties)))
end;
