import "util/robinlib.sml";
import "util/logging.sml";
import "util/set.sml";
import "util/dictionary.sml";
import "util/csv.sml";

structure RepresentationSelection =
struct

structure StringSet = Set(struct
                           type t = string;
                           val compare = String.compare;
                           val fmt = fn s => s;
                           end);
val cmp = fn a => fn b => String.compare(a, b);
val set' = StringSet.fromList;
val insert' = StringSet.insert;
val subset' = StringSet.subset;

structure StringDict = Dictionary(struct
                                   type k = string;
                                   val compare = String.compare;
                                   end);
val dict' = StringDict.fromPairList;
fun getValue d k = StringDict.get k d;

exception KeyError;
exception TableError;


(* Read in some data *)


fun readProps str = map RobinLib.stringTrim (String.tokens (fn c => c = #",") str);

fun loadTable makeRow filename =
    let
        val _ = Logging.write ("LOAD " ^ filename ^ "\n");
        val csvFile = CSVDefault.openIn filename;
        val csvData = CSVDefault.input csvFile;
    in
        map makeRow csvData
    end
    handle IO.Io e => (print ("ERROR: File '" ^ filename ^ "' could not be loaded\n");
                       raise (IO.Io e))
         | TableError => (
             Logging.write ("ERROR: CSV parsing failed in file '" ^ filename ^ "'\n");
             raise TableError
         );

val loadCorrespondenceTable =
    loadTable (fn row => let
                   val (fst, snd, weight) =
                       case row of
                           [x, y] => (readProps x, readProps y, 1.0)
                         | [x, y, z] =>
                           (case (Real.fromString z) of
                                SOME z' => (readProps x, readProps y, z')
                              | NONE => raise TableError)
                         | _ => raise TableError
               in
                   ((set' fst, set' snd), weight)
               end);

val loadQuestionTable =
    loadTable (fn row => let
                   val (fst, snd) =
                       case row of
                           [x, y] => (x, readProps y)
                         | _ => raise TableError
               in
                   (fst, set' snd)
               end);

val loadRepresentationTable = loadQuestionTable;

val propertyTableRep' = ref StringDict.empty;
val correspondingTable' = ref [];
val propertyTableQ' = ref StringDict.empty;

fun init (repTables, corrTables, qTables) = let
    val _ = Logging.write "\n-- Load the representation tables\n";
    val propertyTableRep =
        foldr (fn (a, b) => StringDict.union a b)
              StringDict.empty
              (map (dict' o loadRepresentationTable) repTables);
    val _ = Logging.write "\n-- Load the correspondence tables\n";
    val correspondingTable =
        foldr (fn (a, b) => a @ b)
              []
              (map loadCorrespondenceTable corrTables);
    val _ = Logging.write "\n-- Load the question tables\n";
    val propertyTableQ =
        foldr (fn (a, b) => StringDict.union a b)
              StringDict.empty
              (map (dict' o loadQuestionTable) qTables);
in
    propertyTableRep' := propertyTableRep;
    correspondingTable' := correspondingTable;
    propertyTableQ' := propertyTableQ
end;

fun propertiesRep rep =
    getValue (!propertyTableRep') rep
    handle KeyError =>
           (Logging.write ("ERROR: representation '" ^ rep ^ "' not found!\n");
           raise KeyError);

fun propertiesQ q =
    getValue (!propertyTableQ') q
    handle KeyError =>
           (Logging.write ("ERROR: question named '" ^ q ^ "' not found!\n");
           raise KeyError);

(*
propInfluence : (question * representation * float) -> (question * representation * float)
For the given question and representation, adjust the score based on
their properties.
*)
fun propInfluence (q, r, s) =
    let
        val _ = Logging.write ("\n");
        val _ = Logging.write ("BEGIN propInfluence\n");
        val _ = Logging.indent ();
        val _ = Logging.write ("ARG q = " ^ q ^ " \n");
        val _ = Logging.write ("ARG r = " ^ r ^ " \n");
        val _ = Logging.write ("ARG s = " ^ (Real.toString s) ^ " \n\n");
        val qProps = propertiesQ q;
        val rProps = propertiesRep r;
        val propertyPairs = List.filter
                                (fn ((a, B), _) => (subset' a qProps)
                                                   andalso (subset' B rProps))
                                (!correspondingTable');

        val mix = fn (((qp, rp), c), s) =>
                     (Logging.write ("CORRESPONDENCE (" ^
                           (StringSet.toString qp) ^
                           ", " ^
                           (StringSet.toString rp) ^
                           ") -> " ^
                           (Real.toString c) ^ "\n");
                      Logging.write ("VAL s = " ^ (Real.toString (c + s)) ^ "\n");
                      (c + s));
        val s' = List.foldl mix s propertyPairs;
    in
        Logging.write ("\n");
        Logging.write ("RETURN (" ^ q ^ ", " ^ r ^ ", " ^ (Real.toString s') ^ ")\n");
        Logging.dedent ();
        Logging.write ("END propInfluence\n\n");
        (q, r, s')
    end;

fun userInfluence (q, r, s) = (q, r, s);

fun taskInfluence (q, r, s) = (q, r, s);

(*
topKRepresentations : question -> int -> (representation * real) list
Determine the best k representations from some known set to attempt to
solve the given question. The limit k can take on the special value -1,
in which case every valid representation is returned.
*)
fun topKRepresentations question k =
    let
        val _ = Logging.write ("\n");
        val _ = Logging.write ("BEGIN topKRepresentations\n");
        val _ = Logging.indent ();
        val (questionName, questionRep) = question;
        val _ = Logging.write ("ARG question = (" ^
                     questionName ^
                     ", " ^
                     questionRep ^
                     ")\n");
        val _ = Logging.write ("ARG k = " ^ (Int.toString k) ^ "\n\n");
        val _ = Logging.write ("VAL questionName = " ^ questionName ^ "\n");
        val _ = Logging.write ("VAL questionRep = " ^ questionRep ^ "\n");
        val relevanceScore = (taskInfluence o userInfluence o propInfluence);
        val _ = Logging.write ("VAL relevanceScore = fn : (q, r, s) -> (q, r, s)\n");
        val representations = StringDict.keys (!propertyTableRep');
        val _ = Logging.write ("VAL representations = " ^
                     (RobinLib.listToString (fn s => s) representations) ^
                     "\n");
        val influencedRepresentations =
            List.map
                (fn rep => relevanceScore (questionName, rep, 0.0))
                representations;
        val _ = Logging.write ("VAL influencedRepresentations = " ^
                       (RobinLib.listToString
                            (fn (q, r, s) => "(" ^ r ^ ", " ^ (Real.toString s) ^ ")")
                            influencedRepresentations) ^
                       "\n");

        val sort = RobinLib.mergesort (fn ((a, b, c), (x, y, z)) =>
                                          if c < z then LESS
                                          else if c > z then GREATER
                                          else EQUAL);
        val getValid = List.filter (fn (_, _, s) => s > 0.0);
        val topK = fn xs => if k = ~1 then xs
                            else if (List.length xs) <= k then xs
                            else List.take (xs, k);
        val getRepWithScore = fn (_, r, s) => (r, s);

        val result = map getRepWithScore
                         (topK
                              (getValid
                                   (List.rev
                                        (sort influencedRepresentations))));
    in
        Logging.write ("\n");
        Logging.write ("RETURN " ^
             (RobinLib.listToString
                  (fn (r, s) => "(" ^ r ^ ", " ^ (Real.toString s) ^ ")")
                  result
             ) ^
             "\n");
        Logging.dedent ();
        Logging.write ("END topKRepresentations\n\n");
        result
    end;

end;
