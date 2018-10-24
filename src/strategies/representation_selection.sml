import "util.logging";
import "util.set";
import "util.dictionary";
import "util.csv";

import "strategies.property_tables";
import "strategies.property_readers"; (* Must come after strategies.property_tables *)

structure RepresentationSelection =
struct

structure StringSet = PropertyTables.S;
val set' = StringSet.fromList;
val subset = StringSet.subset;
fun emptyIntn a b = StringSet.isEmpty (StringSet.intersection a b);

structure StringDict = PropertyTables.D;
fun getValue d k = StringDict.get d k;

(* Read in some data *)

val propertyTableRep' = ref (StringDict.empty ());
val correspondingTable' = ref [];
val propertyTableQ' = ref (StringDict.empty ());

fun init (repTables, corrTables, qTables) = let
    val _ = Logging.write "\n-- Load the representation tables\n";
    val propertyTableRep =
        foldr (fn (a, b) => StringDict.union a b)
              (StringDict.empty ())
              (map PropertyTables.loadRepresentationTable repTables);
    val _ = Logging.write "\n-- Load the correspondence tables\n";
    val correspondingTable =
        foldr (fn (a, b) => a @ b)
              []
              (map PropertyTables.loadCorrespondenceTable corrTables);
    val _ = Logging.write "\n-- Load the question tables\n";
    val propertyTableQ =
        foldr (fn (a, b) => StringDict.union a b)
              (StringDict.empty ())
              (map PropertyTables.loadQuestionTable qTables);
in
    propertyTableRep' := propertyTableRep;
    correspondingTable' := correspondingTable;
    propertyTableQ' := propertyTableQ
end;

fun propertiesRS rep =
    getValue (!propertyTableRep') rep
    handle StringDict.KeyError =>
           (Logging.write ("ERROR: representation '" ^ rep ^ "' not found!\n");
           raise StringDict.KeyError);

fun propertiesQ q =
    getValue (!propertyTableQ') q
    handle StringDict.KeyError =>
           (Logging.write ("ERROR: question named '" ^ q ^ "' not found!\n");
           raise StringDict.KeyError);

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
        val rProps = propertiesRS r;
        val _ = Logging.write ("VAL qProps = " ^ StringSet.toString qProps ^ "\n");
        val _ = Logging.write ("VAL rProps = " ^ StringSet.toString rProps ^ "\n\n");
        val propertyPairs' = List.filter
                                 (fn ((aPlus, aMinus), (bPlus, bMinus), _) =>
                                     (subset aPlus qProps) andalso
                                     (subset bPlus rProps) andalso
                                     (emptyIntn aMinus qProps) andalso
                                     (emptyIntn bMinus rProps)
                                 )
                                 (!correspondingTable');
        val identityPairs = StringSet.map
                                (fn p => ((set' [p], StringSet.empty ()),
                                          (set' [p], StringSet.empty ()),
                                          1.0))
                                (StringSet.intersection qProps rProps);
        val propertyPairs = identityPairs @ propertyPairs';

        val mix = fn (((qpp, qpm), (rpp, rpm), c), s) =>
                     (Logging.write ("CORRESPONDENCE ((" ^
                           (StringSet.toString qpp) ^
                           ", " ^
                           (StringSet.toString qpm) ^
                           "), (" ^
                           (StringSet.toString rpp) ^
                           ", " ^
                           (StringSet.toString rpm) ^
                           ")) -> " ^
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
                     (listToString (fn s => s) representations) ^
                     "\n");
        val influencedRepresentations =
            List.map
                (fn rep => relevanceScore (questionName, rep, 0.0))
                representations;
        val _ = Logging.write ("VAL influencedRepresentations = " ^
                       (listToString
                            (fn (q, r, s) => "(" ^ r ^ ", " ^ (Real.toString s) ^ ")")
                            influencedRepresentations) ^
                       "\n");

        val sort = mergesort (fn ((a, b, c), (x, y, z)) =>
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
             (listToString
                  (fn (r, s) => "(" ^ r ^ ", " ^ (Real.toString s) ^ ")")
                  result
             ) ^
             "\n");
        Logging.dedent ();
        Logging.write ("END topKRepresentations\n\n");
        result
    end;

end;
