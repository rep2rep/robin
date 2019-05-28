import "util.logging";
import "util.set";
import "util.dictionary";
import "util.csv";

import "strategies.properties.property";
import "strategies.properties.tables";
import "strategies.properties.readers"; (* Must come after strategies.property_tables *)
import "strategies.properties.importance";
import "strategies.properties.correspondence";

structure RepresentationSelection =
struct

structure TableDict = Dictionary(struct
                                  type k = string * string;
                                  val compare =
                                      cmpJoin String.compare String.compare;
                                  val fmt =
                                      (fn (s, t) => "(" ^ s ^ ", " ^ t ^ ")");
                                  end);


(* Read in some data *)

val propertyTableRep' = ref (TableDict.empty ());
val correspondingTable' = ref [];
val propertyTableQ' = ref (TableDict.empty ());

fun init (repTables, corrTables, qTables) = let

    val _ = Logging.write "\n-- Load the representation tables\n";
    fun rsTableToDict (rs, props) = TableDict.fromPairList [((rs, rs), props)];
    val propertyTableRep =
        TableDict.unionAll
              (map (fn t => (Logging.write ("LOAD " ^ t ^ "\n");
                             rsTableToDict (PropertyTables.loadRepresentationTable t)))
                   repTables)
        handle TableDict.KeyError => (Logging.error "An RS table has been duplicated"; raise TableDict.KeyError);
    val _ = Logging.write "\n-- Load the correspondence tables\n";

    fun dedupCorrespondences [] = []
      | dedupCorrespondences (x::xs) = let
          fun removeCorr y [] = []
            | removeCorr y (z::zs) =
              if Correspondence.matchingProperties y z
              then (
                  if Correspondence.equal y z then
                      zs
                  else
                      (Logging.error ("ERROR: Conflicting correspondences:\n");
                       Logging.error ("\t" ^
                                      (Correspondence.toString y) ^
                                      "\n");
                       Logging.error ("\t" ^
                                      (Correspondence.toString z) ^
                                      "\n");
                       raise Fail "Conflicting correspondence values")
              )
              else z::(removeCorr y zs);
      in
          x::(dedupCorrespondences (removeCorr x xs))
      end;
    val correspondingTable = dedupCorrespondences (
            List.concat
                (map (fn t => (Logging.write ("LOAD " ^ t ^ "\n");
                               PropertyTables.loadCorrespondenceTable t))
                     corrTables));

    val _ = Logging.write "\n-- Load the question tables\n";
    fun qTableToDict pair = TableDict.fromPairList [pair];
    val propertyTableQ =
        TableDict.unionAll
            (map (fn t => (Logging.write ("LOAD " ^ t ^ "\n");
                           qTableToDict (PropertyTables.loadQuestionTable t)))
                 qTables);

in
    propertyTableRep' := propertyTableRep;
    correspondingTable' := correspondingTable;
    propertyTableQ' := propertyTableQ
end;

fun propertiesRS rep =
    TableDict.get (!propertyTableRep') (rep, rep)
    handle TableDict.KeyError =>
           (Logging.error ("ERROR: representation '" ^ rep ^ "' not found!\n");
           raise TableDict.KeyError);

fun withoutImportance props = PropertySet.fromList (QPropertySet.map (QProperty.withoutImportance) props);

fun propertiesQ q =
    TableDict.get (!propertyTableQ') q
    handle TableDict.KeyError =>
           (Logging.error ("ERROR: question named '" ^ (#1 q) ^ "' not found!\n");
           raise TableDict.KeyError);

fun getQTable q = (q, propertiesQ q);

fun getRSTable rs = (rs, propertiesRS rs);

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
        val _ = Logging.write ("ARG q = " ^ (#1 q) ^ ":" ^ (#2 q) ^ " \n");
        val _ = Logging.write ("ARG r = " ^ r ^ " \n");
        val _ = Logging.write ("ARG s = " ^ (Real.toString s) ^ " \n\n");
        val qProps' = propertiesQ q;
        val qProps = withoutImportance qProps';
        val rProps = propertiesRS r;
        val _ = Logging.write ("VAL qProps = " ^ (QPropertySet.toString qProps') ^ "\n");
        val _ = Logging.write ("VAL rProps = " ^ (PropertySet.toString rProps) ^ "\n\n");
        fun modulate strength importance =
            case importance of
                Importance.Noise => 0.0
              | Importance.Zero => 0.0
              | Importance.Low => 0.2 * strength
              | Importance.Medium => 0.6 * strength
              | Importance.High => strength;
        fun liftImportance c = (c,
                                max (Importance.compare)
                                    (Correspondence.liftImportances qProps' c))
        val propertyPairs =
            let
                fun alreadyCorresponding correspondences corr =
                    List.exists (Correspondence.matchingProperties corr)
                                correspondences;
                val baseCorrs = List.filter
                                    (Correspondence.match qProps rProps)
                                    (!correspondingTable');
                val allIdentities =
                    PropertySet.map
                        Correspondence.identity
                        (PropertySet.collectLeftMatches qProps rProps);
                val identityCorrs =
                    List.filter (fn corr =>
                                    not(alreadyCorresponding baseCorrs corr))
                                allIdentities;
                val correspondences = identityCorrs @ baseCorrs;
            in
                map liftImportance correspondences
            end;
        val strength = Correspondence.strength;
        val sort = mergesort (revCmp (fn ((_, i), (_, i')) => Importance.compare (i, i')));
        val mix = fn ((c, i), s) =>
                     (Logging.write ("CORRESPONDENCE " ^
                           (Correspondence.toString c) ^
                           ", IMPORTANCE " ^
                           (Importance.toString i) ^
                           "\n");
                      Logging.write ("VAL s = " ^
                                     (Real.toString ((modulate (strength c) i) + s)) ^
                                     "\n");
                      ((modulate (strength c) i) + s));
        val s' = List.foldl mix s (sort propertyPairs);
    in
        Logging.write ("\n");
        Logging.write ("RETURN (" ^ (#1 q) ^ ":" ^ (#2 q) ^ ", " ^ r ^ ", " ^ (Real.toString s') ^ ")\n");
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
        val representations = map (fn (r, _) => r) (TableDict.keys (!propertyTableRep'));
        val _ = Logging.write ("VAL representations = " ^
                     (listToString (fn s => s) representations) ^
                     "\n");
        val influencedRepresentations =
            List.map
                (fn rep => relevanceScore (question, rep, 0.0))
                representations;
        val _ = Logging.write ("VAL influencedRepresentations = " ^
                       (listToString
                            (fn (q, r, s) => "(" ^ r ^ ", " ^ (Real.toString s) ^ ")")
                            influencedRepresentations) ^
                       "\n");

        val dropQuestion = fn (_, r, s) => (r, s);
        val sortKey = cmpJoin (revCmp Real.compare) String.compare;
        val sort = mergesort (sortKey o spread flip);
        val getValid = List.filter (fn (_, s) => s > 0.0);
        val topK = fn xs => if k = ~1 then xs
                            else if (List.length xs) <= k then xs
                            else List.take (xs, k);

        val result = topK
                         (sort
                              (getValid
                                   (map dropQuestion influencedRepresentations)));
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
