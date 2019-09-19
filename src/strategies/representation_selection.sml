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
                                      Comparison.join String.compare String.compare;
                                  val fmt =
                                      (fn (s, t) => "(" ^ s ^ ", " ^ t ^ ")");
                                  end);


(* Read in some data *)

val _ = registerPropertyReaders
            PropertyTables.setQGenerators
            PropertyTables.setRSGenerators;

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
        val qProps = QPropertySet.withoutImportances qProps';
        val rProps = propertiesRS r;
        val _ = Logging.write ("VAL qProps = " ^ (QPropertySet.toString qProps') ^ "\n");
        val _ = Logging.write ("VAL rProps = " ^ (PropertySet.toString rProps) ^ "\n\n");

        val matches =
            let fun liftImportance c =
                    (c, List.max (Importance.compare)
                            (Correspondence.liftImportances qProps' c));
                val correspondences = allCorrespondenceMatches (!correspondingTable')
                                                               qProps rProps;
            in map liftImportance correspondences end;

        val modulate = Importance.modulate;
        val strength = Correspondence.strength;
        (* Sort correspondences from most to least important *)
        val sort = List.mergesort
                       (Comparison.rev (fn ((_, i), (_, i')) =>
                                   Importance.compare (i, i')));
        val mix = fn ((c, i), s) =>
                     let
                         val s' = s + (modulate i (strength c));
                         (* Logging information *)
                         val cs = Correspondence.toString c;
                         val is = Importance.toString i;
                         val ss = Real.toString s';
                         val _ = Logging.write ("CORRESPONDENCE "
                                                ^ cs
                                                ^ ", IMPORTANCE "
                                                ^ is
                                                ^ "\n");
                         val _ = Logging.write ("VAL s = "
                                                ^ ss
                                                ^ "\n");
                     in
                         s'
                     end;
        val s' = List.foldl mix s (sort matches);
    in
        Logging.write ("\n");
        Logging.write ("RETURN ("
                       ^ (#1 q) ^ ":" ^ (#2 q)
                       ^ ", " ^ r ^ ", "
                       ^ (Real.toString s') ^ ")\n");
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
                     (List.toString (fn s => s) representations) ^
                     "\n");
        val influencedRepresentations =
            List.map
                (fn rep => relevanceScore (question, rep, 0.0))
                representations;
        val _ = Logging.write ("VAL influencedRepresentations = " ^
                       (List.toString
                            (fn (q, r, s) => "(" ^ r ^ ", " ^ (Real.toString s) ^ ")")
                            influencedRepresentations) ^
                       "\n");

        val dropQuestion = fn (_, r, s) => (r, s);
        val sortKey = Comparison.join (Comparison.rev Real.compare) String.compare;
        val sort = List.mergesort (sortKey o spread flip);
        val getValid = List.filter (fn (_, s) => s > 0.0);
        val topK = fn xs => if k = ~1 then xs
                            else if (List.length xs) <= k then xs
                            else List.take (xs, k);
        val result = (topK o sort o getValid o map dropQuestion)
                         influencedRepresentations;
    in
        Logging.write ("\n");
        Logging.write ("RETURN " ^
             (List.toString
                  (fn (r, s) => "(" ^ r ^ ", " ^ (Real.toString s) ^ ")")
                  result
             ) ^
             "\n");
        Logging.dedent ();
        Logging.write ("END topKRepresentations\n\n");
        result
    end;

end;
