import "util.logging";
import "util.set";
import "util.listset";
import "util.dictionary";
import "util.csv";

import "strategies.properties.property";
import "strategies.properties.tables";
import "strategies.properties.readers"; (* Must come after strategies.property_tables *)
import "strategies.properties.importance";
import "strategies.properties.cognitive";

import "strategies.correspondences.correspondence";

structure RepresentationSelection =
struct

structure TableDict = Dictionary(struct
                                  type k = string * string;
                                  val compare =
                                      Comparison.join String.compare
                                                      String.compare;
                                  val fmt =
                                      (fn (s, t) => "(" ^ s ^ ", " ^ t ^ ")");
                                  end);


(* Read in some data *)

val _ = registerPropertyReaders
            PropertyTables.setQGenerators
            PropertyTables.setRSGenerators;

val propertyTableRep = ref (TableDict.empty ());
val correspondenceTable = ref [];
val propertyTableQ = ref (TableDict.empty ());

fun init (repTables, corrTables, qTables) = let
    fun dedupCorrespondences cs =
        let fun eq (x, y) =
                if Correspondence.matchingProperties x y
                then if Correspondence.equal x y
                     then true
                     else (Logging.error
                               ("ERROR: Conflicting correspondences:\n");
                           Logging.error ("\t" ^
                                          (Correspondence.toString x) ^
                                          "\n");
                           Logging.error ("\t" ^
                                          (Correspondence.toString y) ^
                                          "\n");
                          raise Fail "Conflicting correspondence values")
                else false;
        in ListSet.removeDuplicates eq cs end;

    val _ = Logging.write "\n-- Load the correspondence tables\n";
    val correspondingTable' = dedupCorrespondences (
            List.concat
                (map (fn t => (Logging.write ("LOAD " ^ t ^ "\n");
                               PropertyTables.loadCorrespondenceTable t))
                     corrTables));

    val _ = Logging.write "\n-- Load the representation tables\n";
    val propertyTableRep' =
        let fun loadTable tName =
                let val _ = Logging.write ("LOAD " ^ tName ^ "\n");
                    val (rs, props) = PropertyTables.loadRepresentationTable tName;
                in ((rs, rs), props) end;
        in TableDict.fromPairList (map loadTable repTables) end
        handle TableDict.KeyError => (
            Logging.error "An RS table has been duplicated";
            raise TableDict.KeyError);

    val _ = Logging.write "\n-- Load the question tables\n";
    val propertyTableQ' =
        let fun loadTable tName =
                let val _ = Logging.write ("LOAD " ^ tName ^ "\n");
                    val (q, props) = PropertyTables.loadQuestionTable tName;
                in (q, props) end;
        in TableDict.fromPairList (map loadTable qTables) end
        handle TableDict.KeyError => (
            Logging.error "A Q table has been duplicated";
            raise TableDict.KeyError);

in
    propertyTableRep := propertyTableRep';
    correspondenceTable := correspondingTable';
    propertyTableQ := propertyTableQ'
end;

fun getRSDescriptionFor rep =
    TableDict.get (!propertyTableRep) (rep, rep)
    handle TableDict.KeyError =>
           (Logging.error ("ERROR: representation '" ^ rep ^ "' not found!\n");
           raise TableDict.KeyError);

fun getQDescriptionFor q =
    TableDict.get (!propertyTableQ) q
    handle TableDict.KeyError =>
           (Logging.error ("ERROR: question named '" ^ (#1 q) ^ "' not found!\n");
           raise TableDict.KeyError);

fun getAllRSs () =
    map (fn (r, _) => r) (TableDict.keys (!propertyTableRep))

fun getCorrespondences () =
    !correspondenceTable;


(*
informationalSuitability : (question * representation * score) -> (question * representation * score)
For the given question and representation, adjust the score based on
their properties.
*)
fun informationalSuitability (q, r) =
    let
        val _ = Logging.write ("\n");
        val _ = Logging.write ("BEGIN informationalSuitability\n");
        val _ = Logging.indent ();
        val _ = Logging.write ("ARG q = " ^ (#1 q) ^ ":" ^ (#2 q) ^ " \n");
        val _ = Logging.write ("ARG r = " ^ r ^ " \n");
        val qProps = getQDescriptionFor q;
        val rProps = getRSDescriptionFor r;
        val _ = Logging.write ("VAL qProps = " ^ (QPropertySet.toString qProps) ^ "\n");
        val _ = Logging.write ("VAL rProps = " ^ (PropertySet.toString rProps) ^ "\n\n");

        val baseMatches =
            let fun liftImportance c =
                    (c, List.max (Importance.compare)
                            (Correspondence.liftImportances qProps c));
                val correspondences = CorrespondenceList.allMatches
                                          (getCorrespondences ())
                                          qProps rProps;
            in map liftImportance correspondences end;

        val typeMatches = CorrespondenceList.typeCorrespondences
                              baseMatches qProps;
        val matches = baseMatches @ typeMatches;
        val matchGroup = CorrespondenceList.mrmc matches qProps rProps;

        val modulate = Importance.modulate;
        val strength = Correspondence.strength;
        val mix = fn (c, i) =>
                     let
                         val s = (modulate i (strength c));
                         (* Logging information *)
                         val cs = Correspondence.toString c;
                         val is = Importance.toString i;
                         val ss = Real.toString s;
                         val _ = Logging.write ("CORRESPONDENCE "
                                                ^ cs
                                                ^ ", IMPORTANCE "
                                                ^ is
                                                ^ "\n");
                         val _ = Logging.write ("VAL s = "
                                                ^ ss
                                                ^ "\n");
                     in
                         s
                     end;
        (* Sort correspondences from most to least important *)
        val sort = List.mergesort
                       (Comparison.rev (fn ((_, i), (_, i')) =>
                                   Importance.compare (i, i')));
        val s = List.sumIndexed mix (sort matchGroup);
    in
        Logging.write ("\n");
        Logging.write ("RETURN ("
                       ^ (#1 q) ^ ":" ^ (#2 q)
                       ^ ", " ^ r ^ ", "
                       ^ (Real.toString s) ^ ")\n");
        Logging.dedent ();
        Logging.write ("END informationalSuitability\n\n");
        (q, r, s)
    end;

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

        val representations = getAllRSs ();
        val _ = Logging.write ("VAL representations = " ^
                     (List.toString (fn s => s) representations) ^
                     "\n");
        val influencedRepresentations =
            List.map
                (fn rep => informationalSuitability (question, rep))
                representations;
        val _ = Logging.write ("VAL influencedRepresentations = " ^
                       (List.toString
                            (fn (q, r, s) => "(" ^ r ^ ", " ^ (Real.toString s) ^ ")")
                            influencedRepresentations) ^
                       "\n");

        val dropQuestion = fn (_, r, s) => (r, s);
        val sortKey = Comparison.join (Comparison.rev Real.compare) String.compare;
        val sort = List.mergesort (sortKey o mappair flip);
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
