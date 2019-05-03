import "util.logging";
import "util.set";
import "util.dictionary";
import "util.csv";

import "strategies.property";
import "strategies.property_tables";
import "strategies.property_readers"; (* Must come after strategies.property_tables *)
import "strategies.property_importance";
import "strategies.property_correspondence";

structure RepresentationSelection =
struct

structure FileDict = PropertyTables.FileDict;

(* Read in some data *)

val propertyTableRep' = ref (FileDict.empty ());
val correspondingTable' = ref [];
val propertyTableQ' = ref (FileDict.empty ());

fun init (repTables, corrTables, qTables) = let
    val _ = Logging.write "\n-- Load the representation tables\n";
    val propertyTableRep =
        FileDict.unionAll
            (map (fn t => (Logging.write ("LOAD " ^ t ^ "\n");
                           PropertyTables.loadRepresentationTable t)) repTables)
        handle FileDict.KeyError => (Logging.error "An RS table has been duplicated"; raise FileDict.KeyError);
    val _ = Logging.write "\n-- Load the correspondence tables\n";
    val correspondingTable =
        List.concat
            (map (fn t => (Logging.write ("LOAD " ^ t ^ "\n");
                           PropertyTables.loadCorrespondenceTable t)) corrTables);
    val _ = Logging.write "\n-- Load the question tables\n";
    val propertyTableQ =
        FileDict.unionAll
              (map (fn t => (Logging.write ("LOAD " ^ t ^ "\n");
                             PropertyTables.loadQuestionTable t)) qTables);
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
in
    propertyTableRep' := propertyTableRep;
    correspondingTable' := dedupCorrespondences correspondingTable;
    propertyTableQ' := propertyTableQ
end;

fun propertiesRS rep =
    FileDict.get (!propertyTableRep') rep
    handle FileDict.KeyError =>
           (Logging.error ("ERROR: representation '" ^ rep ^ "' not found!\n");
           raise FileDict.KeyError);

fun withoutImportance props = PropertySet.fromList (QPropertySet.map (QProperty.withoutImportance) props);

fun propertiesQ q =
    FileDict.get (!propertyTableQ') q
    handle FileDict.KeyError =>
           (Logging.error ("ERROR: question named '" ^ q ^ "' not found!\n");
           raise FileDict.KeyError);

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
        val qProps' = propertiesQ q;
        val qProps = withoutImportance qProps';
        val rProps = propertiesRS r;
        val _ = Logging.write ("VAL qProps = " ^ (QPropertySet.toString qProps') ^ "\n");
        val _ = Logging.write ("VAL rProps = " ^ (PropertySet.toString rProps) ^ "\n\n");
        fun liftImportance c =
            let
                fun collateImportances propPairs =
                    let
                        val uniqueProperties = PropertySet.fromList (map (fn (p, i) => p) propPairs);
                        fun collectImportances p' [] ans = ans
                          | collectImportances p' ((p,i)::ps) ans =
                            if (Property.compare (p', p) = EQUAL) then collectImportances p' ps (i::ans)
                            else collectImportances p' ps ans;
                    in
                        PropertySet.map (fn p => (p, collectImportances p propPairs [])) uniqueProperties
                    end;
                val importanceLookup = (PropertyDictionary.fromPairList o
                                        collateImportances o
                                        (map QProperty.toPair) o
                                        QPropertySet.toList) qProps';
                val importanceMax = max Importance.compare;
                val getImportance = PropertyDictionary.get importanceLookup;
                val flatten = flatmap (fn x => x);
                val qp = Correspondence.leftMatches qProps c;
                val i = importanceMax (flatten (PropertySet.map getImportance qp));
            in
                (c, i)
            end;
        fun modulate strength importance =
            case importance of
                Importance.Noise => 0.0
              | Importance.Zero => 0.0
              | Importance.Low => 0.2 * strength
              | Importance.Medium => 0.6 * strength
              | Importance.High => strength;
        val propertyPairs' = List.filter
                                 (Correspondence.match qProps rProps)
                                 (!correspondingTable');
        val identityPairs = PropertySet.map
                                (fn p => Correspondence.identity p)
                                (PropertySet.collectLeftMatches qProps rProps);
        val identityPairs' = List.filter (fn corr =>
                                             not(List.exists
                                                     (*(Correspondence.sameProperties corr)*)
                                                     (Correspondence.matchingProperties corr)
                                                            propertyPairs'))
                                                identityPairs;
        val propertyPairs = map liftImportance (identityPairs' @ propertyPairs');
        val strength = Correspondence.strength;
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
        val representations = FileDict.keys (!propertyTableRep');
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
