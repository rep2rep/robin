import "strategies.correspondences.correspondence";

structure CorrespondenceList : CORRESPONDENCELIST =
struct

structure F = Correspondence.F;

fun allMatches corrs qProps rProps =
    let
        val qProps' = QPropertySet.withoutImportances qProps;
        fun alreadyCorr cs c = List.exists (Correspondence.matchingProperties c) cs;
        val baseCorrs = List.filter (Correspondence.match qProps' rProps) corrs;
        val identities = PropertySet.map
                             Correspondence.identity
                             (PropertySet.collectLeftMatches qProps' rProps);
        val newIdentities = List.filter (fn c => not (alreadyCorr baseCorrs c))
                                        identities;
    in
        newIdentities @ baseCorrs
    end;


exception skipProp;
fun typeCorrespondences corrs qProps =
    let fun tCorrs q =
            let val p = QProperty.withoutImportance q
                val t = Property.getTypeOfValue p handle Property.NoAttribute _ => raise skipProp
                val singletonT = PropertySet.fromList [Property.fromKindValueAttributes (Kind.Type, Property.Type t, [])]
                val singletonP = PropertySet.fromList [p]
                val g = QProperty.logGravity q handle Property.NoAttribute _ => 0.0
                fun mkCorrs [] = []
                  | mkCorrs (((x,y,s),i)::L) =
                    if PropertySet.isEmpty (Correspondence.leftMatches singletonT (x,y,s))
                    then (if PropertySet.isEmpty (Correspondence.leftMatches singletonP (x,y,s)) then mkCorrs L else raise skipProp)
                    else ((F.Atom p, F.Atom (Property.fromKindValueAttributes (Kind.Dummy, Property.Label ("\"" ^ F.toString Property.toString y ^ "\""), [])),s), i*g) :: mkCorrs L
            in mkCorrs corrs
            end handle skipProp => [] (* This is the hackiest thing ever, but it should take care of cases when there is already a correspondence for the property, so we don't need to add it from its type *)
    in List.concat (QPropertySet.map tCorrs qProps)
    end;

local
    structure PMS = Multiset(struct type t = Property.property;
                                    val compare = Property.compare;
                                    val fmt = Property.toString;
                             end);
    fun toSet ms = (PropertySet.fromList o PMS.toList) ms;
in
fun mrmc corrs qProps rProps =
    let
        val qProps' = QPropertySet.withoutImportances qProps;

        fun score (cover, pqs, prs, cs) = (* We start with the basic set-cover greedy*)
            let
                (* fun redundancy ms = (PMS.size ms) - (PMS.countUnique ms); *)
                fun redundancy ms = PMS.foldr' (fn ((_, f), t) => t + (f - 1)) 0 ms;
                val pqset = toSet pqs;

                val coverage = Real.fromInt (PropertySet.size (PropertySet.difference qProps' pqset));
                val qredundence = Real.fromInt (redundancy pqs);
                val rredundence = Real.fromInt (redundancy prs);

                val (wc, wq, wr) = (1.0, 1.0, 0.5);
            in (wc * coverage) + (wq * qredundence) + (wr * rredundence) end;

        fun neighbours (cover, pqs, prs, cs') =
            let fun getClauses (c as ((q, r, s), i)) =
                    let val qfs = map (#1 o Correspondence.F.clauseToLists) (Correspondence.F.clauses q);
                        val rfs = map (#1 o Correspondence.F.clauseToLists) (Correspondence.F.clauses r);
                    in List.product (qfs, rfs) end;
                fun createState c cs qf rf =
                    let val pqs' = PMS.union pqs (PMS.fromList qf);
                        val prs' = PMS.union prs (PMS.fromList rf);
                    in (c::cover, pqs', prs', cs) end;
                fun extend (c, cs) =
                    map (fn (qf, rf) => createState c cs qf rf) (getClauses c);
            in List.flatmap extend (List.inout cs') end;

        val (cover, _, _, _) = Algorithms.gradientDescent neighbours score
                                                          ([], PMS.empty(),
                                                           PMS.empty(), corrs);

    in cover end;
end;

end;
