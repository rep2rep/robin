import "strategies.properties.correspondence";

signature CORRESPONDENCEGRAPH =
sig

    type corrgraph;

    val toList : corrgraph -> Correspondence.correspondence list;
    val fromList : Correspondence.correspondence list -> corrgraph;

    val insert : corrgraph -> Correspondence.correspondence -> corrgraph;
    val insertWithParents : corrgraph ->
                            Correspondence.correspondence ->
                            Correspondence.correspondence list ->
                            corrgraph;

    val directDescendents : corrgraph ->
                            Correspondence.correspondence ->
                            Correspondence.correspondence list;
    val allDescendents : corrgraph ->
                         Correspondence.correspondence ->
                         Correspondence.correspondence list;

end;

structure CorrespondenceGraph :> CORRESPONDENCEGRAPH =
struct

datatype corrnode = CorrNode of (Correspondence.correspondence ref
                                 * corrnode ref list);
type corrgraph = corrnode list;

(* val getNode : corrgraph -> Correspondence.correspondence -> corrnode *)
fun getNode cg c = List.hd (List.filter
                                (fn CorrNode (c', ds) =>
                                    Correspondence.equal c (!c'))
                                cg);

val fromList = map (fn c => CorrNode (ref c, []));

val toList = map (fn (CorrNode (c, _)) => !c);

fun insert cg c = (CorrNode (ref c, []))::cg;

fun insertWithParents cg c ps =
    let
        val new = CorrNode (ref c, []);
        fun linkNew (CorrNode (c', ds)) =
            if ListSet.contains (uncurry Correspondence.equal) ps (!c')
            then CorrNode (c', (ref new)::ds)
            else CorrNode (c', ds);
    in
        (new) :: (map linkNew cg)
    end;

fun hasDescendents cg c =
    case getNode cg c of
        CorrNode (_, ds) => List.null ds;

fun directDescendents cg c =
    let
        val CorrNode (c', ds) = getNode cg c;
    in
        map (fn d => case !d of CorrNode (c, _) => !c) ds
    end;

fun allDescendents cg c =
    let
        fun trav cs = List.flatmap (fn nr =>
                                       case !nr of
                                           CorrNode (c, cs) => !c :: (trav cs))
                              cs;
        val CorrNode (c', ds) = getNode cg c;
    in
        trav ds
    end;

end;
