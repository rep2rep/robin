import "util.logging";

import "strategies.properties.correspondence";

signature CORRESPONDENCEGRAPH =
sig

    exception MissingNode;

    type corrgraph;

    val toList : corrgraph -> Correspondence.correspondence list;
    val fromList : Correspondence.correspondence list -> corrgraph;

    val insert : corrgraph -> Correspondence.correspondence -> corrgraph;
    val insertWithParents : corrgraph ->
                            Correspondence.correspondence ->
                            Correspondence.correspondence list ->
                            corrgraph;

    val exists : (Correspondence.correspondence -> bool) -> corrgraph -> bool;
    val hasDescendents : corrgraph -> Correspondence.correspondence -> bool;

    val directDescendents : corrgraph ->
                            Correspondence.correspondence ->
                            Correspondence.correspondence list;
    val allDescendents : corrgraph ->
                         Correspondence.correspondence ->
                         Correspondence.correspondence list;

end;

structure CorrespondenceGraph :> CORRESPONDENCEGRAPH =
struct

exception MissingNode;

datatype corrnode = CorrNode of (Correspondence.correspondence ref
                                 * corrnode ref list
                                 * corrnode ref list);
type corrgraph = corrnode ref list;

(* val getNode : corrgraph -> Correspondence.correspondence -> corrnode ref *)
fun getNode cg c = List.hd (List.filter
                                (fn cnr =>
                                    case (!cnr) of
                                        CorrNode (c', _, _) =>
                                        Correspondence.equal c (!c'))
                                cg)
                   handle List.Empty => raise MissingNode;

val fromList = map (fn c => ref (CorrNode (ref c, [], [])));

val toList = map (fn cnr => case (!cnr) of (CorrNode (c, _, _)) => !c);

(* fun propagate cg f node = *)
(*     let val CorrNode (cr, desc, anc) = node; *)
(*         val c = !cr; *)
(*         val c' = f c; *)
(*     in *)
(*         if not (Correspondence.equal c c') *)
(*         then (cr := c'; ) *)
(*         else cg *)
(*     end; *)

fun insertIf p cg c =
    let val cnr = getNode cg c;
        val CorrNode (cr, desc, anc) = !cnr;
    in if p (!cr)
       then ((cnr := CorrNode (ref c, desc, anc)) ; cg)
       (* then propagate cg (fn x => c) node *)
       else cg
    end handle MissingNode => (ref (CorrNode (ref c, [], [])))::cg;

fun insert cg c =
    insertIf (fn _ => (Logging.error ("Correspondence already exists: "
                                      ^ (Correspondence.toString c)
                                      ^ "\n");
                       raise Fail "collision"))
             cg c;

fun insertWithParents cg c ps =
    let
        val new = ref (CorrNode (ref c, [], []));
        fun addDesc cnr =
            let val CorrNode (cr, desc, anc) = !cnr;
            in cnr := CorrNode (cr, new::desc, anc) end;
        fun addParent cnr =
            let val CorrNode (cr, desc, anc) = !cnr;
            in cnr := CorrNode (cr, desc, new::anc) end;
        fun linkNew cnr =
            let val CorrNode (c', desc, anc) = !cnr
            in if ListSet.contains (uncurry Correspondence.equal) ps (!c')
               then (addParent cnr; addDesc cnr)
               else ()
            end;
        val _ = map linkNew cg;
    in
        new :: cg
    end;

fun exists p cg = List.exists (fn cnr =>
                                  case !cnr of
                                      CorrNode (c, _, _) => p (!c)) cg;

fun hasDescendents cg c =
    case !(getNode cg c) of
        CorrNode (_, ds, ac) => List.null ds;

fun directDescendents cg c =
    let
        val CorrNode (c', ds, ac) = !(getNode cg c);
    in
        map (fn d => case !d of CorrNode (c, _, _) => !c) ds
    end;

fun allDescendents cg c =
    let
        fun trav cs = List.flatmap (fn nr =>
                                       case !nr of
                                           CorrNode (c, ds, ac) => !c :: (trav ds))
                              cs;
        val CorrNode (c', ds, ac) = !(getNode cg c);
    in
        trav ds
    end;

end;
