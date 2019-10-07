import "strategies.properties.property";

signature PATTERN =
sig
  type data = real * real;
  type resources = ((string list * string list * (Type.T list * Type.T)) * int) list;
  type clause = (Type.T list * ((int * int) * resources));

  val unfoldTypeDNF : clause list -> (bool * clause list);

  val satisfyTypeDNF : clause list -> clause list * data;

  val satisfyPattern : Property.property -> Property.property list -> Property.property list
                        -> (clause list * data);

  val arity : Property.property -> int;
  val distinctArity : Property.property -> real;
end;

structure Pattern : PATTERN =
struct

  type data = real * real;
  type resources = ((string list * string list * (Type.T list * Type.T)) * int) list;
  type clause = (Type.T list * ((int * int) * resources));

fun sameTypeDNF (tF, tF') =
    let (* compares everything except the current depth *)
        fun same [] [] = true
          | same ((cl,(_,K))::L) ((cl',(_,K'))::L') = (cl = cl') andalso (K = K') andalso (same L L')
          | same _ _ = false
    in same tF tF'
    end

fun getChildrenOptions _ [] = []
  | getChildrenOptions t (((labels,tokens,(tL,t')),i)::K) =
    if Type.match (t, t')
    then (labels,tokens,(tL,t')) :: getChildrenOptions t K
    else getChildrenOptions t K;

exception Unsatisfiable;

(* takes a list of token labels and diminishes their multiplicity in the knowledge base *)
fun diminish L [] = if null L then [] else raise Unsatisfiable
  | diminish L (((labels,tokens,typeinfo),i)::K) =
    case List.find (fn x => List.exists (fn y => x = y) L) labels of
        SOME label => let val L' = List.remove label L
                          val K' = diminish L' K
                      in (if i <= 1 then K' else ((labels,tokens,typeinfo),i-1)::K')
                      end
      | NONE => ((labels,tokens,typeinfo),i) :: diminish L K;


(* tF ~ [([a,b],K1),([c],K2),([a,c,d,e],K3)]*)
fun unfoldTypeDNF [] = (false,[])
  | unfoldTypeDNF (cl::dnf) = (* HERE *)
    let fun distribute [] LL' = []
          | distribute ((_,tokens,(tL,t))::LL) LL' =
            let fun removeNONEs [] = []
                  | removeNONEs ((SOME x) :: L) = x :: removeNONEs L
                  | removeNONEs (NONE :: L) = removeNONEs L
                fun conjAndDim (l,((d,b),K)) = SOME (tL @ l, ((d,Int.max(b,length (tL @ l))),diminish tokens K)) handle Unsatisfiable => NONE
            in (removeNONEs (map conjAndDim LL')) @ distribute LL LL'
            end
        fun unfoldClause ([],((d,b),K)) = [([],((d+1,b),K))]
          | unfoldClause ((lt::C),((d,b),K)) = distribute (getChildrenOptions lt K) (unfoldClause (C,((d,b),K)));

(* The following functions are not used but could potentially simplify the search if used properly *)
(* begin *)
(*
        fun remove_satisfied [] = []
          | remove_satisfied ((x,_)::L) = if null x then remove_satisfied L else x :: remove_satisfied L

        fun countAndRemoveType _ [] = (0,[])
          | countAndRemoveType t' ((t,i)::C) = let val (s,L) = countAndRemoveType t' C
                                               in if t' = t then (s+i,L) else (s,(t,i)::L)
                                               end;
        fun simplifyClause [] = []
          | simplifyClause ((lt,count)::C) = let val (s,L) = countAndRemoveType lt ((lt,count) :: C)
                                              in (lt,s) :: simplifyClause L
                                              end;
*)(* end *)

        val unfoldedCl = if null (#1 cl) then [cl] else unfoldClause cl
        val clChanged = not (sameTypeDNF ([cl],unfoldedCl))
        val (dnfChanged, unfoldedDNF) = unfoldTypeDNF dnf
    in (clChanged orelse dnfChanged, unfoldedCl @ unfoldedDNF)
    end;


fun satisfyTypeDNF tF =
    let fun iterate x =
            let val (changed,x') = unfoldTypeDNF x
            in (print ("\n       length of DNF: " ^ Int.toString (length x) ^ "");
                if null x' then raise Unsatisfiable
                else (if changed
                       then iterate x'
                       else x)
                )
            end
        fun avgDepthAndBreadth L = (List.avgIndexed (fn (_,((d,_),_)) => real d) L,
                                    List.avgIndexed (fn (_,((_,b),_)) => real b) L)
        fun maxDepthAndBreadth [] = (1,1)
          | maxDepthAndBreadth ((_,((d,b),_))::L) =
            let val (d',b') = maxDepthAndBreadth L
            in (Int.max (d,d'), Int.max (b,b'))
            end
        val sattF = iterate tF
    in (sattF, avgDepthAndBreadth sattF)
    end;

fun satisfyPattern p C P =
    let val occurrences = #2 o (Property.getNumFunction "occurrences")
        val nty = real (List.length (List.removeDuplicates (map Property.getTypeOfValue C)))
        val nt = List.sumIndexed occurrences C / nty
        fun fneg x = if x = ~1 then Real.floor (Math.ln nt)
                      else if x = ~2 then Real.floor (Math.sqrt nt)
                      else if x = ~3 then Real.floor nt
                      else if x >= 0 then x else raise Match
        fun makeTypeListFromHoles M = Property.toListHandlingNegatives fneg M
        fun getLTTNC c = (([Property.LabelOf c],
                           [Property.LabelOf c],
                           Type.getInOutTypes (Property.getTypeOfValue c)),
                          Real.floor (occurrences c))
        fun getLTTNP x = (([Property.LabelOf x],
                           Property.LabelOf x :: Property.getTokens x,
                           (makeTypeListFromHoles (Property.getHoles x), Property.getTypeOfValue x)),
                          Real.floor (occurrences x))

        fun findAndUpdateByTypes ((l,ls,(ts,t)),i) [] = [((l,ls,(ts,t)),i)]
          | findAndUpdateByTypes ((l,ls,(ts,t)),i) (((l',ls',(ts',t')),i')::L) =
            if List.isPermutationOf (fn (x,y) => x = y) ts ts' andalso t = t'
                andalso (List.tl ls handle Empty => []) = (List.tl ls' handle Empty => [])
                (* this last condition makes sure that the patterns are only clustered together if they have the same tokens*)
            then ((l @ l',ls',(ts,t)),i+i')::L
            else ((l',ls',(ts',t')),i') :: findAndUpdateByTypes ((l,ls,(ts,t)),i) L;
        fun clusterByTypes [] = []
          | clusterByTypes (((l,ls,(ts,t)),i)::L) = findAndUpdateByTypes ((l,ls,(ts,t)),i) (clusterByTypes L);
        val C' = clusterByTypes (map getLTTNC C)
        val P' = clusterByTypes (map getLTTNP P)
        val CP = clusterByTypes ((map getLTTNC C) @ map getLTTNP P)

        fun printLTTN ((labels,tokens,(_,_)),i) = print (labels ^ ", [" ^ String.concat tokens ^ "], " ^ (Int.toString i) ^ "\n")
        val ((_,tks,(typs,_)),_) = if Property.kindOf p = Kind.Pattern then getLTTNP p else getLTTNC p
        val udepth = Real.floor (#2 (Property.getNumFunction "udepth" p)) handle NoAttribute => 1
        fun patternClause () = (typs, ((udepth,1), diminish tks CP))
    in satisfyTypeDNF [patternClause ()] handle Unsatisfiable => ([],(0.0,0.0))
    end

(*)
fun maxWithOmegaPlus (x,y) =
    if (x < 0 andalso y < 0) then Int.min (x,y)
    else if x < 0 then x
    else if y < 0 then y
    else Int.max (x,y);

fun listMaxWithOmegaPlus [] = 0
  | listMaxWithOmegaPlus (h::t) = maxWithOmegaPlus (h,listMaxWithOmegaPlus t);

(* Pattern functions *)
fun depth C p = avgDepth (treesFromPattern C p)
fun breadth C p = avgBreadth (treesFromPattern C p)

*)
fun arity p = if Property.kindOf p = Kind.Pattern then Property.size (Property.getHoles p) else List.length (#1 (Type.getInOutTypes (Property.getTypeOfValue p)))
fun distinctArity p = real (Property.countUnique (Property.getHoles p))


end;
