import "strategies.properties.property";

signature PATTERN =
sig
  type branch;
  type expression_tree;

  val decreaseMultiplicityOf : string -> Property.property list -> Property.property list;
  val treesFromToken : Property.property list -> Property.property -> branch list;
  val treesFromPattern : Property.property list -> Property.property -> expression_tree list;
  val baseTokens : expression_tree -> string list;
  val baseDepth : expression_tree -> real;

  val unfoldTypeDNF : (Type.T list * (((string list * string list * (Type.T list * Type.T)) * int) list)) list
                        -> (Type.T list * (((string list * string list * (Type.T list * Type.T)) * int) list)) list;
  val satisfyTypeDNF : (Type.T list * (((string list * string list * (Type.T list * Type.T)) * int) list)) list
                        -> ((Type.T list * (((string list * string list * (Type.T list * Type.T)) * int) list)) list * int);
(*)  val unfoldPattern : Property.property -> Property.property list -> Property.property list
                        -> (Type.T list * (((string * string list * (Type.T list * Type.T)) * int) list)) list;
*)
  val satisfyPattern : Property.property -> Property.property list -> Property.property list
                        -> ((Type.T list * (((string list * string list * (Type.T list * Type.T)) * int) list)) list * int);

  val listMaxWithOmegaPlus : int list -> int;

  val maxBreadth : expression_tree -> int;
  val maxDepth : expression_tree -> int;

  val maxBranchDepth : branch -> int;
  val maxBranchBreadth : branch * int -> int;
  val avgDepth : expression_tree list -> real;
  val avgBreadth : expression_tree list -> real;

  val depth : Property.property list -> Property.property -> real;
  val breadth : Property.property list -> Property.property -> real;
  val arity : Property.property -> int;
  val distinctArity : Property.property -> real;
end;

structure Pattern : PATTERN =
struct

datatype branch = Branch of (string * branch list);
datatype expression_tree = Root of ((string list * int) * ((branch * int) list))

fun baseTokens (Root ((L,_),_)) = L
fun baseDepth (Root ((_,r),_)) = real r


fun inList f x [] = false
  | inList f x (y::L) = if f (x,y) then true else inList f x L;

fun findAndRemove _ _ [] = (false,[])
  | findAndRemove f x (a::L) =
      if f (x, a) then (true,L)
      else let val (found,L') = findAndRemove f x L
           in (found,a::L')
           end;

fun isPermutationOf _ [] [] = true
  | isPermutationOf f (a::A) B = let val (found,B') = findAndRemove f a B
                                 in if found then isPermutationOf f A B'
                                    else false
                                 end
  | isPermutationOf _ _ _ = false;

fun selectTokensWithOutputType C t =
    let fun f x = let val outType = (#2 o Type.getInOutTypes o Property.getTypeOfValue) x
                  in Type.match (t, outType)
                  end
        val L = List.filter f C
    in L
    end;

(*
[["a1","b1","c1"],["a2","b2"],["a3"]] ->
    [["a1", "a2", "a3"], ["a1", "b2", "a3"], ["b1", "a2", "a3"],
     ["b1", "b2", "a3"], ["c1", "a2", "a3"], ["c1", "b2", "a3"]] *)
fun listCombChoices [] = [[]]
  | listCombChoices ([]::L') = []
  | listCombChoices ((a::L)::L') = (map (fn x => a::x) (listCombChoices L')) @ (listCombChoices (L::L'));


fun decreaseMultiplicityOf c [] = []
  | decreaseMultiplicityOf c (c'::L) =
      if c = Property.LabelOf c'
      then
           (if #2 (Property.getNumFunction "occurrences" c') <= 1.0 then L
            else (Property.updateNumFunction "occurrences" (fn x => x - 1.0) c') :: L)
              handle Match => (print ("no attribute \"occurrences\" for token" ^ Property.toString c'); raise Match)
      else c' :: decreaseMultiplicityOf c L

(* returns a list of possible trees that can be constructing by applying a token
   to some arguments, if the token has a function type. *)
fun treesFromToken tokens c = if null tokens then [] else
  let
    val C = List.filter (fn x => #2 (Property.getNumFunction "occurrences" x) > 0.0) tokens
  (*)  val _ = print (Property.LabelOf c ^ " " ^ (Real.toString (#2 (Property.getNumFunction "occurrences" c)))^ "\n")*)
    fun makeTrees_rec CC (cc::LL) = let val CC' = (decreaseMultiplicityOf (Property.LabelOf cc) CC)
                                     in (treesFromToken CC' cc) :: (makeTrees_rec CC' LL)
                                     end
      | makeTrees_rec _ [] = [];
    val t = Property.getTypeOfValue c
    val (iT,_) = Type.getInOutTypes t
    val C' = decreaseMultiplicityOf (Property.LabelOf c) C
    val cL = listCombChoices (map (selectTokensWithOutputType C') iT) (* list of lists of tokens e.g., in [[t,s],[u,v]] you have to select one from each list
                                                                        e.g., [t,u] or [t,v], thus the application of listCombChoices.
                                                                        Each element of cL is a potential set of children *)
  (*)  val _ = print (Property.toString (hd (hd cL)) ^ "\n" handle _ => "none") *)

    fun ch' L = listCombChoices (makeTrees_rec C' L)
    val ch = List.hd (map ch' cL) handle Empty => []

  in
    map (fn x => Branch (Property.LabelOf c,x)) ch
  end;


fun treesFromType C t = List.concat (map (treesFromToken C) (selectTokensWithOutputType C t))

fun treesFromPattern tokens p =
    let val C = List.filter (fn x => #2 (Property.getNumFunction "occurrences" x) > 0.0) tokens
        val tkL = Property.getTokens p
        fun dec (x::L) X = dec L (decreaseMultiplicityOf x X) | dec [] X = X
        val C' = dec tkL C
        val ud = #2 (Property.getNumFunction "udepth" p) handle Match => 1.0
        val H = Property.toPairList (Property.getHoles p)
        fun branchesFromHoles [] = []
          | branchesFromHoles ((t,n)::L) = (map (fn x => (x,n)) (treesFromType C' (t))) :: branchesFromHoles L
        val treeListOptions = listCombChoices (branchesFromHoles H)
        fun mkTree L = Root ((tkL, Real.toInt IEEEReal.TO_NEAREST ud), L)
    in map mkTree treeListOptions
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
                      in (if i = 1 then K' else ((labels,tokens,typeinfo),i-1)::K')
                      end
      | NONE => ((labels,tokens,typeinfo),i) :: diminish L K;


(* tF ~ [([a,b],K1),([c],K2),([a,c,d,e],K3)]*)
fun unfoldTypeDNF [] = []
  | unfoldTypeDNF (cl::dnf) = (* HERE *)
    let fun distribute [] LL' = []
          | distribute ((_,tokens,(tL,t))::LL) LL' =
            let fun removeNONEs [] = []
                  | removeNONEs ((SOME x) :: L) = x :: removeNONEs L
                  | removeNONEs (NONE :: L) = removeNONEs L
                fun conjAndDim (l,K) = let val (K',b) = (diminish tokens K,true) handle Unsatisfiable => ([],false)
                                       in if b then SOME (tL @ l, K') else NONE
                                       end
            in (removeNONEs (map conjAndDim LL')) @ distribute LL LL'
            end
        fun unfoldClause ([],K) = [([],K)]
          | unfoldClause ((lt::C),K) = distribute (getChildrenOptions lt K) (unfoldClause (C,K));
    (*)    fun remove_satisfied [] = []
          | remove_satisfied (x::L) = if null (#1 x) then remove_satisfied L else x :: remove_satisfied L
        val ff = remove_satisfied (unfoldClause cl) *)
    in unfoldClause cl @ (unfoldTypeDNF dnf)
    end;

exception Length;
fun zipLists [] [] = []
  | zipLists (x::xL) (y::yL) = (x,y) :: zipLists xL yL
  | zipLists _ _ = raise Length;

fun sameTypeDNF (tF, tF') =
    let fun sameK (tK,tK') = (#1 tK = #1 tK') andalso (#2 tK = #2 tK')
        fun same (t,t') = (#1 t = #1 t') andalso (List.all sameK (zipLists (#2 t) (#2 t')))
    in List.all same (zipLists tF tF') handle Length => false
    end

fun satisfyTypeDNF tF =
    let fun iterate x i =
            let val x' = unfoldTypeDNF x
            in (print ("\n         length of DNF: " ^ Int.toString (length x) ^ "");
                if null x' then raise Unsatisfiable
                else (if sameTypeDNF (x,x')
                       then (x', i)
                       else iterate x' (i+1))
                )
            end
    in iterate tF 0
    end;
(*)
fun unfoldPattern p C P =
    let val occurrences = #2 o (Property.getNumFunction "occurrences")
        val nt = List.sumIndexed occurrences C
        fun fneg x = if x = ~1 then Real.floor (Math.ln nt)
                      else if x = ~2 then Real.floor (Math.sqrt nt)
                      else if x = ~3 then Real.floor (nt / 2.0)
                      else if x >= 0 then x else raise Match
        fun makeTypeListFromHoles M = Property.toListHandlingNegatives fneg M
        fun getLTTNC c = ((Property.LabelOf c,
                           [Property.LabelOf c],
                           Type.getInOutTypes (Property.getTypeOfValue c)),
                          Real.floor (occurrences c))
        fun getLTTNP x = ((Property.LabelOf x,
                           Property.getTokens x,
                           (makeTypeListFromHoles (Property.getHoles x), Property.getTypeOfValue x)),
                          Real.floor (occurrences x))
        fun clusterByTypes [] = []
          | clusterByTypes (x::L) = x

        fun printLTTN ((labels,tokens,(_,_)),i) = print (labels ^ ", [" ^ String.concat tokens ^ "], " ^ (Int.toString i) ^ "\n")
        val ((_,tks,(typs,_)),_) = getLTTNP p
        fun patternClause () = (typs, (diminish tks (map getLTTNC C)) @ (map getLTTNP P))
    in unfoldTypeDNF [patternClause ()]
    end*)

fun satisfyPattern p C P =
    let val occurrences = #2 o (Property.getNumFunction "occurrences")
        val nt = List.sumIndexed occurrences C
        fun fneg x = if x = ~1 then Real.floor (Math.ln nt)
                      else if x = ~2 then Real.floor (Math.sqrt nt)
                      else if x = ~3 then Real.floor (nt / 2.0)
                      else if x >= 0 then x else raise Match
        fun makeTypeListFromHoles M = Property.toListHandlingNegatives fneg M
        fun getLTTNC c = (([Property.LabelOf c],
                           [Property.LabelOf c],
                           Type.getInOutTypes (Property.getTypeOfValue c)),
                          Real.floor (occurrences c))
        fun getLTTNP x = (([Property.LabelOf x],
                           Property.getTokens x,
                           (makeTypeListFromHoles (Property.getHoles x), Property.getTypeOfValue x)),
                          Real.floor (occurrences x))

        fun findAndUpdateByTypes ((l,ls,(ts,t)),i) [] = [((l,ls,(ts,t)),i)]
          | findAndUpdateByTypes ((l,ls,(ts,t)),i) (((l',ls',(ts',t')),i')::L) =
            if isPermutationOf (fn (x,y) => x = y) ts ts' andalso t = t
            then (print (String.concat (l @ l') ^ "\n"); ((l @ l',ls',(ts,t)),i+i')::L)
            else ((l',ls',(ts',t')),i') :: findAndUpdateByTypes ((l,ls,(ts,t)),i) L;
        fun clusterByTypes [] = []
          | clusterByTypes (((l,ls,(ts,t)),i)::L) = findAndUpdateByTypes ((l,ls,(ts,t)),i) (clusterByTypes L);
        val C' = clusterByTypes (map getLTTNC C)

        fun printLTTN ((labels,tokens,(_,_)),i) = print (labels ^ ", [" ^ String.concat tokens ^ "], " ^ (Int.toString i) ^ "\n")
        val ((_,tks,(typs,_)),_) = getLTTNP p
        fun patternClause () = (typs, (diminish tks C') @ (map getLTTNP P))
    in satisfyTypeDNF [patternClause ()] handle Unsatisfiable => ([],0)
    end


fun maxWithOmegaPlus (x,y) =
    if (x < 0 andalso y < 0) then Int.min (x,y)
    else if x < 0 then x
    else if y < 0 then y
    else Int.max (x,y);

fun listMaxWithOmegaPlus [] = 0
  | listMaxWithOmegaPlus (h::t) = maxWithOmegaPlus (h,listMaxWithOmegaPlus t);

fun maxBranchDepth (Branch (_,[])) = 0
  | maxBranchDepth (Branch (_,h::L)) = maxWithOmegaPlus (maxBranchDepth h, listMaxWithOmegaPlus (map maxBranchDepth L)) + 1;

fun maxBranchBreadth ((Branch (_,[])),i) = i
  | maxBranchBreadth ((Branch (_,h::L)),i) =
      listMaxWithOmegaPlus ((maxBranchBreadth (h,1))
                            :: (1 + List.length L)
                            :: (map (maxBranchBreadth o (fn x => (x,1))) L));

fun maxDepth ((Root ((s,ud),L))) = listMaxWithOmegaPlus ((map (maxBranchDepth o #1) L)) + ud
fun maxBreadth ((Root ((s,_),L))) = listMaxWithOmegaPlus (map maxBranchBreadth L)

fun typeCount (t,n) = n;


(* tree list functions *)
fun avgDepth L = if null L then 1.0 else List.avgIndexed (real o maxDepth) L
fun avgBreadth L = if null L then 1.0 else List.avgIndexed (real o maxBreadth) L

(* Pattern functions *)
fun depth C p = avgDepth (treesFromPattern C p)
fun breadth C p = avgBreadth (treesFromPattern C p)

fun arity p = (Property.size (Property.getHoles p))
fun distinctArity p = real (Property.countUnique (Property.getHoles p))


end;
