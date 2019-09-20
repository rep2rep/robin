import "strategies.properties.property";

signature PATTERN =
sig
  type expression_tree;
  val treesFromPattern : Property.property list -> Property.property -> expression_tree list;
  val baseTokens : expression_tree -> string list;
  val baseDepth : expression_tree -> real;

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

fun selectTokensWithOutputTypes C t =
    List.filter ((isPermutationOf Type.match [t]) o #2 o Type.getInOutTypes o Property.getTypeOfValue) C;

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
fun treesFromToken C c =
  let
    fun makeTrees_rec CC (cc::LL) = let val CC' = (decreaseMultiplicityOf (Property.LabelOf cc) CC)
                                     in (treesFromToken CC' cc) :: (makeTrees_rec CC' LL)
                                     end
      | makeTrees_rec _ [] = [];
    val t = Property.getTypeOfValue c
    val (iT,_) = Type.getInOutTypes t
    val C' = decreaseMultiplicityOf (Property.LabelOf c) C
    val cL = listCombChoices (map (selectTokensWithOutputTypes C') iT) (* list of lists of tokens e.g., in [[t,s],[u,v]] you have to select one from each list
                                                                        e.g., [t,u] or [t,v], thus the application of listCombChoices.
                                                                        Each element of cL is a potential set of children *)
    val ch = List.concat (map (listCombChoices o (makeTrees_rec C')) cL)
  in
    map (Branch o (fn x => (Property.LabelOf c,x))) ch
  end;


fun treesFromType C t = List.concat (map (treesFromToken C) (selectTokensWithOutputTypes C t))

fun treesFromPattern C p =
    let val H = Property.toPairList (Property.getHoles p)
        val tkL = Property.getTokens p
        fun dec (x::L) X = dec L (decreaseMultiplicityOf x X) | dec [] X = X
        val C' = dec tkL C
        val ud = #2 (Property.getNumFunction "udepth" p) handle Match => 1.0
        fun branchesFromHoles [] = []
          | branchesFromHoles ((t,n)::L) = (map (fn x => (x,n)) (treesFromType C' (t))) :: branchesFromHoles L
        val treeListOptions = listCombChoices (branchesFromHoles H)
        fun mkTree L = Root ((tkL, Real.toInt IEEEReal.TO_NEAREST ud), L)
    in map mkTree treeListOptions
    end

exception BadTree;

fun maxWithOmegaPlus (x,y) =
    if (x < 0 andalso y < 0) then Int.min (x,y)
    else if x < 0 then x
    else if y < 0 then y
    else Int.max (x,y);

fun listMax [] = 0
  | listMax (h::t) = maxWithOmegaPlus (h,listMax t);

fun maxBranchDepth (Branch (_,[])) = 0
  | maxBranchDepth (Branch (_,h::L)) = maxWithOmegaPlus (maxBranchDepth h, listMax (map maxBranchDepth L)) + 1;

fun maxBranchBreadth ((Branch (_,[])),i) = i
  | maxBranchBreadth ((Branch (_,h::L)),i) =
      listMax ((maxBranchBreadth (h,1))
                :: (1 + List.length L)
                :: (map (maxBranchBreadth o (fn x => (x,1))) L));

fun maxDepth ((Root ((s,ud),L))) = listMax ((map (maxBranchDepth o #1) L)) + ud
fun maxBreadth ((Root ((s,_),L))) = listMax (map maxBranchBreadth L)

fun typeCount (t,n) = n;


(* tree list functions *)
fun avgDepth L = List.avgIndexed (real o maxDepth) L
fun avgBreadth L = List.avgIndexed (real o maxBreadth) L


(* Pattern functions *)
fun depth C p = avgDepth (treesFromPattern C p) + (#2(Property.getNumFunction "udepth" p) handle Match => 1.0)
fun breadth C p = avgBreadth (treesFromPattern C p)

fun arity p = (Property.size (Property.getHoles p))
fun distinctArity p = real (Property.countUnique (Property.getHoles p))


end;
