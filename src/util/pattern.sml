import "util.type";

signature EXPRESSION_TREES =
sig
  type expression_tree;
  val treesFromToken : Property.property list -> Property.property -> expression_tree list;
  val treesFromPattern : Property.property list -> Property.property -> expression_tree list;
end;

structure ExpressionTrees : EXPRESSION_TREES =
struct

datatype expression_tree = Leaf of (string * Type.T) | Branch of (string * expression_tree list);

fun inList f x [] = false
  | inList f x (y::L) = if f (x,y) then true else inList x L;

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
    List.filter ((isPermutationOf Type.match [t]) o #2 o Type.getInOutTypes o Property.typeOfValue) C;

fun treesFromToken C c =
  let
    val C' = decreaseFrequencyOf c C
    val t = Property.typeOfValue c
    val (iT,_) = Type.getInOutTypes t
    val cL = map (selectTokensWithOutputType C') iT (* list of lists of tokens e.g., in [[t,s],[u,v]] you have to select one from each list (e.g., t and u) *)
    fun xxx ((cc::L)::L') = let val updL' = map (decreaseFrequencyOf cc) L'
                                val localtree = treesFromToken C' cc
                            in (localtree :: (xxx L')) :: xxx (L::L')
                            end
      | xxx ([]::L') = Leaf
    val children = map (treeFromToken C') cL
  in
    if null children
    then Leaf (Property.LabelOf c, Property.typeOfValue c)
    else Branch children
  end

fun treeFromPattern C p =


end;
