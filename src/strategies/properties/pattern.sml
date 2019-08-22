import "strategies.properties.property";

signature EXPRESSION_TREES =
sig
  type expression_tree;
  val treesFromPattern : Property.property list -> Property.property -> expression_tree list;
end;

structure ExpressionTrees : EXPRESSION_TREES =
struct

datatype branch = Leaf of (string) | Branch of (string * expression_tree list);
datatype expression_tree = Root of ((string list * real * real) * ((real * branch) list))

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

fun selectTokensWithOutputTypes C t =
    List.filter ((isPermutationOf Type.match [t]) o #2 o Type.getInOutTypes o Property.getTypeOfValue) C;

(*
[["a1","b1","c1"],["a2","b2"],["a3"]] ->
    [["a1", "a2", "a3"], ["a1", "b2", "a3"], ["b1", "a2", "a3"],
     ["b1", "b2", "a3"], ["c1", "a2", "a3"], ["c1", "b2", "a3"]] *)
fun listCombChoices [] = [[]]
  | listCombChoices ([]::L') = []
  | listCombChoices ((a::L)::L') = (map (fn x => a::x) (listCombChoices L')) @ (listCombChoices (L::L'));

(* returns a list of possible trees that can be constructing by applying a token
   to some arguments, if the token has a function type. *)
fun treesFromToken C c =
  let
    fun makeTrees_rec CC (cc::LL) = let val CC' = (decreaseMultiplicityOf cc CC)
                                     in (treesFromToken CC' cc) :: (makeTrees_rec CC' LL)
                                     end;
    val t = Property.getTypeOfValue c
    val (iT,_) = Type.getInOutTypes t
    val C' = decreaseMultiplicityOf c C
    val cL = listCombChoices (map (selectTokensWithOutputTypes C') iT) (* list of lists of tokens e.g., in [[t,s],[u,v]] you have to select one from each list
                                                                        e.g., [t,u] or [t,v], thus the application of listCombChoices.
                                                                        Each element of cL is a potential set of children *)
    val ch = List.concat (map (listCombChoices o (makeTrees_rec C')) cL)
  in case iT of [] => [Leaf (Property.stringOf c)]
               | _ => map (Branch o (fn x => (c,x))) ch
  end;


(*
(*start dummy test*)
datatype expression_tree = Leaf of ((string list * string) * int) | Branch of (((string list * string) * int) * expression_tree list);
fun selectTokensWithOutputTypes [] _ = []
  | selectTokensWithOutputTypes (c::L) t = let val ((x,y),_) = c in if t = y then c :: (selectTokensWithOutputTypes L t) else (selectTokensWithOutputTypes L t) end;
fun decreaseMultiplicityOf _ [] = []
  | decreaseMultiplicityOf (c,m) ((c',n)::L) = if c = c' then (if n = 1 then L else (c',n-1)::L) else (c',n)::decreaseMultiplicityOf (c,m) L;
fun treesFromToken C c =
  let
    fun makeTrees_rec CC [] = []
      | makeTrees_rec CC (cc::LL) = let val CC' = (decreaseMultiplicityOf cc CC)
                                     in (treesFromToken CC' cc) :: (makeTrees_rec CC' LL)
                                     end
    val t = c
    val ((iT,_),_) = t
    val C' = decreaseMultiplicityOf c C
    val cL = listCombChoices (map (selectTokensWithOutputTypes C') iT) (* first obtains a list of lists of tokens e.g., in [[t,s],[u,v]],
                                                                         from which you have to select one from each list
                                                                        e.g., [t,u] or [t,v], thus the application of listCombChoices.
                                                                        Each element of cL is a potential set of children *)
    val ch = List.concat (map (listCombChoices o (makeTrees_rec C')) cL)
  in case iT of [] => [Leaf (c)]
               | _ => map (Branch o (fn x => (c,x))) ch
  end;
(*end dummy test*)
*)

fun treesFromType C t = List.concat (map (treesFromToken C) (selectTokensWithOutputTypes C t))

fun treesFromPattern C p =
    let val H = Attribute.M.toPairList (Property.getHoles p)
        val tkL = Property.getTokens p
        fun dec (x::L) X = dec L (decreaseMultiplicityOf x X) | dec [] X = X
        val C' = dec tkL C
        val ud = Property.getStringFunction "udepth" p handle Match => 1.0
        val ub = Property.getStringFunction "ubreadth" p handle Match => 1.0
        fun brachesfromHoles [] = []
          | brachesfromHoles ((t,n)::L) = (map (fn x => (x,n)) (treesFromType C' t)) :: brachesfromHoles L
        val treeListOptions = listCombChoices (treesfromHoles H)
        fun mkTree L = Root ((map Property.LabelOf tkL, ud, ub), L)
    in map mkTree treeListOptions
    end




end;
