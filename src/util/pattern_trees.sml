import "util.type";

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

fun findConstantWithOutputTypes C T =
  let
    val filter (map ((isPermutationOf Type.match T) o #2 o Type.getInOutTypes)) C

fun typeTreeFromConsts C t =
  let
    val C' = filterConstsOfOutput C t
    val = map typeTreeFromConsts C'
  in
  end
