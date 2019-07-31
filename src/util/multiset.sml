signature MULTISET =
sig
  type ''a multiset;
  val fromPairList : (''a * int) list -> ''a multiset;
  val fromList : ''a list -> ''a multiset;
end;

structure Multiset : MULTISET =
struct

type 'a multiset = ('a -> int);

fun fromPairList [] = (fn x => 0)
  | fromPairList ((a,n)::L) =
    fn x => if x = a then n + (fromPairList L a) else fromPairList L x;

fun fromList [] = (fn x => 0)
  | fromList (a::L) =
    fn x => if x = a then (fromList L a) + 1 else fromList L x;

end;
