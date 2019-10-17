(*
listset.sml

For small sets it is often not worth applying the Set functor,
so we use lists. The penalty is small enough to not worry about,
so we provide some useful general functions for this case.
*)

signature LISTSET =
sig

    type 'a eqf = 'a * 'a -> bool;

    val removeDuplicates : 'a eqf -> 'a list -> 'a list;

    val union : 'a eqf -> 'a list -> 'a list -> 'a list;
    val intersection : 'a eqf -> 'a list -> 'a list -> 'a list;
    val difference : 'a eqf -> 'a list -> 'a list -> 'a list;

    val unionAll : 'a eqf -> 'a list list -> 'a list;
    val intersectionAll : 'a eqf -> 'a list list -> 'a list;

    val equal : 'a eqf -> ('a list * 'a list) -> bool;
    val contains : 'a eqf -> 'a list -> 'a -> bool;
    val subset : 'a eqf -> 'a list -> 'a list -> bool;

end;

structure ListSet : LISTSET =
struct

type 'a eqf = 'a * 'a -> bool;

fun removeDuplicates eq lst =
    let
        fun remdup ans [] = List.rev ans
          | remdup ans (x::xs) = remdup (x::ans)
                                        (List.filter (fn y => (not o eq) (x, y))
                                                     xs);
    in
        remdup [] lst
    end;

fun contains eq xs x = List.exists (fn y => eq (x, y)) xs;

fun union eq xs ys = removeDuplicates eq (xs @ ys);

fun intersection eq xs ys = let fun addInt (x, zs) =
                                    if contains eq ys x
                                    then (x::zs)
                                    else zs;
                                in List.foldr addInt [] xs end;

fun difference eq xs ys = List.filter
                              (fn x => not (contains eq ys x))
                              xs;

fun unionAll eq xss = removeDuplicates eq (List.concat xss);

fun intersectionAll eq [] = []
  | intersectionAll eq (zs::zss) = List.foldr
                                       (fn (xs, ys) => intersection eq xs ys)
                                       zs zss;

fun subset eq xs ys = List.all (fn x => contains eq ys x) xs;

fun equal eq (xs, ys) = subset eq xs ys andalso subset eq ys xs;

end;
