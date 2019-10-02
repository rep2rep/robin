(*
Random.sml

Provides functions for generating random numbers and permutations.
*)



signature RANDOM =
sig

    val random : unit -> real;
    val randInt : (int * int) -> int;

    val choose : 'a list -> 'a;
    val shuffle : 'a list -> 'a list;

end;

structure Random : RANDOM =
struct

fun random () =
    let
        val maxInt = 72057594037927936; (* 2^(7*8) *)
        (* This next line is super non-portable! *)
        val rand = BinIO.openIn "/dev/urandom";
        val bytes = BinIO.inputN (rand, 7);
        val _ = BinIO.closeIn rand;
        val result = Word8Vector.foldr
                         (fn (b, i) => (i * 255) + (Word8.toInt b))
                         0 bytes;
    in
        (Real.fromInt result) / (Real.fromInt maxInt)
                                    (* Note this cannot generate 1 *)
    end;

fun randInt (low, high) = (* Range is inclusive, exclusive *)
    low + Real.floor (random() * real (high - low));

fun choose [] = raise List.Empty
  | choose xs = let val i = randInt (0, List.length xs) in List.nth (xs, i) end;

fun shuffle xs =
    let
        fun randchoice (a, b) = if random() < 0.5 then LESS else GREATER;
    in
        List.mergesort randchoice xs
    end;

end;
