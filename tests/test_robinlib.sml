(*
Test the robinlib utility functions
*)

(* import *)
(* This is pretty much untestable without a lot of mock work.
   It's probably fine, we'd have noticed it breaking! *)

(* mappair *)

TestSuit.register (
    TestSuit.assertEqual
        (fn () => mappair (fn x => x + 1) (10, 100))
        (11, 101)
        "robinlib: mappair across int pair"
);

TestSuit.register (
    TestSuit.assertEqual
        (fn () => mappair (fn (a, b) => (b, a)) ((2, "a"), (7, "b")))
        (("a", 2), ("b", 7))
        "robinlib: mappair across (string * int) pair"
);

(* mapfst *)

(* mapsnd *)

(* flip *)

(* curry *)

(* uncurry *)

(* fails *)
