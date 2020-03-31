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

TestSuit.register (
    TestSuit.assertEqual
        (fn () => mapfst List.length ([8.2, 4.9, 7.0], "test"))
        (3, "test")
        "robinlib: mapfst on (real list * string) to (int * string)"
);

(* mapsnd *)

TestSuit.register (
    TestSuit.assertEqual
        (fn () => mapsnd String.explode (42, "test"))
        (42, [#"t", #"e", #"s", #"t"])
        "robinlib: mapsnd on (int * string) to (int * char list)"
);

(* flip *)

TestSuit.register (
    TestSuit.assertEqual
        (fn () => flip ("blah", [3, 1, 4]))
        ([3, 1, 4], "blah")
        "robinlib: flip on (string * int list)"
);

(* curry *)

(* uncurry *)

(* fails *)
