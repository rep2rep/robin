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

TestSuit.register (
    let fun f (x, y) = Int.toString (x + (List.length y));
        val arg1 = 37;
        val arg2 = ["this", "is", "a", "test"];
    in TestSuit.assertEqual
           (fn () => curry f arg1 arg2)
           (f (arg1, arg2))
           "robinlib: curry on (int * 'a list) -> string"
    end
);

(* uncurry *)

TestSuit.register (
    let fun f x y = y ^ " ~ " ^ (Real.toString x);
        val arg1 = 3.14;
        val arg2 = "Pi";
    in TestSuit.assertEqual
           (fn () => uncurry f (arg1, arg2))
           (f arg1 arg2)
           "robinlib: uncorry on real -> string -> string"
    end
);

(* fails *)

TestSuit.register (
    TestSuit.assertTrue
        (fn () => fails (fn () => 5 div 0))
        "robinlib: fails on division by zero"
);

TestSuit.register (
    TestSuit.assertFalse
        (fn () => fails (fn () => 5.0 / 0.0)) (* = inf *)
        "robinlib: fails on not-failing division by zero real"
);

TestSuit.register (
    TestSuit.assertTrue
        (fn () => fails (fn () => List.hd []))
        "robinlib: fails on head of empty list"
);

TestSuit.register (
    TestSuit.assertFalse
        (fn () => fails (fn () => "hello"))
        "robinlib: fails on not-failing function"
);
