(*
Test the robinlib utility functions
*)

(* import *)
(* This is pretty much untestable without a lot of mock work.
   It's probably fine, we'd have noticed it breaking! *)

(* mappair *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => mappair (fn x => x + 1) (10, 100))
        (11, 101)
        "robinlib: mappair across int pair"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => mappair (fn (a, b) => (b, a)) ((2, "a"), (7, "b")))
        (("a", 2), ("b", 7))
        "robinlib: mappair across (string * int) pair"
);

(* mapfst *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => mapfst List.length ([8.2, 4.9, 7.0], "test"))
        (3, "test")
        "robinlib: mapfst on (real list * string) to (int * string)"
);

(* mapsnd *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => mapsnd String.explode (42, "test"))
        (42, [#"t", #"e", #"s", #"t"])
        "robinlib: mapsnd on (int * string) to (int * char list)"
);

(* flip *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => flip ("blah", [3, 1, 4]))
        ([3, 1, 4], "blah")
        "robinlib: flip on (string * int list)"
);

(* curry *)

TestSuite.register (
    let fun f (x, y) = Int.toString (x + (List.length y));
        val arg1 = 37;
        val arg2 = ["this", "is", "a", "test"];
    in TestSuite.assertEqual
           (fn () => curry f arg1 arg2)
           (f (arg1, arg2))
           "robinlib: curry on (int * 'a list) -> string"
    end
);

(* uncurry *)

TestSuite.register (
    let fun f x y = y ^ " ~ " ^ (Real.toString x);
        val arg1 = 3.14;
        val arg2 = "Pi";
    in TestSuite.assertEqual
           (fn () => uncurry f (arg1, arg2))
           (f arg1 arg2)
           "robinlib: uncorry on real -> string -> string"
    end
);

(* fails *)

TestSuite.register (
    TestSuite.assertTrue
        (fn () => fails (fn () => 5 div 0))
        "robinlib: fails on division by zero"
);

TestSuite.register (
    TestSuite.assertFalse
        (fn () => fails (fn () => 5.0 / 0.0)) (* = inf *)
        "robinlib: fails on not-failing division by zero real"
);

TestSuite.register (
    TestSuite.assertTrue
        (fn () => fails (fn () => List.hd []))
        "robinlib: fails on head of empty list"
);

TestSuite.register (
    TestSuite.assertFalse
        (fn () => fails (fn () => "hello"))
        "robinlib: fails on not-failing function"
);
