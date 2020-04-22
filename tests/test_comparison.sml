(*
Test the new Comparison structure
*)

(* join *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => Comparison.join Int.compare String.compare ((5, "a"), (5, "a")))
        EQUAL
        "Comparison: join on pair for EQUAL"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => Comparison.join Int.compare String.compare ((5, "a"), (5, "b")))
        LESS
        "Comparison: join on pair for LESS on second"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => Comparison.join Int.compare String.compare ((6, "a"), (5, "b")))
        GREATER
        "Comparison: join on pair for GREATER on first"
);
