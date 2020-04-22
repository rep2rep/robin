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

(* rev *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => Comparison.rev Int.compare (5, 5))
        EQUAL
        "Comparison: rev to EQUAL is still EQUAL"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => Comparison.rev Int.compare (10, 5))
        LESS
        "Comparison: rev to GREATER is now LESS"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => Comparison.rev Int.compare (2, 20))
        GREATER
        "Comparison: rev to LESS is now GREATER"
);
