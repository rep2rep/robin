(*
Test the extended List structure.
*)

(* remove *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.remove 5 [3, 5, 7, 9])
        [3, 7, 9]
        "List: remove on ints, no duplicates"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.remove "x" ["w", "x", "z", "y", "x", "a"])
        (* This removes ALL occurrences of x *)
        ["w", "z", "y", "a"]
        "List: remove on strings, with duplicates"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.remove #"a" [#"b", #"c"])
        [#"b", #"c"]
        "List: remove on chars, needle not occurring"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.remove false [])
        []
        "List: remove on bools, empty list"
);
