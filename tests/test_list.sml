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

(* removeDuplicates *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.removeDuplicates [])
        []
        "List: removeDuplicates on empty list"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.removeDuplicates [9, 7, 3, 2, 6])
        [9, 7, 3, 2, 6]
        "List: removeDuplicates on list of unique integers"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.removeDuplicates [1, 1, 2, 3, 5, 8, 1])
        [1, 2, 3, 5, 8]
        "List: removeDuplicates on integers, single duplicated element"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.removeDuplicates [1, 2, 1, 2, 1, 2, 3, 3, 3])
        [1, 2, 3]
        "List: removeDuplicates on integers, multiple duplicated elements"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.removeDuplicates ["a", "a", "a", "a"])
        ["a"]
        "List: removeDuplicates on strings, all elements the same"
);
