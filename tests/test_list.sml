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

(* mergesort *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.mergesort String.compare [])
        []
        "List: mergesort empty list"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.mergesort Int.compare [0])
        [0]
        "List: mergesort singleton"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.mergesort String.compare ["x", "x", "x", "x"])
        ["x", "x", "x", "x"]
        "List: mergesort all equal"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => let fun cmp ((a, b), (x, y)) =  String.compare (a, x);
                  in List.mergesort cmp [("a", 2), ("a", 1), ("a", 3)] end)
        [("a", 2), ("a", 1), ("a", 3)]
        "List: mergesort all equal, stable"

);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.mergesort Int.compare [2, 3, 1, 5, 7, 4, 100])
        [1, 2, 3, 4, 5, 7, 100]
        "List: mergesort int list without duplicates"
);
