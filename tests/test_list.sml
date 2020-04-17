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

(* intersperse *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.intersperse 0 [])
        []
        "List: intersperse into empty still empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.intersperse false [true])
        [true]
        "List: intersperse into singleton still singleton"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.intersperse "x" ["a", "b", "c", "d", "e"])
        ["a", "x", "b", "x", "c", "x", "d", "x", "e"]
        "List: intersperse string"
);

(* enumerate *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.enumerate [])
        []
        "List: enumerate empty remains empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.enumerate ["a", "b", "c"])
        [(0, "a"), (1, "b"), (2, "c")]
        "List: enumerate string list"
);

(* enumerateFrom *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.enumerateFrom 100 [])
        []
        "List: enumerateFrom empty remains empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.enumerateFrom 100 [1, 2, 3])
        [(100, 1), (101, 2), (102, 3)]
        "List: enumerateFrom int list"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.enumerateFrom ~4 [false, true, false])
        [(~4, false), (~3, true), (~2, false)]
        "List: enumerateFrom negative start on bool list"
);

(* filterOption *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.filterOption [])
        []
        "List: filterOption empty remains empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.filterOption [SOME 1, SOME 2, SOME 3])
        [1, 2, 3]
        "List: filterOption all SOME"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.filterOption [NONE, NONE, NONE])
        []
        "List: filterOption all NONE"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.filterOption [SOME "a", NONE, SOME "b", SOME "c", NONE, NONE])
        ["a", "b", "c"]
        "List: filterOption on string option list"
);

(* isPermutationof *)

TestSuite.register (
    TestSuite.assertTrue
        (fn () => List.isPermutationOf op= [] [])
        "List: isPermutationOf empty"
);

TestSuite.register (
    TestSuite.assertTrue
        (fn () => List.isPermutationOf op= [1] [1])
        "List: isPermutationof singleton integer"
);

TestSuite.register (
    TestSuite.assertFalse
        (fn () => List.isPermutationOf op= [1, 2, 2] [2, 1])
        "List: isPermutationOf understands duplicates"
);

TestSuite.register (
    TestSuite.assertFalse
        (fn () => List.isPermutationOf op= [1, 1, 2, 2, 2] [1, 1, 1, 2, 2])
        "List: isPermutationOf handles duplicates correctly"
);

TestSuite.register (
    TestSuite.assertTrue
        (fn () => List.isPermutationOf op= [1, 2, 3, 4, 5, 6] [2, 4, 1, 5, 6, 3])
        "List: isPermutationOf works"
);

(* mapArgs *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.mapArgs (fn x => x) [])
        []
        "List: mapArgs empty remains empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.mapArgs Int.toString [1, 2, 3])
        [(1, "1"), (2, "2"), (3, "3")]
        "List: mapArgs on int list"
);
