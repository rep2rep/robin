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

(* flatmap *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.flatmap (fn x => x) [])
        []
        "List: flatmap empty remains empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.flatmap (fn x => []) [1, 2, 3, 4])
        []
        "List: flatmap with all empty results is empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.flatmap (fn s => String.explode s) ["hi", "there"])
        [#"h", #"i", #"t", #"h", #"e", #"r", #"e"]
        "List: flatmap strings to chars"
);

(* update *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.update (fn _ => true) (fn x => x) [])
        []
        "List: update empty is empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.update (fn _ => true) (fn x => x) [1, 2, 3, 4, 5])
        [1, 2, 3, 4, 5]
        "List: update with identity is identity"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.update (fn _ => false) (fn x => 2 * x) [1, 2, 3, 4, 5])
        [1, 2, 3, 4, 5]
        "List: updating nothing is identity"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.update (fn x => x = 3) (fn _ => 42) [1, 2, 3, 4, 5])
        [1, 2, 42, 4, 5]
        "List: updating one element works"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.update (fn x => x < 3) (fn x => x + 50) [5, 4, 3, 2, 1])
        [5, 4, 3, 52, 51]
        "List: updating multiple elements works"
);

(* product *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.product ([], []))
        []
        "List: product both empty is empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.product ([1, 2, 3], []))
        []
        "List: product right empty is still empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.product ([], [4, 5, 6]))
        []
        "List: product left empty is still empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.product ([1, 2, 3], [10, 20, 30]))
        [(1, 10), (1, 20), (1, 30),
         (2, 10), (2, 20), (2, 30),
         (3, 10), (3, 20), (3, 30)]
        "List: product with homogeneous lists (int)"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.product (["a", "b", "c"], [1, 2]))
        [("a", 1), ("a", 2),
         ("b", 1), ("b", 2),
         ("c", 1), ("c", 2)]
        "List: product with heterogeneous lists (string, int)"
);

(* toString *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.toString Int.toString [])
        "[]"
        "List: toString of empty is \"[]\""
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.toString (fn x => x) ["hello"])
        "[hello]"
        "List: toString of singleton has no commas"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.toString Real.toString [1.2, 2.4])
        "[1.2, 2.4]"
        "List: toString on real list"
);


(* unfold *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.unfold (fn x => NONE) 0)
        []
        "List: unfold immediate NONE is empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.unfold (fn x => if x < 5 then SOME(Int.toString x, x+1) else NONE) 0)
        ["0", "1", "2", "3", "4"]
        "List: unfold 0:int to strings of ints"
);

(* replicate *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.replicate 0 1)
        []
        "List: replicate zero times is empty"
);

TestSuite.register (
    TestSuite.assertError
        (fn () => List.replicate ~2 1)
        Size
        "List: replicate negative is a Size error"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.replicate 5 "a")
        ["a", "a", "a", "a", "a"]
        "List: replicate string"
);

(* max, min *)

let
    fun testEmpty fnc fnstr =
        TestSuite.register (
            TestSuite.assertError
                (fn () => fnc Int.compare [])
                List.Empty
                ("List: " ^ fnstr ^ " of an empty list is an error"));
    fun testSingleton fnc fnstr =
        TestSuite.register (
            TestSuite.assertEqual
                (fn () => fnc Int.compare [42])
                42
                ("List: " ^ fnstr ^ " of singleton is its value"));
    fun testRepeated fnc fnstr =
        TestSuite.register (
            TestSuite.assertEqual
                (fn () => fnc Int.compare [2, 2, 2, 2, 2])
                2
                ("List: " ^ fnstr ^ " of repeated element is that element"));
    fun testGeneral fnc fnstr =
        TestSuite.register (
            TestSuite.assertEqual
                (fn () => fnc Int.compare [3, 1, 6, 3, 1, 2, 7, 4])
                (if fnstr = "min" then 1 else 7)
                ("List: " ^ fnstr ^ " is correct"));
    val tests = [testEmpty, testSingleton, testRepeated, testGeneral];
in
    map (fn t => t List.min "min") tests;
    map (fn t => t List.max "max") tests;
    ()
end;

(* argmin, argmax *)

let
    fun testEmpty fnc fnstr =
        TestSuite.register (
            TestSuite.assertError
                (fn () => #1 (fnc Real.fromInt []))
                List.Empty
                ("List: " ^ fnstr ^ " of an empty list is an error"));
    fun testSingleton fnc fnstr =
        TestSuite.register (
            TestSuite.assertEqual
                (fn () => #1 (fnc Real.fromInt [42]))
                42
                ("List: " ^ fnstr ^ " of singleton is its value"));
    fun testRepeated fnc fnstr =
        TestSuite.register (
            TestSuite.assertEqual
                (fn () => #1 (fnc Real.fromInt [2, 2, 2, 2, 2]))
                2
                ("List: " ^ fnstr ^ " of repeated element is that element"));
    fun testGeneral fnc fnstr =
        TestSuite.register (
            TestSuite.assertEqual
                (fn () => #1 (fnc Real.fromInt [3, 1, 6, 3, 1, 2, 7, 4]))
                (if fnstr = "argmin" then 1 else 7)
                ("List: " ^ fnstr ^ " is correct"));
    val tests = [testEmpty, testSingleton, testRepeated, testGeneral];
in
    map (fn t => t List.argmin "argmin") tests;
    map (fn t => t List.argmax "argmax") tests;
    ()
end;

(* takeWhile *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.takeWhile (fn x => true) [])
        []
        "List: takeWhile empty is empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.takeWhile (fn x => false) [1, 2, 3, 4, 5])
        []
        "List: takeWhile false is empty"
);

TestSuite.register (
    let val l = [5, 4, 3, 2, 1];
    in TestSuite.assertEqual
           (fn () => List.takeWhile (fn x => true) l)
           l
           "List: takeWhile true is original list"
    end
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.takeWhile (fn x => String.size x = 3) ["sup", "hey", "hello", "wow"])
        ["sup", "hey"]
        "List: takeWhile on string list"
);

(* dropWhile *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.dropWhile (fn x => false) [])
        []
        "List: dropWhile empty is empty"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.dropWhile (fn x => true) [1, 2, 3, 4, 5])
        []
        "List: dropWhile true is empty"
);

TestSuite.register (
    let val l = [5, 4, 3, 2, 1];
    in TestSuite.assertEqual
           (fn () => List.dropWhile (fn x => false) l)
           l
           "List: dropWhile false is original list"
    end
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.dropWhile (fn x => String.size x = 3) ["sup", "hey", "hello", "wow"])
        ["hello", "wow"]
        "List: dropWhile on string list"
);

(* split *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.split ([], 0))
        ([], [])
        "List: split empty at position 0 is fine"
);

TestSuite.register (
    TestSuite.assertError
        (fn () => List.split (["a"], 2))
        Subscript
        "List: split beyond length is error"
);

TestSuite.register (
    let val l = [1, 2, 3, 4];
        val k = List.length l;
    in
        TestSuite.assertEqual
            (fn () => List.split (l, k))
            (l, [])
            "List: split at end is original with an empty list"
    end
);

TestSuite.register (
    let val l = [1, 2, 3, 4];
    in
        TestSuite.assertEqual
            (fn () => List.split (l, 0))
            ([], l)
            "List: split at 0 is an empty list with original"
    end
);

TestSuite.register (
    TestSuite.assertError
        (fn () => List.split ([1, 2, 3, 4, 5], ~1))
        Subscript
        "List: split on negative number is an error"
);

TestSuite.register (
    let val l = ["hi", "salut", "hola", "kia ora"];
        val k = 2;
    in
        TestSuite.assertTrue
            (fn () => (op@ (List.split (l, k))) = l)
            "List: split then concat is original list"
    end
);

(* rotate *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.rotate 5 [])
        []
        "List: rotate empty list is still empty"
);

TestSuite.register (
    let val l = ["a", "b", "c", "d"] in
        TestSuite.assertEqual
            (fn () => List.rotate 0 l)
            l
            "List: rotate by zero is original"
    end
);

TestSuite.register (
    let val l = ["a", "b", "c", "d"] in
        TestSuite.assertEqual
            (fn () => List.rotate (List.length l) l)
            l
            "List: rotate by entire length is original"
    end
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.rotate 2 [1, 2, 3, 4, 5])
        [3, 4, 5, 1, 2]
        "List: rotate by value in [0, ..., length - 1]"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.rotate ~2 [1, 2, 3, 4, 5])
        [4, 5, 1, 2, 3]
        "List: rotate by negative value moves backward"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => List.rotate 8 [1, 2, 3, 4, 5])
        [4, 5, 1, 2, 3]
        "List: rotate by value beyond length wraps around"
);

(* weightedSumIndexed, sumIndexed, weightedSum, sum *)

let
    val w = fn r => 1.0;
    val i = fn r => r;
    fun mkTest (f, s) =
        TestSuite.register (
            TestSuite.assertTrue
                (fn () => Real.== ((f []), 0.0))
                ("List: " ^ s ^ " on empty list is 0")
        );
in
    map mkTest [(List.weightedSumIndexed w i, "weightedSumIndexed"),
                (List.sumIndexed i, "sumIndexed"),
                (List.weightedSum w, "weightedSum"),
                (List.sum, "sum")]
end;

TestSuite.register (
    TestSuite.assertTrue
        (fn () => Real.==(List.weightedSumIndexed (fn (_, c) => Real.fromInt c)
                                                  (Real.fromInt o String.size o #1)
                                                  [("hi", 2), ("hey", 3), ("hello", 1)],
                          18.0))
        "List: weightedSumIndexed on (string * int) list, indexing by string length, weighting by the 'count'"
);

TestSuite.register (
    TestSuite.assertTrue
        (fn () => Real.==(List.sumIndexed (Real.fromInt o List.length) [[0], [0, 0], [0]],
                          4.0))
        "List: sumIndexed on int list list, indexed by length"
);

TestSuite.register (
    TestSuite.assertTrue
        (fn () => Real.== (List.weightedSum (fn x => x * x) [1.0, 2.0, 1.0, 0.5],
                           10.125))
        "List: weightedSum on where weight is the square of the value"
);

TestSuite.register (
    TestSuite.assertTrue
        (fn () => Real.== (List.sum [4.0, 1.0, 3.0, 2.6],
                           10.6))
        "List: sum works correctly"
);

(* weightAvgIndexed, avgIndexed, weightedAvg, avg *)

let
    val w = fn r => 1.0;
    val i = fn r => r;
    fun mkTest (f, s) =
        TestSuite.register (
            TestSuite.assertError
                (fn () => f [])
                List.Empty
                ("List: " ^ s ^ " on empty list is an Empty error")
        );
in
    map mkTest [(List.weightedAvgIndexed w i, "weightedAvgIndexed"),
                (List.avgIndexed i, "avgIndexed"),
                (List.weightedAvg w, "weightedAvg"),
                (List.avg, "avg")]
end;

TestSuite.register (
    TestSuite.assertTrue
        (fn () => Real.==(List.weightedAvgIndexed (fn (_, c) => Real.fromInt c)
                                                  (Real.fromInt o String.size o #1)
                                                  [("hi", 2), ("hey", 3), ("hello", 1)],
                          3.0))
        "List: weightedAvgIndexed on (string * int) list, indexing by string length, weighting by the 'count'"
);

TestSuite.register (
    TestSuite.assertTrue
        (fn () => Real.==(List.avgIndexed (Real.fromInt o List.length) [[0], [0, 0], [0]],
                          4.0/3.0))
        "List: avgIndexed on int list list, indexed by length"
);

TestSuite.register (
    TestSuite.assertTrue
        (fn () => Real.== (List.weightedAvg (fn x => x * x) [1.0, 2.0, 1.0, 0.5],
                           1.62))
        "List: weightedAvg on where weight is the square of the value"
);

TestSuite.register (
    TestSuite.assertTrue
        (fn () => Real.== (List.avg [4.0, 1.0, 3.0, 2.6],
                           2.65))
        "List: avg works correctly"
);
