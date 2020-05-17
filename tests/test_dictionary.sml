(* Test dictionary.sml *)

import "util.dictionary";

structure D = Dictionary(struct type k = string;
                                val compare = String.compare;
                                val fmt = fn s => s;
                         end);

(* toPairList *)
(* This remains untested because... well how do you test it? *)

(* empty *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => D.toPairList (D.empty ()))
        []
        "Dictionary: empty dictionary is definitely empty!"
);

(* fromPairList *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => D.toPairList (D.fromPairList []))
        []
        "Dictionary: fromPairList empty input manes an empty dictionary"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => D.toPairList (D.fromPairList [("b", 2), ("a", 1)]))
        [("a", 1), ("b", 2)]
        "Dictionary: fromPairList output is ordered"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => D.toPairList (D.fromPairList [("b", 2), ("a", 5), ("a", 1)]))
        [("a", 1), ("b", 2)]
        "Dictionary: fromPairList uses last value to resolve duplicates"
);

(* insert *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => let val d = D.empty ();
                      val _ = D.insert d ("a", 1);
                  in D.toPairList d end)
        [("a", 1)]
        "Dictionary: insert into empty is the singleton"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => let val d = D.fromPairList [("a", 1), ("c", 3)];
                      val _ = D.insert d ("b", 2);
                  in D.toPairList d end)
        [("a", 1), ("b", 2), ("c", 3)]
        "Dictionary: insert into an existing dictionary, no conflict"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => let val d = D.fromPairList [("a", 1), ("b", 2)];
                      val _ = D.insert d ("b", 3);
                  in D.toPairList d end)
        [("a", 1), ("b", 3)]
        "Dictionary: insert, overwriting an element in an existing dictionary"
);

(* remove *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => let val d = D.empty ();
                      val _ = D.remove d "a";
                  in D.toPairList d end)
        []
        "Dictionary: remove from empty dictionary does nothing"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => let val d = D.fromPairList [("a", 1), ("b", 2)];
                      val _ = D.remove d "c";
                  in D.toPairList d end)
        [("a", 1), ("b", 2)]
        "Dictionary: removing an element that is not in the dictionary does nothing"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => let val d = D.fromPairList [("a", 1), ("b",2)];
                      val _ = D.remove d "a";
                  in D.toPairList d end)
        [("b", 2)]
        "Dictionary: removing an existing element removes it"
);

(* get *)

TestSuite.register (
    TestSuite.assertError
        (fn () => let val d = D.empty ();
                  in D.get d "a" end)
        D.KeyError
        "Dictionary: get from empty dictionary is a KeyError"
);

TestSuite.register (
    TestSuite.assertError
        (fn () => let val d = D.fromPairList [("a", 1), ("b", 2)];
                  in D.get d "c" end)
        D.KeyError
        "Dictionary: get key not in dictionary is a KeyError"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => let val d = D.fromPairList [("a", 1), ("b", 2)];
                  in D.get d "b" end)
        2
        "Dictionary: get key returns associated value"
);

(* update *)

TestSuite.register (
    TestSuite.assertError
        (fn () => let val d = D.empty ();
                  in D.update d "a" (fn s => s) end)
        D.KeyError
        "Dictionary: update empty dictionary is a key error"
);

TestSuite.register (
    TestSuite.assertError
        (fn () => let val d = D.fromPairList [("a", 1), ("b", 2)];
                  in D.update d "c" (fn s => s) end)
        D.KeyError
        "Dictionary: update missing key is a KeyError"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => let val d = D.fromPairList [("a", 1), ("b", 2)];
                      val v = D.update d "a" (fn v => 5*v);
         in (v, D.toPairList d) end)
        (5, [("a", 5), ("b", 2)])
        "Dictionary: update existing key returns new value and updates the dictionary"
);
