(*
Test the extensions to the Option structure
*)

(* oneOf *)

TestSuite.register (
    TestSuite.assertEqual
        (fn () => Option.oneOf [] 5)
        NONE
        "Option: oneOf empty is NONE"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => Option.oneOf [(fn _ => NONE), (fn _ => NONE)] "a")
        NONE
        "Option: oneOf all functions that return NONE is NONE"
);

TestSuite.register (
    TestSuite.assertEqual
        (fn () => Option.oneOf [(fn x => SOME (x + 1)), (fn x => SOME (x + 2))] 0)
        (SOME 1)
        "Option: oneOf returns first result to return SOME value"
);
