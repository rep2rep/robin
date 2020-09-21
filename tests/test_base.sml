signature TESTSUITE =
sig

    exception TestFail of string;

    type test;

    val register : test -> unit;

    val assertEqual : (unit -> ''a) -> ''a -> string -> test;
    val assertTrue : (unit -> bool) -> string -> test;
    val assertFalse : (unit -> bool) -> string -> test;
    val assertError : (unit -> 'a) -> exn -> string -> test;

    val run : unit -> (int * string list);

end;

structure TestSuite : TESTSUITE =
struct

exception TestFail of string;

type test = unit -> unit;

val tests: test list ref = ref [];

fun register f = tests := (f::(!tests));

fun assertEqual f a s =
    fn () => (if f () = a then () else raise TestFail s)
             handle TestFail s => raise TestFail (s ^ "\n -> Failed.")
                  | e => raise TestFail (s ^ "\n -> Failed due to exception " ^
                                         (exnMessage e) ^ ".");

fun assertTrue f s = assertEqual f true s;

fun assertFalse f s = assertEqual f false s;

fun assertError f e s =
    fn () => let val _ = f () in raise TestFail s end
             handle TestFail s => raise TestFail (s ^ "\n -> Failed to raise any exception.")
                  | e' => if (exnMessage e) = (exnMessage e')
                          then ()
                          else raise TestFail (s ^ "\n -> Failed with unexpected exception " ^
                                               (exnMessage e') ^ ".");

fun run () =
    let
        fun loop (count, failures) [] = (count, List.rev failures)
          | loop (count, failures) (f::fs) =
            let val (c, s) = (f (); (true, ""))
                             handle TestFail s' => (false, s');
                val _ = print (if c then "." else "!");
            in loop (count + (if c then 0 else 1), s::failures) fs end;
        fun printAll [] = ()
          | printAll (s::ss) = (if s <> "" then print (s ^ "\n") else ();
                                printAll ss);

        val (failCount, errors) = loop (0, []) (List.rev (!tests));
        val testCount = List.length (!tests);
        val _ = print "\n";
        val ratio = "(" ^ (Int.toString (testCount - failCount)) ^ "/" ^ (Int.toString testCount) ^ ")";
        val _ = if failCount = 0
                then print ("\nAll tests passed " ^ ratio ^ ".\n")
                else (print ((Int.toString failCount) ^ " tests failed " ^ ratio ^ ".\n\n");
                      printAll errors);
    in (failCount, errors) end;

end;
