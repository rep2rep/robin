signature TESTSUIT =
sig

    exception TestFail of string;

    val register : (unit -> unit) -> unit;

    val assertEqual : (unit -> ''a) -> ''a -> string -> (unit -> unit);

    val run : unit -> (int * string list);

end;

structure TestSuit : TESTSUIT =
struct

exception TestFail of string;

val tests: (unit -> unit) list ref = ref [];

fun register f = tests := (f::(!tests));

fun assertEqual f a s =
    fn () => if f () = a then () else raise TestFail s;

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
        val _ = print "\n";
        val _ = if failCount = 0
                then print "\nAll tests passed.\n"
                else (print ((Int.toString failCount) ^ " tests failed.\n\n");
                      printAll errors);
    in (failCount, errors) end;

end;
