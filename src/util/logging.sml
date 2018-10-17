use "base.sml";

signature LOGGING =
sig

    val write : string -> unit;
    val enable : unit -> unit;
    val disable : unit -> unit;
    val indent : unit -> unit;
    val indentBy : int -> unit;
    val dedent : unit -> unit;
    val dedentBy : int -> unit;

end;

structure Logging : LOGGING =
struct

val stderr = TextIO.stdErr;
val enabled = ref false;
val indentDepth = ref 0;

fun write s =
    let
        val indent = String.implode (List.tabulate (4 * !indentDepth, fn _ => #" "));
    in
        if (!enabled)
        then TextIO.output (stderr, indent ^ s)
        else ()
    end;

fun enable () = enabled := true;

fun disable () = enabled := false;

fun indentBy i = indentDepth := !indentDepth + i;

fun indent () = indentBy 1;

fun dedentBy i = indentBy (~i);

fun dedent () = dedentBy 1;

end;
