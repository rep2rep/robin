use "./tests/test_base.sml";

fun registerAll () =
    let
        val test_dir = "./tests/";
        val test_dir_stream = OS.FileSys.openDir test_dir;
        val test_files =
            let fun streamToList ans s =
                    case (OS.FileSys.readDir s) of
                        SOME f => streamToList ((test_dir ^ f)::ans) s
                      | NONE => List.rev ans
            in streamToList [] test_dir_stream end;
        fun isTest s = s <> "./tests/test_base.sml"
                       andalso String.isPrefix (test_dir ^ "test_") s
                       andalso String.isSuffix ".sml" s;
    in List.app use (List.filter isTest test_files) end;

fun main () =
    let val _ = registerAll ();
        val (c, _) = TestSuit.run();
    in if c = 0 then 0 else 1 end;
