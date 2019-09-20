import "strategies.representation_selection";


fun filesMatchingPrefix dir prefix =
    let
        fun getWholeDir direc out = case OS.FileSys.readDir (direc) of
                                      SOME f => getWholeDir direc (f::out)
                                    | NONE => List.rev out;
        val dirstream = OS.FileSys.openDir dir;
        val filenames = getWholeDir dirstream [];
        val filteredFiles = List.filter (String.isPrefix prefix) filenames;
        fun attachDir p = dir ^ p;
    in
        map (OS.FileSys.fullPath o attachDir) filteredFiles
    end;

fun load problem =
    let val paths = filesMatchingPrefix "tables/" ("Q_table_" ^ problem)
    in map PropertyTables.loadQuestionTable paths
    end;

fun subRSVariety_rank rL =
  let fun f (_,x) = (x, CognitiveProperties.subRSVariety x)
  in List.mergesort (fn (x,y) => Real.compare (#2 x, #2 y)) (map f rL)
  end;

fun tokenRegistration_rank qL =
    let fun f (_,x) = (x, CognitiveProperties.tokenRegistration x)
    in List.mergesort (fn (x,y) => Real.compare (#2 x, #2 y)) (map f qL)
    end;
