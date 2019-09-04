import os

def allPairs(*values):
    for i in values:
        for j in values:
            if i != j:
                yield (i, j)

tests = {
    "intsum": list(allPairs("natlang", "algebra", "dots", "graphs")),
    "medical": [(f, t) for f, t in
                allPairs("natlang", "bayes", "P-space_diagrams", "event_trees",
                         "contingency", "euler", "geometric")
                if f in {"natlang", "bayes"}],
    "birds": [(f, t) for f, t in
                allPairs("natlang", "bayes", "P-space_diagrams", "event_trees",
                         "contingency", "euler", "geometric")
                if f in {"natlang", "bayes"}]
}

if not os.path.exists("pseudo_tables"):
    os.makedirs("pseudo_tables/csv")
elif not os.path.exists("pseudo_tables/csv"):
    os.mkdir("pseudo_tables/csv")

print("""
import "strategies.representation_selection";

Logging.enable();

local

    fun filesMatchingPrefix dir prefix =
        let
            fun getWholeDir direc out = case OS.FileSys.readDir direc of
                                            SOME f => getWholeDir direc (f::out)
                                          | NONE => List.rev out;
            val dirstream = OS.FileSys.openDir dir;
            val filenames = getWholeDir dirstream [];
            val filteredFiles = List.filter (String.isPrefix prefix) filenames;
            fun attachDir p = dir ^ p;
        in
            map (OS.FileSys.fullPath o attachDir) filteredFiles
        end;

in

val _ = RepresentationSelection.init(
        filesMatchingPrefix "tables/" "RS_table_",
        filesMatchingPrefix "tables/" "correspondences_",
        filesMatchingPrefix "tables/" "Q_table_"
    );

val correspondences = !RepresentationSelection.correspondingTable';
val getQTable = RepresentationSelection.getQTable;
val getRSTable = RepresentationSelection.getRSTable;

val translateTable = PropertyTables.computePseudoQuestionTable;
val saveTable = PropertyTables.questionTableToCSV;

end;
""")

for problem, pairs in tests.items():
    for (fromRS, toRS) in pairs:
        command = f'saveTable (translateTable (getQTable ("{problem}", "{fromRS}")) (getRSTable "{toRS}") correspondences) "pseudo_tables/csv/{problem}_from_{fromRS}_to_{toRS}.csv";'
        print(command)
