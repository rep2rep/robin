import "strategies.representation_selection";


fun setOfDict x = #2 (hd (PropertyTables.FileDict.items x));

val BirdsBayes = setOfDict
                  (PropertyTables.loadQuestionTable
                      "/home/daniel/robin/tables/Q_table_birds_bayes.csv");

val Bayes = setOfDict
                (PropertyTables.loadRepresentationTable
                    "/home/daniel/robin/tables/RS_table_bayes.csv");
val Geometric = setOfDict
                    (PropertyTables.loadRepresentationTable
                        "/home/daniel/robin/tables/RS_table_geometric.csv");
val Natlang = setOfDict
                (PropertyTables.loadRepresentationTable
                    "/home/daniel/robin/tables/RS_table_natlang.csv");
val Contingency = setOfDict
                      (PropertyTables.loadRepresentationTable
                          "/home/daniel/robin/tables/RS_table_contingency.csv");
val Euler = setOfDict
                (PropertyTables.loadRepresentationTable
                    "/home/daniel/robin/tables/RS_table_euler.csv");
val Dots = setOfDict
              (PropertyTables.loadRepresentationTable
                  "/home/daniel/robin/tables/RS_table_dots.csv");
val PS = setOfDict
            (PropertyTables.loadRepresentationTable
                "/home/daniel/robin/tables/RS_table_1dimps.csv");
