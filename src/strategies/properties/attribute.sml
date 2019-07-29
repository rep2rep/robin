import "util.logging";
import "util.parser";
import "util.multiset";
import "util.type";

fun decomposeAtts [] = []
  | decomposeAtts (a::L) =
      let val (x,_,y) = Parser.breakOn "==" a;
      in (x,y) :: decomposeAtts L
      end;

fun makeHoleList [] = []
  | makeHoleList (a::L) =
      let val (x,_,y) = Parser.breakOn "=>" a;
          val x' = Type.fromString x;
          val y' = case Int.fromString y of
                      SOME n => n
                    | NONE => (print ("bad list of holes: " ^ a); raise Parser.ParseError);
      in (x',y') :: makeHoleList L
      end;

fun makeHoleMultiset L = Multiset.fromPairList (makeHoleList L);
