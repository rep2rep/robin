import "util.parser";
import "util.multiset";
import "util.type";

signature ATTRIBUTE =
sig
  type T;
  val fromString : string -> T;
  val stringOf : T -> string;
end


structure Attribute : ATTRIBUTE =
struct

  datatype T = OfType of Type.T | Holes of Type.T Multiset.T | Content of Type.T | Feature of string;

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
(*)
  fun getHoles [] = []
    | getHoles ((x,y)::L) =
        if x = "holes"
        then let Parser.splitStrip "." (Parser.removeSquareBrackets y)
        else getHoles L*)

end
