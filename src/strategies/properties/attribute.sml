import "util.parser";
import "util.multiset";
import "util.type";

signature ATTRIBUTE =
sig
  structure M : MULTISET;
  type T;
  val fromType : Type.T -> T;
  val fromHoles : Type.T M.multiset -> T;
  val fromContent : Type.T -> T;
  val fromFeature : string -> T;
  val read : string -> T list;
  val toString : T -> string;
  val getType : T list -> Type.T option
end


structure Attribute : ATTRIBUTE =
struct

  structure M = Multiset(struct
                         type t = Type.T;
                         val compare = Type.compare;
                         val fmt = Type.toString;
                         end);

  datatype T = IsOfType of Type.T
             | Holes of Type.T M.multiset
             | Tokens of string list
             | Content of Type.T
             | Occurrences of int
             | Feature of string;
(*
  fun compare (IsOfType t, IsOfType t') = Type.compare (t,t')
    | compare (IsOfType _, _) = LESS
    | compare (_, IsOfType _) = GREATER
    | compare (Holes L, Holes L') = M.compare
    | compare (Holes _, _) =
    | compare (_, Holes _) =
    | compare (IsOfType t, IsOfType t') =
    | compare (IsOfType _, _) =
    | compare (_, IsOfType _) =
    | compare (IsOfType t, IsOfType t') = *)
  fun fromType t = IsOfType t;
  fun fromHoles H = Holes H;
  fun fromTokens L = Tokens L;
  fun fromContent c = Content c;
  fun fromFeature s = Feature s;

  fun decomposeAttribute a =
      let val (x,_,y) = (Parser.breakOn ":=" a);
      in (Parser.stripSpaces x,  Parser.stripSpaces y)
      end;

  fun intFromString s =
      case Int.fromString s of
              SOME n => n
            | NONE => if s = "#t" then ~1 else if s = "sqrt(#t)" then ~2 else if s = "log(#t)" then ~3
                        else (print ("bad numerical expression: " ^ s); raise Parser.ParseError);

  fun makeHoleList [] = []
    | makeHoleList (a::L) =
        let val (sx,_,sy) = Parser.breakOn "=>" a;
            val y = Parser.stripSpaces sy;
            val x' = Type.fromString sx;
            val y' = intFromString y;
        in (x',y') :: makeHoleList L
        end;

  fun holeMultisetFromList L = M.fromPairList (makeHoleList L);

(* holeMultisetFromString takes a string with square brackets with
    type/multiplicities pairs separated by dots [int => 1. real => 4. set => 1]*)
  fun holeMultisetFromString s =
      let val L = Parser.splitStrip "." (Parser.removeSquareBrackets s)
      in holeMultisetFromList L
      end

  fun tokenListFromString s = Parser.splitStrip "." (Parser.removeSquareBrackets s)

  fun holePairToString (t,n) = (Type.toString t) ^ " => " ^ (Int.toString n)

  fun attributeFromPair (x,y) =
      if y = "" then Feature x
      else if x = "type" then IsOfType (Type.fromString y)
      else if x = "content" then Content (Type.fromString y)
      else if x = "holes" then Holes (holeMultisetFromString y)
      else if x = "tokens" then Tokens (tokenListFromString y)
      else if x = "occurrences" then Occurrences (intFromString y)
      else (print ("error parsing attribute: " ^ x ^ " " ^ y); raise Parser.ParseError)

  fun read s =
      let val s' = (Parser.removeBraces o Parser.stripSpaces) s
          val Astrs = Parser.splitStrip ";" s'
          val A = map (attributeFromPair o decomposeAttribute) Astrs
      in  A
      end

  fun getType [] = NONE
    | getType (a::L) = case a of IsOfType t => SOME t
                                      | _ => getType L;

  fun toString (Feature s) = s
    | toString (IsOfType t) = "type := " ^ Type.toString t
    | toString (Occurrences n) = "occurrences := " ^ Int.toString n
    | toString (Content t) = "content := " ^ Type.toString t
    | toString (Tokens L) = "tokens := [" ^ (String.concat (intersperse ". " L)) ^ "]"
    | toString (Holes m) =
      let val L = M.toPairList m
          val sL = map holePairToString L
      in "holes := [" ^ (String.concat (intersperse ". " sL)) ^ "]"
      end;


(*)
  fun getHoles [] = []
    | getHoles ((x,y)::L) =
        if x = "holes"
        then let Parser.splitStrip "." (Parser.removeSquareBrackets y)
        else getHoles L*)

end
