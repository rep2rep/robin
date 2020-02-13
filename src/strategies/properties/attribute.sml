import "util.parser";
import "util.multiset";
import "util.type";

signature ATTRIBUTE =
sig
  structure M : MULTISET;
  type T;

  val sameSort : T * T -> bool
  val equal : T * T -> bool;

  val fromType : Type.T -> T;
  val fromHoles : Type.T M.multiset -> T;
  val fromContent : Type.T -> T;
  val fromTokens : string list -> T;
  val fromNumFunction : string * real -> T;
  val fromStringFunction : string * string -> T;
  val fromFeature : string -> T;

  val getType : T -> Type.T;
  val getHoles : T -> Type.T M.multiset;
  val getTokens : T -> string list;
  val getContent : T -> Type.T;
  val getNumFunction : T -> string * real;
  val getStringFunction : T -> string * string;
  val getFeature : T -> string;

  val updateNumFunction : string -> (real -> real) -> T -> T;

  val fromString : string -> T;
  val toString : T -> string;
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
             | NumFunction of string * real (* e.g., frequency := 4 *)
             | StringFunction of string * string (* e.g., registration := icon *)
             | Feature of string;

  fun sameSort ((IsOfType _), (IsOfType _)) = true
    | sameSort ((Holes _), (Holes _)) = true
    | sameSort ((Tokens _), (Tokens _)) = true
    | sameSort ((Content _), (Content _)) = true
    | sameSort ((NumFunction (s,_)), (NumFunction (s',_))) = (s = s')
    | sameSort ((StringFunction (s,_)), (StringFunction (s',_))) = (s = s')
    | sameSort ((Feature s), (Feature s')) = (s = s')
    | sameSort _ = false

  fun equal ((IsOfType t), (IsOfType t')) = (t = t')
    | equal ((Holes m), (Holes m')) = M.equal (m,m')
    | equal ((Tokens C), (Tokens C')) = List.isPermutationOf (fn (x,y) => x = y) C C'
    | equal ((Content t), (Content t')) = (t = t')
    | equal ((NumFunction (s,n)), (NumFunction (s',n'))) = (s = s' andalso Real.==(n,n'))
    | equal ((StringFunction (s,r)), (StringFunction (s',r'))) = (s = s' andalso r = r')
    | equal ((Feature s), (Feature s')) = (s = s')
    | equal _ = false

  fun fromType t = IsOfType t;
  fun fromHoles H = Holes H;
  fun fromTokens L = Tokens L;
  fun fromContent c = Content c;
  fun fromNumFunction (s,n) = NumFunction (s,n);
  fun fromStringFunction (s,s') = StringFunction (s,s');
  fun fromFeature s = Feature s;

  fun getType (IsOfType t) = t
    | getType _ = raise Match;

  fun getHoles (Holes H) = H
    | getHoles _ = raise Match;

  fun getTokens (Tokens L) = L
    | getTokens _ = raise Match;

  fun getContent (Content c) = c
    | getContent _ = raise Match;

  fun getNumFunction (NumFunction (s,n)) = (s,n)
    | getNumFunction _ = raise Match;

  fun updateNumFunction s' f (NumFunction (s,n)) = if s' = s then NumFunction (s,f n)
                                                    else NumFunction (s,n)
    | updateNumFunction _ _ a = a;

  fun getStringFunction (StringFunction (s,s')) = (s,s')
    | getStringFunction _ = raise Match;

  fun getFeature (Feature s) = s
    | getFeature _ = raise Match;

  fun decomposeAttribute a =
      let val (x,_,y) = (Parser.breakOn ":=" a);
      in (Parser.stripSpaces x,  Parser.stripSpaces y)
      end;


(* the purpose of this function is to represent some common arithmetic equations as negative numbers.
  #t is meant to represent number of tokens.
  The idea is that some patterns don't have a fixed number of holes,
  but the number is something like "of the order of the number of tokens" (which would mean that the pattern
  involves the whole representation [or half, or a fixed fraction])*)
  fun intFromString s =
      case Int.fromString s of
              SOME n => n
            | NONE => if s = "#t" then ~3 else
                      if s = "sqrt(#t)" then ~2 else
                      if s = "log(#t)" then ~1
                        else (raise Parser.ParseError);

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
      in holeMultisetFromList L  handle Parser.ParseError => (print ("bad numerical expression in: " ^ s); raise Parser.ParseError)
      end

  fun tokenListFromString s = Parser.splitStrip "." (Parser.removeSquareBrackets s);

  fun attributeFromPair (x,y) =
      if y = "" then Feature x
      else if x = "type" then IsOfType (Type.fromString y)
      else if x = "content" then Content (Type.fromString y)
      else if x = "holes" then Holes (holeMultisetFromString y)
      else if x = "tokens" then Tokens (tokenListFromString y)
      else case Real.fromString y of SOME n => NumFunction (x,n)
                                   | NONE => StringFunction (x,y);

  val fromString = attributeFromPair o decomposeAttribute;

  fun toString (Feature s) = s
    | toString (IsOfType t) = "type := " ^ Type.toString t
    | toString (Content t) = "content := " ^ Type.toString t
    | toString (Tokens L) = "tokens := [" ^ (String.concatWith ". " L) ^ "]"
    | toString (NumFunction (s,n)) = s ^ " := " ^ Real.toString n
    | toString (StringFunction (s,s')) = s ^ " := " ^ s'
    | toString (Holes m) =
      let val L = M.toPairList m
          fun holePairToString (t,n) = (Type.toString t) ^ " => " ^ (Int.toString n)
          val sL = map holePairToString L
      in "holes := [" ^ (String.concatWith ". " sL) ^ "]"
      end;


end
