import "util.set";
import "util.type";
import "util.dictionary";
import "util.parser";

import "strategies.properties.importance";
import "strategies.properties.kind";
import "strategies.properties.attribute";

signature PROPERTY =
sig
    exception ParseError;

    datatype value = Label of string | Number of int | Boolean of bool | Type of Type.T | Raw of string;
    type property;
    structure M : MULTISET;
    val toListHandlingNegatives : (int -> int) -> Type.T M.multiset -> Type.T list;
    val toPairList : Type.T M.multiset -> (Type.T * int) list;
    val countUnique : Type.T M.multiset -> int;
    val size : Type.T M.multiset -> int;
    val contains : Type.T M.multiset -> Type.T -> bool;
    exception NegativeCount of Type.T * int;

    exception Error of string;

    val kindOf : property -> Kind.kind;
    val valueOf : property -> value;

    val LabelOf : property -> string;
    val NumberOf : property -> int;
    val BooleanOf : property -> bool;
    val TypeOf : property -> Type.T;

    val attributesOf : property -> Attribute.T list;
    val getTypeOfValue : property -> Type.T;
    val getHoles : property -> Type.T M.multiset;
    val getTokens : property -> string list;
    val getContent : property -> Type.T;
    val getStringFunction : string -> property -> (string * string);
    val getNumFunction : string -> property -> (string * real);
    val getFeatures : property -> string list;

    val updateNumFunction : string -> (real -> real) -> property -> property;

    val compare : property * property -> order;
    val match : property * property -> bool;

    val toString : property -> string;
    val fromKindValueAttributes : Kind.kind * value * (Attribute.T list) -> property;
    val toKindValueAttributes : property -> Kind.kind * value * (Attribute.T list);
    val fromString : string -> property;
    val findAttributes : string -> (string * Attribute.T list);
end;

structure Property :> PROPERTY =
struct

datatype value = Label of string | Number of int | Boolean of bool | Type of Type.T | Raw of string;

fun stringOfValue (Label s) = s
  | stringOfValue (Number n) = Int.toString n
  | stringOfValue (Boolean b) = if b then "TRUE" else "FALSE"
  | stringOfValue (Type t) = Type.toString t
  | stringOfValue (Raw s) = "RAW: " ^ s;

type property = (Kind.kind * value * Attribute.T list);
structure M = Attribute.M

fun toListHandlingNegatives f m = M.toListHandlingNegatives f m;
fun toPairList m = M.toPairList m;
fun countUnique m = M.countUnique m;
fun size m = M.size m;
fun contains m a = M.contains m a;
exception NegativeCount = M.NegativeCount;

exception ParseError;
exception Error of string;

fun toKindValuePair (k,v,_) = (k,v)

fun kindOf (k,_,_) = k;
fun valueOf (_,v,_) = v;

fun LabelOf p =
    case valueOf p of Label s => s
                    | _ => raise Error "Not a Label";

fun NumberOf p =
    case valueOf p of Number n => n
                    | _ => raise Error "Not a Number";

fun BooleanOf p =
    case valueOf p of Boolean b => b
                    | _ => raise Error "Not a Boolean";

fun TypeOf p =
    case valueOf p of Type t => t
                    | _ => raise Error "Not a Type";

fun attributesOf (_,_,A) = A;

fun typeFromAttributes [] = NONE
  | typeFromAttributes (a::L) = SOME (Attribute.getType a) handle Match => typeFromAttributes L;

exception NoAttribute of string;

fun getTypeOfValue (_,_,A) = case typeFromAttributes A of SOME t => t
                                                     | NONE => raise NoAttribute "type";

fun getHoles (_,_,[]) = raise NoAttribute "holes"
  | getHoles (k,v,(a::L)) = Attribute.getHoles a handle Match => getHoles (k,v,L);

fun getTokens (_,_,[]) = raise NoAttribute "tokens"
  | getTokens (k,v,(a::L)) = Attribute.getTokens a handle Match => getTokens (k,v,L);

fun getContent (_,_,[]) = raise NoAttribute "content"
  | getContent (k,v,(a::L)) = Attribute.getContent a handle Match => getContent (k,v,L);

fun getNumFunction s (_,_,[]) = raise NoAttribute s
  | getNumFunction s (k,v,(a::L)) =
    (case Attribute.getNumFunction a of (s',n) =>
        (if s' = s then (s',n) else getNumFunction s (k,v,L))) handle Match => getNumFunction s (k,v,L);

fun getStringFunction s (_,_,[]) = raise NoAttribute s
  | getStringFunction s (k,v,(a::L)) =
    (case Attribute.getStringFunction a of (s',n) =>
        (if s' = s then (s',n) else getStringFunction s (k,v,L))) handle Match => getStringFunction s (k,v,L);

fun getFeatures (_,_,[]) = []
  | getFeatures (k,v,(a::L)) = Attribute.getFeature a :: getFeatures (k,v,L) handle Match => getFeatures (k,v,L);

fun updateNumFunction s f (k,v,L) = (k,v,map (Attribute.updateNumFunction s f) L);


fun compareKindValuePair ((k,v),(k',v')) =
    let val c = Kind.compare (k,k')
    in if c = EQUAL
       then case (v,v') of (Boolean b, Boolean b') => if b then if b' then EQUAL else GREATER else if b' then LESS else EQUAL
                         | (Boolean _, _) => LESS
                         | (_,Boolean _) => GREATER
                         | (Label s, Label s') => String.compare (s,s')
                         | (Label _, _) => LESS
                         | (_, Label _) => GREATER
                         | (Type t, Type t') => Type.compare (t,t')
                         | (Type _, _) => LESS
                         | (_, Type _) => GREATER
                         | (Number n, Number n') => Int.compare (n,n')
                         | (Number _, _) => LESS
                         | (_, Number _) => GREATER
                         | (Raw s, Raw s') => String.compare (s,s')
       else c
    end;

(*A lexicographic order for the property type. The name (kind) takes precedence,
then the value, and then its type.
  Useful for putting it into a dictionary; not for much else*)
fun compare ((k,v,ats),(k',v',ats')) =
    let
        val c = compareKindValuePair ((k,v),(k',v'))
        val xt = typeFromAttributes ats
        val xt' = typeFromAttributes ats'
    in
        if c = EQUAL
        then case (xt,xt') of
                (NONE, NONE) => EQUAL
              | (NONE, SOME _) => LESS
              | (SOME t, SOME t') => Type.compare (t,t')
              | (SOME _, NONE) => GREATER
        else c
    end;

fun kindValuePairMatch (k,v) (k',v') =
    (k = k') andalso (case (v,v') of (Type t, Type t') => Type.match (t,t') | _ => v = v')

(*match is meant to be used for finding whether a correspondence holds
  without the need to have type or attribute information, and type-matching when
  there is type information *)
fun match (p,p') =
    let val M = (Type.match (getTypeOfValue p, getTypeOfValue p') handle Match => true)
    in M andalso (kindValuePairMatch (toKindValuePair p) (toKindValuePair p'))
    end


fun toString (k,v,ats) =
    let val sv = stringOfValue v
        val sats = case ats of [] => "" | aL => " : {" ^ (String.concatWith "; " (map Attribute.toString aL)) ^ "}";
    in (Kind.toString k) ^ "-" ^ sv ^ sats
    end

fun breakUntilCharacter _ [] = ([],[])
  | breakUntilCharacter c (h::t) =
      if h = c then ([],t)
      else let val (x,y) = breakUntilCharacter c t
           in (h::x,y)
           end;

fun readAttributes s =
    let val s' = (Parser.removeBraces o Parser.stripSpaces) s
        val Astrs = Parser.splitStrip ";" s'
        val A = map Attribute.fromString Astrs
    in  A
    end

fun findAttributes s =
    let
      val (v,_,As) = Parser.breakOn ":" s;
      val A = readAttributes As (* takes {sdf; fsdfd:=sdf ; fdsdf:=[type1 =>2. type2 => 1] ;fte} and returns a list of attributes*)
    in (Parser.stripSpaces v, A)
    end

fun fromKindValueAttributes (k,v,A) = (k,v,A)
fun toKindValueAttributes (k,v,A) = (k,v,A)

(* This function is used only by correspondence-related functions,
   which shouldn't care about attributes other than type. Thus, the
   only attribute carried on is the type *)
fun fromString s =
    let val (ks,_,vs) = Parser.breakOn "-" s
        val (v,A) = findAttributes vs
        val k = Kind.fromString ks
    in
        if vs = "" then (k, Boolean true, A)
        else if k = Kind.Type then (k, Type (Type.fromString v), A)
        else (k, Label v, A)
    end;

end;

signature QPROPERTY =
sig
    type property;

    exception ParseError;

    val compare : property * property -> order;
    val toString : property -> string;
    val fromString : string -> property;
    val toPair : property -> (Property.property * Importance.importance);
    val fromPair : (Property.property * Importance.importance) -> property;
    val withoutImportance : property -> Property.property;
    val kindOf : property -> Kind.kind;
    val importanceOf : property -> Importance.importance;
end;


structure QProperty :> QPROPERTY =
struct

type property = (Property.property * Importance.importance);
exception ParseError;

val compare = Comparison.join Property.compare Importance.compare;
fun toString (p, i) = "(" ^ (Property.toString p) ^ ", " ^ (Importance.toString i) ^ ")";
fun fromString s =
    case Parser.splitStrip "," (Parser.removeParens s) of
        [a, b] =>(case Importance.fromString b of
                      SOME b' => (Property.fromString a, b')
                    | NONE => raise ParseError)
      | _ => raise ParseError;
fun toPair (s, i) = (s, i);
fun fromPair (s, i) = (s, i);
fun withoutImportance (s, _) = s;
fun kindOf (s, _) = Property.kindOf s;
fun importanceOf (_, i) = i;

end;

structure PropertySet =
struct
structure S = Set(struct
                type t = Property.property;
                val compare = Property.compare;
                val fmt = Property.toString;
                end);
open S;

fun filterMatches p ps = filter (fn v => Property.match (p,v)) ps;

fun isMatchedIn p ps = not (isEmpty (filterMatches p ps));

(* finds matches between two property sets,
   but only returns the matches of the left (ps) *)
fun collectLeftMatches ps ps' =
    let
        val x = map (fn p => filterMatches p ps) ps';
    in
        unionAll x
    end;

fun collectOfKind ps k =
    let fun isOfKind p = Property.kindOf p = k;
    in filter isOfKind ps
    end;


end;


structure PropertyDictionary = Dictionary(struct
                                           type k = Property.property;
                                           val compare = Property.compare;
                                           val fmt = Property.toString;
                                           end);
structure QPropertySet =
struct
structure QS = Set(struct
                    type t = QProperty.property;
                    val compare = QProperty.compare;
                    val fmt = QProperty.toString;
                    end);
open QS;

fun withoutImportances ps =
    PropertySet.fromList (map (QProperty.withoutImportance) ps)

fun collectOfKind ps k =
    let
        fun isOfKind p = (QProperty.kindOf p) = k;
    in filter isOfKind ps
    end;

fun collectOfImportance ps i =
    let fun isOfImportance p = (#2 (QProperty.toPair p) = i);
    in filter isOfImportance ps
    end;

fun filterMatches p qs =
    let
        fun qmatch v = Property.match (p, QProperty.withoutImportance v);
    in
        filter qmatch qs
    end;

fun isMatchedIn p qs = not (isEmpty (filterMatches p qs));

end;
