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

    datatype value = Label of string
                   | Number of int
                   | Boolean of bool
                   | Type of Type.T
                   | Raw of string;
    type t;

    structure M : MULTISET;
    val toListHandlingNegatives : (int -> int) -> Type.T M.multiset -> Type.T list;
    val toPairList : Type.T M.multiset -> (Type.T * int) list;
    val HolesFromList : Type.T list -> Type.T M.multiset
    val countUnique : Type.T M.multiset -> int;
    val size : Type.T M.multiset -> int;
    val contains : Type.T M.multiset -> Type.T -> bool;
    exception NegativeCount of Type.T * int;

    exception Error of string;
    exception NoAttribute of string;

    val kindOf : t -> Kind.kind;
    val valueOf : t -> value;

    val LabelOf : t -> string;
    val NumberOf : t -> int;
    val BooleanOf : t -> bool;
    val TypeOf : t -> Type.T;

    val updateAttribute : Attribute.T -> t -> t;

    val attributesOf : t -> Attribute.T list;
    val getTypeOfValue : t -> Type.T;
    val getHoles : t -> Type.T M.multiset;
    val getTokens : t -> string list;
    val getContent : t -> Type.T;
    val getStringFunction : string -> t -> (string * string);
    val getNumFunction : string -> t -> (string * real);
    val getFeatures : t -> string list;

    val sameHoles : (t * t) -> bool;
    val sameTokens : (t * t) -> bool;

    val updateNumFunction : string -> (real -> real) -> t -> t;

    val compare : t * t -> order;
    val match : t * t -> bool;

    val toString : t -> string;
    val fromString : string -> t;
    val fromKindValueAttributes : Kind.kind * value * (Attribute.T list) -> t;
    val toKindValueAttributes : t -> Kind.kind * value * (Attribute.T list);
    val findAttributes : string -> (string * Attribute.T list);
end;

structure Property :> PROPERTY =
struct

datatype value = Label of string
               | Number of int
               | Boolean of bool
               | Type of Type.T
               | Raw of string;

fun stringOfValue (Label s) = s
  | stringOfValue (Number n) = Int.toString n
  | stringOfValue (Boolean b) = if b then "TRUE" else "FALSE"
  | stringOfValue (Type t) = Type.toString t
  | stringOfValue (Raw s) = "RAW: " ^ s;

type t = (Kind.kind * value * Attribute.T list);

structure M = Attribute.M

fun toListHandlingNegatives f m = M.toListHandlingNegatives f m;
fun toPairList m = M.toPairList m;
fun HolesFromList l = M.fromList l;
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

fun updateAttribute a' (k,s,a::A) =
    if Attribute.sameSort (a,a') then (k,s,a'::A)
    else (case updateAttribute a' (k,s,A) of (_,_,A') => (k,s,a::A'))
  | updateAttribute a' (k,s,[]) = (k,s,[a'])

fun attributesOf (_,_,A) = A;

fun typeFromAttributes [] = NONE
  | typeFromAttributes (a::L) = SOME (Attribute.getType a) handle Match => typeFromAttributes L;

exception NoAttribute of string;

fun getTypeOfValue (k, Label v,A) =
    (case typeFromAttributes A of SOME t => t | NONE => raise NoAttribute "type")
  | getTypeOfValue (k,v,A) = raise NoAttribute "type";

fun getHoles (_,_,[]) = raise NoAttribute "holes"
  | getHoles (k,v,(a::L)) = Attribute.getHoles a handle Match => getHoles (k,v,L);

fun getTokens (k,v,[]) = if k = Kind.Token then (case v of Label s => [s] | _ => raise Error "IMPOSSIBLE ERROR")
                    else raise NoAttribute "tokens"
  | getTokens (k,v,(a::L)) = Attribute.getTokens a handle Match => getTokens (k,v,L);

fun getContent (_,_,[]) = raise NoAttribute "content"
  | getContent (k,v,(a::L)) = Attribute.getContent a handle Match => getContent (k,v,L);

fun getNumFunction s (k, v,[]) = raise NoAttribute s
  | getNumFunction s (k,v,(a::L)) =
    (case Attribute.getNumFunction a of (s',n) =>
        (if s' = s then (s',n) else getNumFunction s (k,v,L))) handle Match => getNumFunction s (k,v,L) ;

fun getStringFunction s (_,_,[]) = raise NoAttribute s
  | getStringFunction s (k,v,(a::L)) =
    (case Attribute.getStringFunction a of (s',n) =>
        (if s' = s then (s',n) else getStringFunction s (k,v,L))) handle Match => getStringFunction s (k,v,L);

fun getFeatures (_,_,[]) = []
  | getFeatures (k,v,(a::L)) = Attribute.getFeature a :: getFeatures (k,v,L) handle Match => getFeatures (k,v,L);

fun updateNumFunction s f (k,v,L) = (k,v,map (Attribute.updateNumFunction s f) L);

fun sameHoles (p,p') = M.equal (getHoles p, getHoles p')
fun sameTokens (p,p') = List.isPermutationOf op=
                                             (getTokens p handle NoAttribute _ => [])
                                             (getTokens p' handle NoAttribute _ => [])

fun compareKindValuePair ((k,v),(k',v')) =
    let val c = Kind.compare (k,k')
    in if c = EQUAL
       then case (v,v') of (Boolean b, Boolean b') => if b then
                                                          if b' then EQUAL else GREATER
                                                      else
                                                          if b' then LESS else EQUAL
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
    let val M = (Type.match (getTypeOfValue p, getTypeOfValue p') handle NoAttribute _ => true)
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
    type t;

    exception ParseError;

    val compare : t * t -> order;
    val toString : t -> string;
    val fromString : string -> t;
    val toPair : t -> (Property.t * Importance.t);
    val fromPair : (Property.t * Importance.t) -> t;
    val withoutImportance : t -> Property.t;
    val kindOf : t -> Kind.kind;
    val importanceOf : t -> Importance.t;

    val gravity : t -> real;
    val logGravity : t -> real;
end;


structure QProperty :> QPROPERTY =
struct

type t = (Property.t * Importance.t);
exception ParseError;

val compare = Comparison.join Property.compare Importance.compare;
fun toString (p, i) = "(" ^ (Property.toString p) ^ ", " ^ (Importance.toString i) ^ ")";
fun fromString s =
    case Parser.splitStrip "," (Parser.removeParens s) of
        [a, b] => (case Importance.fromString b of b' => (Property.fromString a, b'))
      | _ => raise ParseError;
fun toPair (s, i) = (s, i);
fun fromPair (s, i) = (s, i);
fun withoutImportance (s, _) = s;
fun kindOf (s, _) = Property.kindOf s;
fun importanceOf (_, i) = i;

(* gravity gives weight 0 to things with 0 occurrences, but grows linearly with occurrences *)
fun gravity x = (Importance.weight (importanceOf x))
                * #2 (Property.getNumFunction "occurrences" (withoutImportance x))

(* logGravity cares less about occurrences, but in fact gives positive weight to things with 0 occurrences. *)
fun logGravity x = (Importance.weight (importanceOf x))
                    * (Math.ln(2.0 + #2 (Property.getNumFunction "occurrences" (withoutImportance x)))/Math.ln(2.0))

end;

structure PropertySet =
struct
structure S = Set(struct
                type t = Property.t;
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
                                           type k = Property.t;
                                           val compare = Property.compare;
                                           val fmt = Property.toString;
                                           end);
structure QPropertySet =
struct
structure QS = Set(struct
                    type t = QProperty.t;
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
    let fun isOfImportance p = Importance.equal (#2 (QProperty.toPair p), i);
    in filter isOfImportance ps
    end;

fun filterMatches p qs =
    let
        fun qmatch v = Property.match (p, QProperty.withoutImportance v);
    in
        filter qmatch qs
    end;

fun isMatchedIn p qs = not (isEmpty (filterMatches p qs));


fun collectOfKindPresentInQ qS k =
    filter
      (fn x => (#2 (Property.getNumFunction "occurrences" (QProperty.withoutImportance x)) > 0.0 handle Property.NoAttribute _ => false)
               orelse (#2 (Property.getNumFunction "uses" (QProperty.withoutImportance x)) > 0.0 handle Property.NoAttribute _ => false))
      (collectOfKind qS k)

end;
