import "util.set";
import "util.type";
import "util.dictionary";
import "util.parser";

import "strategies.properties.importance";

signature PROPERTY =
sig
    exception ParseError;

    type kind;
    datatype value = Label of string | Number of int | Boolean of bool;
    type property;
    type attribute;

    val stringOfKind : kind -> string;
    val kindOfString : string -> kind;

    val sameKind : kind * kind -> bool;
    val kindOf : property -> kind;
    val valueOf : property -> value;

    val LabelOf : property -> string;
    val NumberOf : property -> int;
    val BooleanOf : property -> bool;

    val typeOf : property -> Type.T;
    val attributesOf : property -> attribute list;

    val compare : property * property -> order;
    val match : property * property -> bool;

    val toString : property -> string;
    val fromKindValuePair : kind * value -> property;
    val toKindValuePair : property -> kind * value;
    val fromString : string -> property;
end;

structure Property :> PROPERTY =
struct

type kind = string;
datatype value = Label of string | Number of int | Boolean of bool;
type attribute = string;

fun kindOfString s = s;
fun stringOfKind s = s;

fun sameKind (k,k') = (k = k');

fun stringOfValue (Label s) = s
  | stringOfValue (Number n) = Int.toString n
  | stringOfValue (Boolean b) = if b then "TRUE" else "FALSE";

type property = (kind * value * (Type.T option) * attribute list);

exception ParseError;

fun toKindValuePair (k,v,_,_) = (k,v)

fun kindOf (k,_,_,_) = k;
fun valueOf (_,v,_,_) = v;

fun LabelOf p =
    case valueOf p of Label s => s
                    | _ => (print "Not a Label";raise Match);

fun NumberOf p =
    case valueOf p of Number n => n
                    | _ => (print "Not a Number";raise Match);

fun BooleanOf p =
    case valueOf p of Boolean b => b
                    | _ => (print "Not a Boolean";raise Match);

fun typeOf (_,_,x,_) = case x of SOME t => t | NONE => raise Match;

fun attributesOf (_,_,_,a) = a;

fun compareKindValuePair ((k,v),(k',v')) =
    let val c = String.compare (k,k')
    in if c = EQUAL
       then case (v,v') of (Boolean b, Boolean b') => if b then if b' then EQUAL else GREATER else if b' then LESS else EQUAL
                         | (Boolean _, _) => LESS
                         | (_,Boolean _) => GREATER
                         | (Label s, Label s') => String.compare (s,s')
                         | (Label _, _) => LESS
                         | (_, Label _) => GREATER
                         | (Number n, Number n') => Int.compare (n,n')
       else c
    end;

(*A lexicographic order for the property type. The name takes precedence, then
  the KIND (Simple, Typed, Attr), and in the end the type or attribute list.
  Useful for putting it into a dictionary; not for much else*)
fun compare ((k,v,xt,ats),(k',v',xt',ats')) =
    let
        val c = compareKindValuePair ((k,v),(k',v'))
    in
        if c = EQUAL
        then case (xt,xt') of
                (NONE, NONE) =>  List.collate String.compare (ats,ats')
              | (NONE, SOME _) => LESS
              | (SOME t, SOME t') => let val c' = Type.compare (t,t')
                                     in if c' = EQUAL
                                        then List.collate String.compare (ats,ats')
                                        else c'
                                     end
              | (SOME _, NONE) => GREATER
        else c
    end;


(*match is meant to be used for finding whether a correspondence holds
  without the need to have type or attribute information, and type-matching when
  there is type information *)
fun match (p,p') =
    let val M = (Type.match (typeOf p) (typeOf p') handle _ => true)
    in M andalso ((toKindValuePair p) = (toKindValuePair p'))
    end

fun toString (k,v,xt,ats) =
    let val sv = stringOfValue v
        val st = case xt of NONE => "" | SOME t => " : " ^ Type.toString t
        val sats = case ats of [] => "" | aL => " : {" ^ (String.concat (intersperse "; " aL)) ^ "}";
    in k ^ "-" ^ sv ^ st ^ sats
    end


fun breakUntilCharacter _ [] = ([],[])
  | breakUntilCharacter c (h::t) =
      if h = c then ([],t)
      else let val (x,y) = breakUntilCharacter c t
           in (h::x,y)
           end;

fun findAttributes s =
    let fun ff (c::c'::L) = if (c,c') = (#":",#"{") then breakUntilCharacter #"}" L
                           else let val (x,y) = ff (c'::L)
                                in (x,c::y)
                                end
          | ff L = ([],L)
        val (atts',rest) = ff (String.explode s)
        val atts = Parser.splitStrip ";" (String.implode atts')
    in (atts, String.implode rest)
    end;

(* assumes  we're not dealing with a pattern. It's easier this way *)
fun findType s =
    let val (_,s') = findAttributes s;
        val (v,_,typ') = Parser.breakOn ":" s';
        val typ = case typ' of "" => NONE | ts => SOME (Type.fromString ts)
    in (typ, v)
    end;

fun fromKindValuePair (k,vRaw) =
    let
        val (atts,rest) = findAttributes (stringOfValue vRaw);
        val (xt,v) = if k = "pattern" then (NONE,rest) else findType rest;
    in (k,Label v,xt,atts)
    end;

fun breakStringUntil c s =
  let val (x,y) = breakUntilCharacter c (String.explode s)
  in (String.implode x, String.implode y)
  end;

fun fromString s =
    let val (ks,_,vs) = Parser.breakOn "-" s;
    in
        if vs = ""
        then fromKindValuePair (kindOfString s, Boolean true)
        else fromKindValuePair (kindOfString ks, Label vs)
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
end;


structure QProperty :> QPROPERTY =
struct

type property = (Property.property * Importance.importance);
exception ParseError;

val compare = cmpJoin Property.compare Importance.compare;
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
    let fun isOfKind p = Property.sameKind (Property.kindOf p, Property.kindOfString k);
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

fun collectOfKind ps k =
    let fun isOfKind p = Property.sameKind
                            (Property.kindOf (#1 (QProperty.toPair p)),
                             Property.kindOfString k);
    in filter isOfKind ps
    end;

fun collectOfImportance ps i =
    let fun isOfImportance p = (#2 (QProperty.toPair p) = i);
    in filter isOfImportance ps
    end;

end;
