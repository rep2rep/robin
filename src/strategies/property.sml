import "util.set";
import "util.type";
import "util.dictionary";

import "strategies.property_importance";

signature PROPERTY =
sig
    exception ParseError;

    type kind;
    datatype value = Label of string | Number of int | Boolean of bool;
    type property;

    val stringOfKind : kind -> string;
    val kindOfString : string -> kind;

    val kindOf : property -> kind;
    val valueOf : property -> value;
    val typeOf : property -> Type.T;
    val attributesOf : property -> string list;

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

fun kindOfString s = s;
fun stringOfKind s = s;

fun stringOfValue (Label s) = s
  | stringOfValue (Number n) = Int.toString n
  | stringOfValue (Boolean b) = if b then "TRUE" else "FALSE";

datatype property = Simple of (kind * value)
                  | Typed of ((kind * value) * Type.T)
                  | Attr of ((kind * value) * string list);

exception ParseError;

fun toKindValuePair (Simple kv) = kv
  | toKindValuePair (Typed (kv,_)) = kv
  | toKindValuePair (Attr (kv,_)) = kv;

fun kindOf p = #1 (toKindValuePair p);
fun valueOf p = #2 (toKindValuePair p);

fun typeOf (Typed (_,t)) = t
  | typeOf _ = raise Match;

fun attributesOf (Attr (_,a)) = a
  | attributesOf _ = raise Match;

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
fun compare (Simple p, Simple p') = compareKindValuePair (p, p')
  | compare (Typed (p,t), Typed (p',t')) = let val c = compareKindValuePair (p, p')
                                           in if c = EQUAL
                                              then Type.compare (t,t')
                                              else c
                                           end
  | compare (Attr (p,a), Attr (p',a')) = let val c = compareKindValuePair (p, p')
                                           in if c = EQUAL
                                              then List.collate String.compare (a,a')
                                              else c
                                           end
  | compare (p,p') = let val c = compareKindValuePair (toKindValuePair p, toKindValuePair p')
                     in if c = EQUAL
                        then case (p,p') of (Simple _, _) => LESS
                                          | (_, Simple _) => GREATER
                                          | (Typed _, _) => LESS
                                          | (_, Typed _) => GREATER
                                          | _ => raise Match
                        else c
                     end  ;

(*propertyMatch is meant to be used for finding whether a correspondence holds
  without the need to have type or attribute information, and type-matching when
  there is type information *)
fun match (Simple kv, p) = (kv = toKindValuePair p)
  | match (p, Simple kv) = (kv = toKindValuePair p)
  | match (Typed (kv,t), Typed (kv',t')) = (kv = kv' andalso Type.match t t')
  | match (Attr (kv,_), Attr (kv',_)) = (kv = kv')
  | match _ = false

(*
fun toString (Simple (k,v)) = k ^ "[" ^ stringOfValue v ^ "]"
  | toString (Typed ((k,v),t)) = k ^ "[" ^ stringOfValue v  ^ " : " ^ (Type.toString t) ^ "]"
  | toString (Attr ((k,v),a)) = k ^ "[" ^ stringOfValue v  ^ " : {" ^ (String.concat (intersperse "; " a)) ^ "}"  ^ "]";
  *)
fun toString (Simple (k,v)) = k ^ "-" ^ stringOfValue v
  | toString (Typed ((k,v),t)) = k ^ "-" ^ stringOfValue v  ^ " : " ^ (Type.toString t)
  | toString (Attr ((k,v),a)) = k ^ "-" ^ stringOfValue v  ^ " : {" ^ (String.concat (intersperse "; " a)) ^ "}" ;

fun fromKindValuePair (k,vRaw) =
    if stringOfKind k = "pattern" then Simple (k,vRaw) else
      let val sv = stringOfValue vRaw
      in
        case map stringTrim (String.tokens (fn c => c = #":") sv) of
           [v,s] =>
              if String.substring (s,0,1) = "{" then
                  if s = "{}" then Attr ((k,Label v),[])
                  else let
                            fun dropEnds [] = []
                              | dropEnds [x] = []
                              | dropEnds [x, y] = []
                              | dropEnds (x::xs) = List.rev (List.tl (List.rev xs));
                            val s' = String.implode (dropEnds (String.explode s));
                        in
                          Attr ((k,Label v),map stringTrim (String.tokens (fn c => c = #";") s'))
                        end
              else Typed ((k,Label v), Type.fromString s)
         | [_] => Simple (k,vRaw)
         | _ => raise Match
      end;

fun breakStringUntil c s =
  let
      fun breakUntilChar c [] a = (rev a,[])
        | breakUntilChar c (h::t) a = if h = c then (rev a,t) else breakUntilChar c t (h::a);
      val (x,y) = (breakUntilChar c (String.explode s) [])
  in
    (String.implode x, String.implode y)
  end;

fun fromString s =
  let val (ks,vs) = breakStringUntil #"-" s
  in if vs = "" then fromKindValuePair (kindOfString s, Boolean true)
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
    let
        val splitter = fn c => c = #"(" orelse  c = #")" orelse c = #",";
    in
        case String.tokens splitter s of
            [a, b] => (case Importance.fromString b of
                           SOME b' => (Property.fromString a, b')
                         | NONE => raise ParseError)
          | _ => raise ParseError
    end;
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


end;


structure PropertyDictionary = Dictionary(struct
                                           type k = Property.property;
                                           val compare = Property.compare;
                                           val fmt = Property.toString;
                                           end);
structure QPropertySet = Set(struct
                              type t = QProperty.property;
                              val compare = QProperty.compare;
                              val fmt = QProperty.toString;
                              end);
