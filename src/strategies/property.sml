import "util.set";
import "util.type";
import "util.dictionary";

import "strategies.property_importance";

signature PROPERTY =
sig
    type property;

    exception ParseError;

    val compare : property * property -> order;
    val propertyMatch : property * property -> bool;
    val nameOf : property -> string;
    val toString : property -> string;
    val fromString : string -> property;
end;

structure Property :> PROPERTY =
struct

datatype property = Simple of string
                  | Typed of (string * Type.T)
                  | Attr of (string * string list);

exception ParseError;

fun nameOf (Simple s) = s
  | nameOf (Typed (s,_)) = s
  | nameOf (Attr (s,_)) = s;

(*A lexicographic order for the property type. The name takes precedence, then
  the KIND (Simple, Typed, Attr), and in the end the type or attribute list.
  Useful for putting it into a dictionary; not for much else*)
fun compare (Simple s, Simple s') = String.compare (s, s')
  | compare (Typed (s,t), Typed (s',t')) = let val c = String.compare (s, s')
                                           in if c = EQUAL
                                              then Type.compare (t,t')
                                              else c
                                           end
  | compare (Attr (s,a), Attr (s',a')) = let val c = String.compare (s, s')
                                           in if c = EQUAL
                                              then List.collate String.compare (a,a')
                                              else c
                                           end
  | compare (p,p') = let val c = String.compare (nameOf p, nameOf p')
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
fun propertyMatch (Simple s, p) = (s = nameOf p)
  | propertyMatch (p, Simple s) = (s = nameOf p)
  | propertyMatch (Typed (s,t), Typed (s',t')) = (s = s' andalso Type.unify t t')
  | propertyMatch (Attr (s,_), Attr (s',_)) = (s = s')
  | propertyMatch _ = false

fun toString (Simple s) = s
  | toString (Typed (s,t)) = s ^ " : " ^ (Type.typeToString t)
  | toString (Attr (s,a)) = s ^ " : {" ^ (String.concat (intersperse ", " a)) ^ "}";

(*as-is, fromString is an ugly function. Very ad-hoc.*)
fun fromString x =
  if String.isPrefix "pattern-" x then Simple x else
      case map stringTrim (String.tokens (fn c => c = #":") x) of
         [r,s] =>
            if String.substring (s,0,1) = "{" then
                if s = "{}" then Attr (r,[])
                else let
                          fun dropEnds [] = []
                            | dropEnds [x] = []
                            | dropEnds [x, y] = []
                            | dropEnds (x::xs) = List.rev (List.tl (List.rev xs));
                          val s' = String.implode (dropEnds (String.explode s));
                      in
                        Attr (r,map stringTrim (String.tokens (fn c => c = #";") s'))
                      end
            else Typed (r, Type.vartype s)
       | [r] => Simple r
       | _ => raise Match;

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

structure PropertySet = Set(struct
                             type t = Property.property;
                             val compare = Property.compare;
                             val fmt = Property.toString;
                             end);
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
