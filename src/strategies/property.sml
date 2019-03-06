import "util.set";
import "util.dictionary";

import "strategies.property_importance";

signature PROPERTY =
sig
    type property;

    exception ParseError;

    val compare : property * property -> order;
    val toString : property -> string;
    val fromString : string -> property;
end;

structure Property :> PROPERTY =
struct

type property = string;
exception ParseError;

val compare = String.compare;
fun toString s = s;
fun fromString s = s;

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
                                           end);
structure QPropertySet = Set(struct
                              type t = QProperty.property;
                              val compare = QProperty.compare;
                              val fmt = QProperty.toString;
                              end);
