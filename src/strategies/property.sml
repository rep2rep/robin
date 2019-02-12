import "util.set";
import "util.dictionary";

import "strategies.property_importance";

signature PROPERTY =
sig
    eqtype property;

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

structure QProperty :> PROPERTY =
struct
type property = (string * Importance.importance);
exception ParseError;
val compare = cmpJoin String.compare Importance.compare;
fun toString (p, i) = "(" ^ p ^ ", " ^ (Importance.toString i) ^ ")";
fun fromString s =
    let
        val splitter = fn c => c = #"(" orelse  c = #")" orelse c = #",";
    in
        case String.tokens splitter s of
            [a, b] => (case Importance.fromString b of
                           SOME b' => (a, b')
                         | NONE => raise ParseError)
          | _ => raise ParseError
    end;
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
