signature PARSER =
sig

    exception ParseError;

    type (''a, 'b) parser = ''a list -> ('b * ''a list);

    val expect : ''a -> 'b -> (''a, 'b) parser;
    val accept : (''a -> ('b option)) -> (''a, 'b) parser;
    val produce : 'b -> (''a, 'b) parser;
    val either : (unit -> (''a, 'b) parser) -> (unit -> (''a, 'b) parser) -> (''a, 'b) parser;
    val sequence : (''a, 'b) parser -> ('b -> (''a, 'c) parser) -> (''a, 'c) parser;
    val repeat : (unit -> (''a, 'b) parser) -> (''a, 'b list) parser;
    val try : (''a, 'b) parser -> ''a list -> bool;
    val run : (''a, 'b) parser -> ''a list -> 'b;

    val stripSpaces : string -> string;
    val splitOn : string -> string -> string list;
    val splitStrip : string -> string -> string list;
    val breakOn : string -> string -> (string * string * string);
    val removeDelimiters : (string * string) -> string -> string;
    val removeParens : string -> string;
    val removeBraces : string -> string;
    val removeSquareBrackets : string -> string;
    val removeDoubleQuotes : string -> string;
    val removeSingleQuotes : string -> string;

end;

structure Parser : PARSER =
struct

exception ParseError;

type (''a, 'b) parser = (''a list -> ('b * ''a list));

fun run p xs =
    let
        val (b, rest) = p xs;
    in
        if List.null rest then b else raise ParseError
    end;

fun accept f [] = raise ParseError
  | accept f (x::xs) = case (f x) of
                           SOME z => (z, xs)
                         | _ => raise ParseError;

fun expect s b = accept (fn k => if s = k then SOME b else NONE);

fun produce z = fn xs => (z, xs);

fun either p q = fn xs => (p ()) xs
                          handle ParseError => (q ()) xs;

fun sequence p qf = fn xs =>
                       let
                           val (b, xs') = p xs;
                           val q = qf b;
                       in
                           q xs'
                       end;

fun repeat p = fn xs =>
                  let
                      fun runUntil ans q [] = (List.rev ans, [])
                        | runUntil ans q ys =
                          let
                              val (b, ys') = (q ()) ys;
                          in
                              runUntil (b::ans) q ys'
                          end
                          handle ParseError => (List.rev ans, ys);
                  in
                      runUntil [] p xs
                  end;

fun try p xs = ((run p xs); true)
               handle ParseError => false;

fun splitOn sep s =
    if (String.size sep) = 1
    then let
        val char = List.hd (String.explode sep);
        val match = fn c => c = char;
    in
        String.tokens match s
    end
    else let
        val chars = String.explode s;
        val sepChars = String.explode sep;
        fun startsWithSep' [] r = (true, r)
          | startsWithSep' _ [] = (false, [])
          | startsWithSep' (s::sep) (t::rest) =
            if s = t then startsWithSep' sep rest
            else (false, []);
        fun startsWithSep chars = startsWithSep' sepChars chars;
        fun group ans collected [] =
            let
                val finalWord = String.implode(List.rev collected)
            in
                List.rev (finalWord :: ans)
            end
          | group ans collected (t::tokens) =
            let
                val (isSep, rest') = startsWithSep (t::tokens);
                val rest = if isSep then rest' else tokens;
            in
                if isSep
                then group ((String.implode (List.rev collected))::ans) [] rest
                else group ans (t::collected) rest
            end;
    in
        group [] [] (String.explode s)
    end;

fun stripSpaces str =
    let
        val chars = String.explode str;
        val remainingChars = List.rev
                                 (List.dropWhile
                                      Char.isSpace
                                      (List.rev
                                           (List.dropWhile
                                                Char.isSpace
                                                chars)));
    in
        String.implode remainingChars
    end;

fun splitStrip sep s = map stripSpaces (splitOn sep s);

fun breakOn sep s =
    let
        val sepChars = String.explode sep;
        val sChars = String.explode s;
        fun fwJoin cs = String.implode cs;
        fun bwJoin cs = String.implode (List.rev cs);
        fun break front [] after _ = (bwJoin front, sep, fwJoin after)
          | break front (c::cs) [] _ = (s, "", "")
          | break front (x::xs) (c::cs) accum =
            if x = c then break front xs cs (x::accum)
            else break (c::(accum@front)) sepChars cs [];
    in
        break [] sepChars sChars []
    end;

fun removeDelimiters (left, right) s =
    let
        val leftChars = String.explode left;
        val rightChars = List.rev (String.explode right);
        val sChars = String.explode s;
        fun dropMatching [] s _ = SOME (List.rev s)
          | dropMatching x [] dropped = NONE
          | dropMatching (x::xs) (c::cs) dropped =
            if x = c then dropMatching xs cs (c::dropped)
            else NONE;
    in
        case dropMatching leftChars sChars [] of
            NONE => s
          | SOME chars => case dropMatching rightChars chars [] of
                              NONE => s
                            | SOME chars' => String.implode chars'
    end;

val removeParens = removeDelimiters ("(", ")");
val removeBraces = removeDelimiters ("{", "}");
val removeSquareBrackets = removeDelimiters ("[", "]");
val removeDoubleQuotes = removeDelimiters ("\"", "\"");
val removeSingleQuotes = removeDelimiters ("'", "'");

end;

infixr 3 >=>     fun p >=> q = Parser.sequence p q;
infixr 3 >>>     fun p >>> q = Parser.sequence p (fn _ => q);
