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

    val splitStringOn : string -> string -> string list;

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

fun splitStringOn sep s =
    let
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

end;

infixr 3 >=>     fun p >=> q = Parser.sequence p q;
infixr 3 >>>     fun p >>> q = Parser.sequence p (fn _ => q);
