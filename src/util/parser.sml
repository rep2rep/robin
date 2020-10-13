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
end;

infixr 3 >=>     fun p >=> q = Parser.sequence p q;
infixr 3 >>>     fun p >>> q = Parser.sequence p (fn _ => q);
