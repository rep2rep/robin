(*
algorithms.sml

This structure provides some simple algorithms:
back-tracking, greedy-search, simulated-annealing
*)

signature ALGORITHMS =
sig

    (* backtrack : isDone -> neighbours -> start -> allSolutions *)
    val backtrack : ('a -> bool) -> ('a -> 'a list) -> 'a -> 'a list;

    (* greedy : isDone -> neighbours -> score -> start -> solution *)
    val greedy : ('a -> bool) -> ('a -> 'a list) -> ('a -> real) -> 'a -> 'a;

    (* anneal : temperature -> jump -> energy
                -> transitionProbability -> iterations -> start -> solution *)
    val anneal : (real -> real) -> ('a -> 'a) -> ('a -> real)
                 -> (real * real * real -> real) -> int -> 'a -> 'a;

    (* gradientDescent : neighbours -> altitude -> start -> solution *)
    val gradientDescent : ('a -> 'a list) -> ('a -> real) -> 'a -> 'a;

    (* graphSearch : save -> get -> empty -> isDone -> neighbours -> start -> end *)
    val graphSearch : ('a * 'f -> 'f) -> ('f -> 'a * 'f) -> 'f
                      -> ('a -> bool) -> ('a -> 'a list) -> 'a -> 'a;

end;

structure Algorithms : ALGORITHMS =
struct

fun backtrack done next start =
    let fun loop state =
            if done state then [state]
            else List.flatmap loop (next state);
    in loop start end;

fun greedy done next score start =
    let fun loop state =
            if done state then state
            else loop (#1 (List.argmax score (next state)))
                 handle List.Empty => state;
    in loop start end;

fun anneal temperature jump energy transition iterations start =
    let fun loop k state =
            if k = iterations then state
            else let val state' = jump state;
                     val progression = (real iterations) / (real k);
                     val temp = temperature progression;
                     val trnsProb = let val e1 = energy state;
                                        val e2 = energy state';
                                    in if e2 < e1 then 1.0
                                       else transition (e1, e2, temp) end;
                     val trnsThrshld = 0.5; (* Make truly random *)
                 in if trnsProb > trnsThrshld
                    then loop (k+1) state'
                    else loop (k+1) state end;
    in loop 1 start end;

fun gradientDescent next score start =
    let fun loop (state, alt) =
            let val neighbours = next state;
                val steepest = List.argmin (fn n => (score n - alt)) neighbours
                               handle List.Empty => (state, 0.0);
            in if #2 steepest >= 0.0
               then state
               else loop (mapsnd (fn s => s + alt) steepest) end;
    in loop (start, score start) end;

fun graphSearch save get empty done next start =
    let fun loop frontier =
            let val (state, frontier') = get frontier;
            in if (done state)
               then state
               else loop (List.foldr save frontier' (next state)) end;
    in loop (save (start, empty)) end;

end;
