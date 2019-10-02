import "util.stream";

import "strategies.properties.correspondence";


signature DISCOVERCORRESPONDENCES = sig

    (* Correspondences, Old RSs, New RS *)
    type state = Correspondence.correspondence list *
                 PropertySet.t PropertySet.set list *
                 PropertySet.t PropertySet.set;

    val discover : state -> Correspondence.correspondence Stream.stream;

end;


structure DiscoverCorrespondences : DISCOVERCORRESPONDENCES = struct

type state = Correspondence.correspondence list *
             PropertySet.t PropertySet.set list *
             PropertySet.t PropertySet.set;

fun corrExists c cs = List.exists (fn c' => Correspondence.matchingProperties c c') cs;

fun makeCorr (p, q) =
    (Correspondence.F.Atom p, Correspondence.F.Atom q, 1.0);

fun atomOnly (Correspondence.F.Atom x, Correspondence.F.Atom y, z) = true
  | atomOnly _ = false;

fun findMatches cs rs =
    let
        val matchChecker = fn m => not o PropertySet.isEmpty o (m rs);
        val leftMatches = List.filter (matchChecker Correspondence.leftMatches) cs;
        val rightMatches = List.filter (matchChecker Correspondence.rightMatches) cs;
    in (leftMatches, rightMatches) end;


(** Discovery rules **)
(*  All of these have type state -> correspondence option *)

(* Identity is handled elsewhere in the code, so ignore it here *)
fun discoverIdentity (cs, rss, rs') = NONE;

fun discoverReversal (cs, rss, rs') =
    let
        fun flipCorr (a, b, c) = (b, a, c);
        val (leftMatches, rightMatches) = findMatches cs rs';
        (* val corrs = List.filter atomOnly (leftMatches @ rightMatches); *)
        (* val corr = Random.choose corrs; *)
        val corr = Random.choose (leftMatches @ rightMatches);
    in
        SOME (flipCorr corr)
    end handle List.Empty => NONE;

fun discoverComposition (cs, rss, rs') =
    let
        fun doCompose ((_, x, _), (y, _, _)) = Correspondence.F.equal
                                                (Property.match)
                                                (x, y);
        val (leftMatches, rightMatches) = findMatches cs rs';
        val corrPairs = (List.product leftMatches cs) @ (List.product cs rightMatches);
        val corrs = List.filter doCompose corrPairs;
        val ((x, _, xs), (_, z, zs)) = Random.choose corrs;
    in
        SOME (x, z, xs * zs)
    end handle List.Empty => NONE;

fun discoverAttribute (cs, rss, rs') = NONE;

fun discoverValue (cs, rss, rs') = NONE;


(* All together now... *)

fun discover state' =
    let
        val rules' = [discoverIdentity,
                      discoverReversal,
                      discoverComposition,
                      discoverAttribute,
                      discoverValue];
        fun addCorr c (cs, rss, rs') = case c of
                                           (* TODO: no duplicates *)
                                           SOME c' => (c'::cs, rss, rs')
                                         | NONE => (cs, rss, rs');
        fun extractCorr (corr, _, _) = corr;
        fun generator (corr, rules, state) =
            let
                val newCorr = Option.oneOf rules state;
            in
                case newCorr of
                    (* We shuffle the rules to guarantee some variety *)
                    SOME c => SOME (SOME c, Random.shuffle rules, addCorr corr state)
                  | NONE => NONE
            end;
    in
        Stream.mapPartial extractCorr (Stream.unfold generator (NONE, rules', state'))
    end;

end;
