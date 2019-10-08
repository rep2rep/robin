import "util.stream";
import "util.random";

import "strategies.properties.correspondence";


signature DISCOVERCORRESPONDENCES = sig

    (* Correspondences, Old RSs, New RS *)
    type state = Correspondence.correspondence list *
                 PropertySet.t PropertySet.set list *
                 PropertySet.t PropertySet.set;

    datatype reason = IDENTITY of Property.property
                    | REVERSAL of Correspondence.correspondence
                    | COMPOSITION of Correspondence.correspondence * Correspondence.correspondence
                    (* | KIND of Property.property * Property.property *)
                    | ATTRIBUTE of Property.property * Property.property
                    | VALUE of Property.property * Property.property;

    val reasonString : reason -> string;
    val discover : state -> (Correspondence.correspondence * reason) Stream.stream;

end;


structure DiscoverCorrespondences : DISCOVERCORRESPONDENCES = struct

type state = Correspondence.correspondence list *
             PropertySet.t PropertySet.set list *
             PropertySet.t PropertySet.set;

datatype reason = IDENTITY of Property.property
                | REVERSAL of Correspondence.correspondence
                | COMPOSITION of Correspondence.correspondence * Correspondence.correspondence
                (* | KIND of Property.property * Property.property *)
                | ATTRIBUTE of Property.property * Property.property
                | VALUE of Property.property * Property.property;

fun reasonString (IDENTITY p) =
    "The property " ^ (Property.toString p) ^ " corresponds to itself"
  | reasonString (REVERSAL c) =
    "Reversed from " ^ (Correspondence.toString c)
  | reasonString (COMPOSITION (c1, c2)) =
    "Composed from " ^ (Correspondence.toString c1) ^ " and " ^ (Correspondence.toString c2)
  | reasonString (ATTRIBUTE (p1, p2)) =
    "Found a common attribute between " ^ (Property.toString p1) ^ " and " ^ (Property.toString p2)
  | reasonString (VALUE (p1, p2)) =
    "Is a common attribute between " ^ (Property.toString p1) ^ " and " ^ (Property.toString p2);

fun corrExists c cs = List.exists (Correspondence.matchingProperties c) cs;

fun anyCommonCorrs cs cs' = List.exists (fn c => corrExists c cs') cs;

fun doCorrespond cs (p1, p2) = Correspondence.matchExists (PropertySet.fromList [p1]) (PropertySet.fromList [p2]) cs;

fun makeCorr (p, q) =
    (Correspondence.F.Atom p, Correspondence.F.Atom q, 1.0);

fun atomOnly (Correspondence.F.Atom x, Correspondence.F.Atom y, z) = true
  | atomOnly _ = false;

local
    fun propertyFromType t = Property.fromKindValueAttributes (Kind.Type, Property.Type t, []);
    fun propertyFromToken s = Property.fromKindValueAttributes (Kind.Token, Property.Label s, []);
    fun unifyish (Type.Ground x, Type.Ground y) = [(Type.Ground x, Type.Ground y)]
      | unifyish (Type.Var x, Type.Var y) = []
      | unifyish (Type.Pair (a, b), Type.Pair (x, y)) = (unifyish (a, x)) @ (unifyish (b, y))
      | unifyish (Type.Function (a, b), Type.Function (x, y)) = (unifyish (a, x)) @ (unifyish (b, y))
      | unifyish (Type.Constructor (s, x), Type.Constructor (s', y)) = (Type.Ground s, Type.Ground s')::(unifyish (x, y))
      | unifyish _ = raise Match;
in
fun checkAttrCorr cs (a, b) =
    let (*  Type * Type  *)
        val (at, bt) = spread Attribute.getType (a, b);
        val unified = unifyish (at, bt) handle Match => [];
        val typePairs = map (spread propertyFromType) unified;
        val success = List.all (doCorrespond cs) typePairs;
    in
        success andalso not (List.null unified)
    end handle Match =>
    let (*  Content * Content  *)
        val (ac, bc) = spread Attribute.getContent (a, b);
        val (at, bt) = spread propertyFromType (ac, bc);
    in
        doCorrespond cs (at, bt)
    end handle Match =>
    let (*  (Holes * Type) * (Holes * Type)  *)
        val (ah, bh) = spread (Attribute.M.toPairList o Attribute.getHoles) (a, b);
        val holePairs = List.map (fn ((a, _), (b, _)) => (a, b)) (List.filter (fn ((_, c), (_, c')) => c = c') (List.product ah bh));
        val (at, bt) = spread Attribute.getType (a, b);
        val typePairs = map (spread propertyFromType) ((at, bt) :: (List.flatmap unifyish holePairs));
        val success = List.all (doCorrespond cs) typePairs;
    in
        success andalso not (List.null holePairs)
    end handle Match =>
    let (*  Tokens * Tokens  *)
        val (at, bt) = spread Attribute.getTokens (a, b);
        val tokPairs = List.product at bt;
        val propPairs = map (spread propertyFromToken) tokPairs;
        val success = List.all (doCorrespond cs) propPairs;
    in
        success andalso not (List.null tokPairs)
    end handle Match => false;

fun potentialAttrCorr (a, b) =
    let
        val _ = print ("(" ^ (Property.toString a) ^ ", " ^ (Property.toString b) ^ ")\n");
    in
        []
    end;
end;

fun findMatches cs rs =
    let
        val matchChecker = fn m => not o PropertySet.isEmpty o (m rs);
        val leftMatches = List.filter (matchChecker Correspondence.leftMatches) cs;
        val rightMatches = List.filter (matchChecker Correspondence.rightMatches) cs;
    in (leftMatches, rightMatches) end;

fun chooseNew [] _ = NONE
  | chooseNew options existing =
    let
        val (option, reason) = Random.choose options;
    in
        if corrExists option existing
        then chooseNew
                 (List.filter (not o (fn (opt, r) =>
                                         Correspondence.matchingProperties option opt))
                              options)
                 existing
        else SOME (option, reason)
    end;


(** Discovery rules **)
(*  All of these have type state -> (correspondence * reason) option *)

(* Identity is handled elsewhere in the code, so ignore it here *)
fun discoverIdentity (cs, rss, rs') = NONE;

fun discoverReversal (cs, rss, rs') =
    let
        fun flipCorr (a, b, c) = ((b, a, c), (* c needs to be changed too *)
                                  REVERSAL (a, b, c));
        val (leftMatches, rightMatches) = findMatches cs rs';
        val corrs = map flipCorr (leftMatches @ rightMatches);
    in
        chooseNew corrs cs
    end handle List.Empty => NONE;

fun discoverComposition (cs, rss, rs') =
    let
        fun doCompose ((_, x, _), (y, _, _)) = Correspondence.F.equal
                                                (Property.match)
                                                (x, y);
        fun compose ((x, y, xs), (y', z, zs)) = ((x, z, xs * zs),
                                               COMPOSITION ((x, y, xs), (y', z, zs)));
        val (leftMatches, rightMatches) = findMatches cs rs';
        val corrPairs = (List.product leftMatches cs) @ (List.product cs rightMatches);
        val validCorrPairs = List.filter doCompose corrPairs;
        val corrs = map compose validCorrPairs;
    in
        chooseNew corrs cs
    end handle List.Empty => NONE;

(* fun discoverKind (cs, rss, rs') = NONE; *)

fun discoverAttribute (cs, rss, rs') =
    let
        fun matchAttrs (p, q) =
            let
                val pAttrs = Property.attributesOf p;
                val qAttrs = Property.attributesOf q;
                val corrAttrs = List.filter (checkAttrCorr cs) (List.product pAttrs qAttrs);
            in
                not (List.null corrAttrs)
            end;
        val potentialCorrs = List.flatmap (fn rs => PropertySet.product rs rs') rss;
        val matchingAttrs = List.filter matchAttrs potentialCorrs;
        val corrs = map (fn pq => (makeCorr pq, ATTRIBUTE pq)) matchingAttrs;
    in
        chooseNew corrs cs
    end handle List.Empty => NONE;

fun discoverValue (cs, rss, rs') =
    let
        val potentialMatchingValues = List.flatmap (fn rs => PropertySet.product rs rs') rss;
        val matchingValues = List.filter (doCorrespond cs) potentialMatchingValues;
        val attrOptions = List.flatmap potentialAttrCorr matchingValues;
        val corrs = map (fn pq => (makeCorr pq, VALUE pq)) attrOptions;
    in
        chooseNew corrs cs
    end handle List.Empty => NONE;


(* All together now... *)

fun discover state' =
    let
        val rules' = [discoverIdentity,
                      discoverReversal,
                      discoverComposition,
                      (* discoverKind, *)
                      discoverAttribute,
                      discoverValue];
        fun addCorr c (cs, rss, rs') = case c of
                                           SOME (c', _) => (c'::cs, rss, rs')
                                         | NONE => (cs, rss, rs');
        fun extractCorr (corr, _, _) = corr;
        fun generator (corr, rules, state) =
            let
                val state' = addCorr corr state;
                val newCorr = Option.oneOf rules state';
            in
                case newCorr of
                    (* We shuffle the rules to guarantee some variety *)
                    SOME c => SOME (SOME c, Random.shuffle rules, state')
                  | NONE => NONE
            end;
    in
        Stream.mapPartial extractCorr (Stream.unfold generator (NONE, rules', state'))
    end;

end;
