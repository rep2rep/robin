import "util.stream";
import "util.random";

import "strategies.correspondences.correspondence";


signature DISCOVERCORRESPONDENCES = sig

    (* Correspondences, Old RSs, New RS *)
    type state = Correspondence.t list *
                 PropertySet.t PropertySet.set list *
                 PropertySet.t PropertySet.set;

    datatype reason = IDENTITY of Property.t
                    | REVERSAL of Correspondence.t
                    | COMPOSITION of Correspondence.t * Correspondence.t
                    (* | KIND of Property.t * Property.t *)
                    | ATTRIBUTE of Property.t * Property.t
                    | VALUE of Property.t * Property.t;

    val reasonString : reason -> string;
    val discover : state -> (Correspondence.t * reason) Stream.stream;

end;


structure DiscoverCorrespondences : DISCOVERCORRESPONDENCES = struct

type state = Correspondence.t list *
             PropertySet.t PropertySet.set list *
             PropertySet.t PropertySet.set;

datatype reason = IDENTITY of Property.t
                | REVERSAL of Correspondence.t
                | COMPOSITION of Correspondence.t * Correspondence.t
                (* | KIND of Property.t * Property.t *)
                | ATTRIBUTE of Property.t * Property.t
                | VALUE of Property.t * Property.t;

fun reasonString (IDENTITY p) =
    "The property " ^ (Property.toString p) ^ " corresponds to itself"
  | reasonString (REVERSAL c) =
    "Reversed from " ^ (Correspondence.toString c)
  | reasonString (COMPOSITION (c1, c2)) =
    "Composed from " ^ (Correspondence.toString c1) ^ " and " ^ (Correspondence.toString c2)
  | reasonString (ATTRIBUTE (p1, p2)) =
    "Found a common attribute between " ^ (Property.toString p1) ^ " and " ^ (Property.toString p2)
  | reasonString (VALUE (p1, p2)) =
    "This is potentially a common attribute between " ^ (Property.toString p1) ^ " and " ^ (Property.toString p2) ^ ", which correspond";

local fun covered c c' =
          (Correspondence.sameProperties c c' (* andalso ((Correspondence.strength c) <= (Correspondence.strength c')) *));
          (* orelse (Correspondence.implies c c'); (* A more general rule already exists *) *)
in fun corrExists c cs = List.exists (covered c) cs end;

fun anyCommonCorrs cs cs' = List.exists (fn c => corrExists c cs') cs;

fun doCorrespond cs (p1, p2) =
    let fun findMatch qs rs = case List.filter (Correspondence.match qs rs) cs of
                                  [] => NONE
                                | x => SOME x;
    in findMatch (PropertySet.fromList [p1]) (PropertySet.fromList [p2]) end;

fun makeCorr' s (p, q) =
    (Correspondence.F.Atom p, Correspondence.F.Atom q, s);

fun makeCorr (p, q) = makeCorr' 1.0 (p, q);

fun atomOnly (Correspondence.F.Atom x, Correspondence.F.Atom y, z) = true
  | atomOnly _ = false;

fun corrToPairs (p, q, v) =
    let
        fun formulaToValue (Correspondence.F.Atom a, Correspondence.F.Atom b) = SOME ((a, b), v)
          | formulaToValue _ = NONE
        val (pClauses, qClauses) = mappair (Stream.fromList o Correspondence.F.clauses) (p, q);
        val basePairs = Stream.product pClauses qClauses;
    in
        Stream.mapPartial formulaToValue basePairs
    end;

fun streamSetProduct ps qs = (uncurry Stream.product) (mappair (Stream.fromList o PropertySet.toList) (ps, qs));

local
    fun getAttr' f [] = raise Match
      | getAttr' f (a::attrs) = f a handle Match => getAttr' f attrs;
    fun getAttr f = (getAttr' f) o Property.attributesOf;
    fun propertyFromType t = Property.fromKindValueAttributes (Kind.Type, Property.Type t, []);
    fun propertyFromToken s = Property.fromKindValueAttributes (Kind.Token, Property.Label s, []);
    fun unifyish (f, g) = let val (genf, fsubs) = Type.generalise f;
                              val (geng, gsubs) = Type.generalise g;
                              fun simplify [] = []
                                | simplify ((x, y)::xs) = if List.exists (fn (a, b) => a = x orelse b = y orelse a = y orelse b = x) xs
                                                          then raise Type.TUNIFY else (x, y)::(simplify xs);
                              fun lookup v [] = NONE
                                | lookup v ((x,y)::xs) = if v = y then SOME (Type.Ground x) else lookup v xs;
                              fun align [] = []
                                | align ((Type.Var x, Type.Var y)::us) = let val x' = lookup x fsubs;
                                                                             val y' = lookup y gsubs;
                                                                         in case (x', y') of
                                                                                (SOME a, SOME b) => (a, b)::(align us)
                                                                              | _ => (align us) end
                                | align (_::us) = align us;
                              val unifications = simplify (Type.unify [(genf, geng)]) handle Type.TUNIFY => raise Match;
                          in align unifications end;
    fun allCorrStrength cs s = let val (h, t) = Stream.step s
                               in case doCorrespond cs h of
                                      NONE => NONE
                                    | SOME cs' => Option.map (fn vs => (map Correspondence.strength cs')@vs) (allCorrStrength cs t)
                               end handle Subscript => NONE;
    fun attrCorrStrength p cs opts = case allCorrStrength cs opts of
                                         SOME vs => let val vMax = List.max Real.compare vs;
                                                    in SOME (p, vMax) end
                                       | NONE => raise Match;
    val getType = getAttr Attribute.getType;
    val getHoles = getAttr Attribute.getHoles;
    val getTokens = getAttr Attribute.getTokens;
    val getContent = getAttr Attribute.getContent;
in
fun checkAttrCorr cs (a, b) =
    let (*  Type * Type  *)
        val (at, bt) = mappair getType (a, b);
        (* Need to check if there are holes: if so, we ignore this type match *)
        val _ = if fails (fn () => mappair getHoles (a, b)) then ()  else raise Match;
        val unified = Stream.fromList (unifyish (at, bt));
        val typePairs = Stream.map (mappair propertyFromType) unified;
    in
        attrCorrStrength (a, b) cs typePairs
    end handle Match =>
    let (*  Content * Content  *)
        val (ac, bc) = mappair getContent (a, b);
        val unified = Stream.fromList (unifyish (ac, bc));
        val typePairs = Stream.map (mappair propertyFromType) unified;
    in
        attrCorrStrength (a, b) cs typePairs
    end handle Match =>
    let (*  (Holes * Type) * (Holes * Type)  *)
        val (ah, bh) = mappair (Stream.fromList o Attribute.M.toPairList o getHoles) (a, b);
        val holePairs = Stream.map (fn ((a, _), (b, _)) => (a, b)) (Stream.filter (fn ((_, c), (_, c')) => c = c') (Stream.product ah bh));
        val _ = if Stream.null holePairs then raise Match else ();
        val (at, bt) = mappair getType (a, b);
        val typePairs = Stream.map (mappair propertyFromType) (Stream.cons ((at, bt), (Stream.flatmap (Stream.fromList o unifyish) holePairs) handle Match => Stream.empty));
    in
        attrCorrStrength (a, b) cs typePairs
    end handle Match =>
    let (*  Tokens * Tokens  *)
        val (at, bt) = mappair (Stream.fromList o getTokens) (a, b);
        val tokPairs = Stream.product at bt;
        val _ = if Stream.null tokPairs then raise Match else ();
        val propPairs = Stream.map (mappair propertyFromToken) tokPairs;
    in
        attrCorrStrength (a, b) cs propPairs
    end handle Match => NONE;

fun potentialAttrCorr (a, b) =
    let
        val types = fn () =>
            let
                val typeStream = Stream.fromList (unifyish (mappair getType (a, b)));
                val propStream = Stream.map (mappair propertyFromType) typeStream;
            in
                propStream
            end handle Match => Stream.empty;
        val content = fn () =>
            let
                val contentStream = Stream.fromList (unifyish (mappair getContent (a, b)));
                val propStream = Stream.map (mappair propertyFromType) contentStream;
            in
                propStream
            end handle Match => Stream.empty;
        val holes = fn () =>
            let
                val (ah, bh) = mappair ((map (fn (k, _) => k)) o Attribute.M.toPairList o getHoles) (a, b);
                val holeStream = uncurry Stream.product (mappair Stream.fromList (ah, bh));
                val typeStream = Stream.flatmap (Stream.fromList o (fn h => unifyish h handle Match => [])) holeStream;
                val propStream = Stream.map (mappair propertyFromType) typeStream;
            in
                propStream
            end handle Match => Stream.empty;
        val tokens = fn () =>
            let
                val tokenStream = uncurry Stream.product (mappair (Stream.fromList o getTokens) (a,b)
                                                          handle Match => (Stream.empty, Stream.empty));
                val propStream = Stream.map (mappair propertyFromToken) tokenStream;
            in
                propStream
            end;
        val chosen = Stream.flatmap (fn f => f()) (Stream.fromList [types, content, holes, tokens]);
    in
        chosen
    end;
end;

fun findMatches cs rs =
    let
        val matchChecker = fn m => not o PropertySet.isEmpty o (m rs);
        val leftMatches = Stream.filter (matchChecker Correspondence.leftMatches) cs;
        val rightMatches = Stream.filter (matchChecker Correspondence.rightMatches) cs;
    in (leftMatches, rightMatches) end;

fun chooseNew options existing =
    let
        fun maybeUse (option, reason) cc =
            if corrExists option existing
            then cc()
            else SOME (option, reason);
        fun listChoose (xs, s) =
            let
                val (x, xs') = Random.chooseRemove xs;
            in
                maybeUse (x()) (fn () => listChoose (xs', s))
            end handle List.Empty => NONE;
        fun streamChoose (rest, s) =
            let
                val _ = if Stream.null s then raise Subscript else ();
                val (x, xf) = Stream.lazyStep s;
                val r = Random.random ();
            in
                if r < 0.5  (* Lower probabilities means it will walk further before finding something *)
                then maybeUse (x()) (fn () => streamChoose (rest, xf))
                else streamChoose ((x::rest), xf)
            end
            handle Subscript => listChoose (rest, Stream.empty);
    in
        streamChoose ([], options)
    end;


(** Discovery rules **)
(*  All of these have type state -> (correspondence * reason) option *)

(* Identity is handled elsewhere in the code, so ignore it here *)
fun discoverIdentity (cs, rss, rs') = let val corrs = Stream.map (fn p => (makeCorr (p, p), IDENTITY p)) (Stream.fromList (PropertySet.toList rs'))
                                      in chooseNew corrs cs end handle List.Empty => NONE;

fun discoverReversal (cs, rss, rs') =
    let
        fun flipCorr (a, b, c) = ((b, a, c), (* c needs to be changed too *)
                                  REVERSAL (a, b, c));
        val (leftMatches, rightMatches) = findMatches (Stream.fromList cs) rs';
        val corrs = Stream.map flipCorr (Stream.interleave leftMatches rightMatches);
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
        val css = Stream.fromList cs;
        val (leftMatches, rightMatches) = findMatches css rs';
        val corrPairs = Stream.interleave (Stream.product leftMatches css) (Stream.product css rightMatches);
        val validCorrPairs = Stream.filter doCompose corrPairs;
        val corrs = Stream.map compose validCorrPairs;
    in
        chooseNew corrs cs
    end handle List.Empty => NONE;

(* fun discoverKind (cs, rss, rs') = NONE; *)

fun discoverAttribute (cs, rss, rs') =
    let
        val potentialCorrs = Stream.flatmap (fn rs => streamSetProduct rs rs') (Stream.fromList rss);
        val matchingAttrs = Stream.mapPartial (checkAttrCorr cs) potentialCorrs;
        val corrs = Stream.map (fn (pq, s) => (makeCorr' s pq, ATTRIBUTE pq)) matchingAttrs;
    in
        chooseNew corrs cs
    end handle List.Empty => NONE;

fun discoverValue (cs, rss, rs') =
    let
        val allProps = PropertySet.union rs' (PropertySet.unionAll rss);
        fun sourceProperty p = PropertySet.filterMatches p allProps;
        fun sourcePropPairs ((p, q), s) = let val pqs = streamSetProduct (sourceProperty p) (sourceProperty q)
                                          in Stream.map (fn v => (v, s)) pqs end;
        val matchingValues = Stream.flatmap corrToPairs ((uncurry Stream.interleave) (findMatches (Stream.fromList cs) rs'));
        val attachedAttrs = Stream.flatmap sourcePropPairs matchingValues;
        fun findAttrs (v, s) = Stream.map (fn p => (p, v, s)) (potentialAttrCorr v);
        val attrOptions = Stream.flatmap findAttrs attachedAttrs;
        val corrs = Stream.map (fn (pq, ab, s) => (makeCorr' s pq, VALUE ab)) attrOptions;
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
        fun insert ans c [] = c::ans
          | insert ans c (c'::cs) = if (Correspondence.sameProperties c c')
                                    then
                                        if (Correspondence.strength c) > (Correspondence.strength c')
                                        then ans @ (c::cs)
                                        else ans @ (c'::cs)
                                    else insert (c'::ans) c cs;
        fun addCorr c (cs, rss, rs') = case c of
                                           SOME (c', _) => (insert [] c' cs, rss, rs')
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
