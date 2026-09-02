Package["WolframInstitute`Infrageometry`"]

PackageExport[InfraSubstrate]
PackageExport[InfraSubstrateStyle]
PackageExport[InfraSubstrateCode]


(* ===================== InfraSubstrate ===================== *)

(* InfraSubstrate[name, size, style] is the named example substrate at size "Small" | "Medium" |
   "Large" or a raw spec, drawn as an InfraSubstrateStyle backdrop for a construction on top of it.
   InfraSubstrate[] lists the roster by class; InfraSubstrate[All] is the flat name list.
   A substrate drawn from a random construction is seeded from outside, with SeedRandom, like any
   other random draw -- that is what recovers a figure in another session or on another machine.

   Any substrate inflates: "Inflate" -> amount grows a fiber of that many extra vertices over
   every vertex through InflateGraph, and "Inflate" -> {opts} passes its full option list.  So
   local dimensional noise is a knob on the whole roster rather than a substrate of its own.

   The three named sizes are calibrated to a common ladder of roughly 100 / 300 / 1000 vertices
   (about x3 per step, so the percentual growth corresponds across substrates), as closely as
   each generator's granularity allows -- exceptions are noted on their lines.

   The roster below is the definition: one substrate[name, size] line per name, the size table
   inline (a raw spec passes through the replacement untouched), and every exception -- an
   interior strip, a kept embedding, special coordinates -- written into the definition itself.
   To add a substrate, add a line and its name to the list; to change one, edit its line. *)

(* The roster is classified by what a substrate models: an OPEN manifold (a boundaryless
   patch -- an open subset of an unbounded or cut geometry, delivered with the rim contour
   edges removed), a CLOSED manifold (a compact tessellated surface, nothing to strip), a
   FRACTAL (self-similar, a Hausdorff dimension between the integers), an exotic graph (no
   manifold model and no scaling law), or a Wolfram-model universe. *)

$substrateClasses = <|
  "OpenManifold" -> {
    "PlanePatch", "BoxPatch",
    "TriangularPatch", "SquarePatch", "HexagonalPatch", "HyperbolicPatch",
    "SquareGridPatch", "CubicGridPatch" },
  "ClosedManifold" -> {
    "SphereMesh",
    "SquareTorus", "TriangularTorus", "HexagonalTorus",
    "UniformLengthSphere", "ProlateUniformLengthEllipsoid", "TriaxialUniformLengthEllipsoid", "Buckyball" },
  "Fractal" -> { "SierpinskiTriangle", "MengerCarpet", "MengerSponge" },
  "Exotic" -> { "BinaryTree", "DilutedTree", "CompleteGraph" },
  "WolframModel" -> { "wm6655", "wm8619", "wm1811" }
|>;

substrate[ "PlanePatch", size_ ] :=
  trimPendants @ BoundarylessGraph @ DiscretizeRegion[ Rectangle[ ],
    MaxCellMeasure -> size /. { "Small" -> 0.0085, "Medium" -> 0.0028, "Large" -> 0.00082 },
    PrecisionGoal -> Infinity ]

substrate[ "BoxPatch", size_ ] :=
  trimPendants @ BoundarylessGraph @ DiscretizeRegion[ Cuboid[ ],
    MaxCellMeasure -> size /. { "Small" -> 0.004, "Medium" -> 0.00133, "Large" -> 0.0004 },
    PrecisionGoal -> Infinity ]

(* DiscretizeRegion ignores MaxCellMeasure on special surface regions unless PrecisionGoal
   is also given, and then only in the {"Area" -> m} spec form; the low goal leaves refined
   vertices off the unit sphere, so they are normalized back onto it *)
substrate[ "SphereMesh", size_ ] :=
  With[ { mr = DiscretizeRegion[ Sphere[ ],
      MaxCellMeasure -> { "Area" -> size /. { "Small" -> 0.5, "Medium" -> 0.1, "Large" -> 0.02 } },
      PrecisionGoal -> 1 ] },
    Graph[ IndexGraph @ MeshConnectivityGraph @ mr, VertexCoordinates -> Normalize /@ MeshCoordinates @ mr ] ]

substrate[ "TriangularPatch", size_ ] :=
  BoundarylessGraph[ TessellationNeighborhoodGraph[ { 3, 6 }, size /. { "Small" -> 5, "Medium" -> 9, "Large" -> 16 } ], Method -> "MaxDegree" ]

substrate[ "SquarePatch", size_ ] :=
  BoundarylessGraph[ TessellationNeighborhoodGraph[ { 4, 4 }, size /. { "Small" -> 7, "Medium" -> 12, "Large" -> 22 } ], Method -> "MaxDegree" ]

substrate[ "HexagonalPatch", size_ ] :=
  BoundarylessGraph[ TessellationNeighborhoodGraph[ { 6, 3 }, size /. { "Small" -> 8, "Medium" -> 14, "Large" -> 25 } ], Method -> "MaxDegree" ]

substrate[ "HyperbolicPatch", size_ ] :=
  BoundarylessGraph[ TessellationNeighborhoodGraph[ { 3, 7 }, size /. { "Small" -> 3, "Medium" -> 4, "Large" -> 5 } ], Method -> "MaxDegree" ]

substrate[ "SquareGridPatch", size_ ] :=
  BoundarylessGraph[ GridGraph[ size /. { "Small" -> { 10, 10 }, "Medium" -> { 17, 17 }, "Large" -> { 32, 32 } } ], Method -> "MaxDegree" ]

(* GridGraph indexes with the first dimension fastest, and without explicit coordinates a 3D
   grid falls back to a 2D spring layout *)
substrate[ "CubicGridPatch", size_ ] :=
  With[ { dims = size /. { "Small" -> { 5, 5, 5 }, "Medium" -> { 7, 7, 7 }, "Large" -> { 10, 10, 10 } } },
    BoundarylessGraph[ Graph[ GridGraph @ dims, VertexCoordinates -> Reverse /@ Tuples[ Range /@ Reverse @ dims ] ], Method -> "MaxDegree" ] ]

substrate[ "SquareTorus", size_ ] :=
  torusEmbedded[ "Square", size /. { "Small" -> { 10, 10 }, "Medium" -> { 20, 15 }, "Large" -> { 40, 25 } } ]

substrate[ "TriangularTorus", size_ ] :=
  torusEmbedded[ "Triangular", size /. { "Small" -> { 10, 10 }, "Medium" -> { 20, 15 }, "Large" -> { 40, 25 } } ]

(* the honeycomb carries two vertices per cell, so its torus takes half-size dims *)
substrate[ "HexagonalTorus", size_ ] :=
  torusEmbedded[ "Hexagonal", size /. { "Small" -> { 7, 7 }, "Medium" -> { 15, 10 }, "Large" -> { 25, 20 } } ]

(* a full binary tree only comes in sizes 2^k - 1, so the ladder is x4 like the buckyball *)
substrate[ "BinaryTree", size_ ] := KaryTree[ size /. { "Small" -> 63, "Medium" -> 255, "Large" -> 1023 } ]

substrate[ "CompleteGraph", size_ ] := CompleteGraph[ size /. { "Small" -> 10, "Medium" -> 30, "Large" -> 90 } ]

(* intermediate growth 2^(r^alpha): branch 2 exactly at the depths hit by Ceiling[k^(1/alpha)] *)
substrate[ "DilutedTree", size_ ] :=
  With[ { spec = size /. { "Small" -> { 1/2, 16 }, "Medium" -> { 1/2, 26 }, "Large" -> { 1/2, 42 } } },
    BranchingSequenceTree @ Table[
      If[ MemberQ[ Ceiling[ Range[ Last @ spec ]^( 1 / First @ spec ) ], level ], 2, 1 ],
      { level, Last @ spec } ] ]

substrate[ "SierpinskiTriangle", size_ ] :=
  IndexGraph @ MeshConnectivityGraph @ SierpinskiMesh[ size /. { "Small" -> 4, "Medium" -> 5, "Large" -> 6 } ]

(* the plane carpet, Hausdorff dimension log 8 / log 3 against the triangle's log 3 / log 2.
   Both Menger substrates multiply by about 7 a level, so neither can hold the x3 ladder and
   each simply takes its three widest drawable levels: 96 / 688 / 5280 here, 64 / 896 / 15616
   for the sponge, whose top level is the heaviest object in the roster by an order *)
substrate[ "MengerCarpet", size_ ] :=
  IndexGraph @ MeshConnectivityGraph @ MengerMesh[ size /. { "Small" -> 2, "Medium" -> 3, "Large" -> 4 } ]

substrate[ "MengerSponge", size_ ] :=
  IndexGraph @ MeshConnectivityGraph @ MengerMesh[ size /. { "Small" -> 1, "Medium" -> 2, "Large" -> 3 }, 3 ]

(* BuckyballGraph's 3D-ness lives in GraphLayout "Dimension" -> 3, which any Graph re-wrap
   resets to a 2D layout -- bake the embedding explicitly *)
substrate[ "Buckyball", size_ ] :=
  With[ { g = ResourceFunction[ "BuckyballGraph" ][ size /. { "Small" -> 1, "Medium" -> 2, "Large" -> 4 } ] },
    Graph[ g, VertexCoordinates -> GraphEmbedding @ g ] ]

(* a Wolfram-model universe is named by its Registry of Notable Universes number
   (wolframphysics.org/universes/wmNNNN), evolved from its registry initial condition for the
   given number of generations.  The universes the writeup uses are inlined so a figure builds
   with no network; any other wmNNNN is pulled through ResourceFunction["WolframModelData"]. *)

substrate[ "wm6655", size_ ] :=
  hypergraphGraph @ ResourceFunction[ "WolframModel" ][
    { { 1, 2 }, { 1, 3 } } -> { { 1, 2 }, { 1, 4 }, { 2, 4 }, { 3, 4 } },
    { { 1, 1 }, { 1, 1 } },
    size /. { "Small" -> 7, "Medium" -> 9, "Large" -> 11 }, "FinalState" ]

substrate[ "wm8619", size_ ] :=
  hypergraphGraph @ ResourceFunction[ "WolframModel" ][
    { { 1, 2, 2 }, { 1, 3, 4 } } -> { { 4, 5, 5 }, { 5, 3, 2 }, { 1, 2, 5 } },
    { { 1, 1, 1 }, { 1, 1, 1 } },
    size /. { "Small" -> 100, "Medium" -> 300, "Large" -> 1000 }, "FinalState" ]

substrate[ "wm1811", size_ ] :=
  hypergraphGraph @ ResourceFunction[ "WolframModel" ][
    { { 1, 1, 2 }, { 1, 3, 4 } } -> { { 4, 4, 3 }, { 2, 5, 3 }, { 2, 5, 3 } },
    { { 1, 1, 1 }, { 1, 1, 1 } },
    size /. { "Small" -> 100, "Medium" -> 300, "Large" -> 1000 }, "FinalState" ]

substrate[ "UniformLengthSphere", size_ ] :=
  UniformLengthGraph[ BoundaryDiscretizeRegion @ Ellipsoid[ { 0, 0, 0 }, { 1, 1, 1 } ],
    size /. { "Small" -> 100, "Medium" -> 300, "Large" -> 1000 }, "KeepCoordinates" -> True ]

substrate[ "ProlateUniformLengthEllipsoid", size_ ] :=
  UniformLengthGraph[ BoundaryDiscretizeRegion @ Ellipsoid[ { 0, 0, 0 }, { 5, 1, 1 } ],
    size /. { "Small" -> 100, "Medium" -> 300, "Large" -> 1000 }, "KeepCoordinates" -> True ]

substrate[ "TriaxialUniformLengthEllipsoid", size_ ] :=
  UniformLengthGraph[ BoundaryDiscretizeRegion @ Ellipsoid[ { 0, 0, 0 }, { 4, 2, 1 } ],
    size /. { "Small" -> 100, "Medium" -> 300, "Large" -> 1000 }, "KeepCoordinates" -> True ]

(* any registry entry beyond the inlined three, one network round-trip per id, memoized *)
substrate[ id_String /; StringMatchQ[ id, "wm" ~~ DigitCharacter .. ], size_ ] :=
  With[ { u = registryUniverse @ id },
    hypergraphGraph @ ResourceFunction[ "WolframModel" ][ u[ "Rule" ], u[ "Init" ],
      size /. { "Small" -> 6, "Medium" -> 8, "Large" -> 10 }, "FinalState" ] ]


(* ===================== The wrapper ===================== *)

InfraSubstrate[ ] := $substrateClasses

InfraSubstrate[ All ] := Catenate @ Values @ $substrateClasses

InfraSubstrate[ name_String ] := InfraSubstrate[ name, "Medium" ]

Options[ InfraSubstrate ] = { "KeepCoordinates" -> False, "Inflate" -> None };

InfraSubstrate[ name_String, size_, style : ( _String | Automatic ) : Automatic,
    opts : OptionsPattern[ { InfraSubstrate, Graph } ] ] :=
  With[
    { own = FilterRules[ { opts }, Options @ InfraSubstrate ] },
    { g = substrateGraph[ name, size, OptionValue[ InfraSubstrate, own, "Inflate" ] ],
      keep = TrueQ @ OptionValue[ InfraSubstrate, own, "KeepCoordinates" ] },
    Graph[ g, FilterRules[ { opts }, Options @ Graph ],
      substrateCoordinates[ g, keep ],
      Sequence @@ InfraSubstrateStyle @ Replace[ style, Automatic :> defaultAmbientStyle[ g, size ] ] ] ]

(* A random substrate is a draw like any other: it is seeded from outside, with SeedRandom, which
   is what makes the same figure recoverable in another session or on another machine.  BlockRandom
   carries no RandomSeeding of its own -- it is here only to leave the ambient stream where it
   found it, which is what lets the substrate be a function of the seed rather than of how many
   draws happened to precede it.  That in turn is what makes the memo honest: the ambient state
   is part of the key, so re-seeding gives a fresh graph instead of the cached one, while the
   repeated calls a figure makes across many cells cost one generation. *)
substrateGraph[ name_, size_, inflate_ ] :=
  substrateGraph[ name, size, inflate, Hash @ $RandomGeneratorState ]

substrateGraph[ name_, size_, inflate_, state_ ] := substrateGraph[ name, size, inflate, state ] =
  BlockRandom @ If[ inflate === None,
    substrate[ name, size ],
    InflateGraph[ substrate[ name, size ],
      Sequence @@ Replace[ inflate,
        amount : Except[ { ___Rule } ] :> { "ExtraVertices" -> amount } ] ] ]

(* a substrate is bare combinatorics by default: a stored embedding is discarded and a spring
   layout of its own dimension places the vertices; "KeepCoordinates" -> True draws the
   substrate where it lives instead *)
substrateCoordinates[ g_Graph, True ] :=
  If[ Options[ g, VertexCoordinates ] === { VertexCoordinates -> Automatic },
    Sequence @@ { }, VertexCoordinates -> GraphEmbedding @ g ]
substrateCoordinates[ g_Graph, _ ] :=
  If[ Options[ g, VertexCoordinates ] === { VertexCoordinates -> Automatic },
    Sequence @@ { },
    Sequence @@ { VertexCoordinates -> Automatic, GraphLayout ->
      { "VertexLayout" -> "SpringElectricalEmbedding",
        "Dimension" -> Last @ Dimensions @ GraphEmbedding @ g } } ]

(* the denser the picture, the fainter the backdrop must be for a construction drawn over it
   to read; a raw spec is placed on that scale by its vertex count.  A named size keeps its
   look even inflated: the count branch saturates at "GrayFaint" past 800 vertices, so
   deferring an inflated substrate to it would draw "Medium" and "Large" with one dot *)
defaultAmbientStyle[ _, "Small" ] := "Gray"
defaultAmbientStyle[ _, "Medium" ] := "GrayOpaque"
defaultAmbientStyle[ _, "Large" ] := "GrayFaint"
defaultAmbientStyle[ g_, _ ] := Which[ VertexCount @ g <= 250, "Gray", VertexCount @ g <= 800, "GrayOpaque", True, "GrayFaint" ]


(* ===================== InfraSubstrateCode ===================== *)

(* InfraSubstrateCode[name, size, style] is the code behind InfraSubstrate[name, size, style]:
   the construction with the style applied to it, as one line, which under the same SeedRandom
   gives the identical graph.  The roster line is read off its own down-value rather than
   transcribed, so the printed code cannot drift from the code that runs; the size table and the
   layout dimension are baked in, and a call to a private helper is replaced by the body it
   stands for, so nothing in the printed code refers to a name the reader cannot see.
   "KeepCoordinates" is the one clause that has to name the graph twice, so it takes a With. *)

Options[ InfraSubstrateCode ] = Options[ InfraSubstrate ];

InfraSubstrateCode[ name_String ] := InfraSubstrateCode[ name, "Medium" ]

InfraSubstrateCode[ name_String, size_, style : ( _String | Automatic ) : Automatic,
    opts : OptionsPattern[ { InfraSubstrate, Graph } ] ] :=
  With[
    { own = FilterRules[ { opts }, Options @ InfraSubstrate ],
      graphOpts = FilterRules[ { opts }, Options @ Graph ] },
    { inflate = OptionValue[ InfraSubstrate, own, "Inflate" ] },
    { g = substrateGraph[ name, size, inflate ],
      generator = substrateGenerator[ name, size, inflate ] },
    { ambient = Replace[ style, Automatic :> defaultAmbientStyle[ g, size ] ],
      clauses = coordinateCode[ g, TrueQ @ OptionValue[ InfraSubstrate, own, "KeepCoordinates" ] ] },
    { arguments = Join[
        If[ graphOpts === { }, { }, { codeString @ HoldComplete @ graphOpts } ],
        clauses,
        { "Sequence @@ InfraSubstrateStyle[" <> codeString @ HoldComplete @ ambient <> "]" } ] },
    If[ FreeQ[ clauses, "VertexCoordinates -> GraphEmbedding[graph]" ],
      "Graph[" <> StringRiffle[ Prepend[ arguments, codeString @ generator ], ", " ] <> "]",
      "With[{graph = " <> codeString @ generator <> "},\n  Graph[" <>
        StringRiffle[ Prepend[ arguments, "graph" ], ", " ] <> "]]" ]
  ]

(* the roster line for this name, with the size table collapsed to the value this size selects
   and the inflation call wrapped around it -- everything substrateGraph feeds to BlockRandom.
   The collapsed subexpression is not always the bare symbol: "/." binds looser than "->", so
   MaxCellMeasure -> size /. table is a replacement on the whole rule, and the mesh patches
   read that way.  The v /; True is what forces the value out -- a plain RuleDelayed right-hand
   side lands inside the HoldComplete unevaluated, printing the table back verbatim. *)
substrateGenerator[ name_, size_, inflate_ ] :=
  With[
    { clause = FirstCase[ DownValues @ substrate,
        Verbatim[ RuleDelayed ][ Verbatim[ HoldPattern ][ substrate[ p_, s_ ] ], body_ ] /; MatchQ[ name, p ] :>
          { First @ s, HoldComplete @ body } ] },
    { sizeSymbol = First @ clause },
    { baked = Last[ clause ] /.
        { HoldPattern[ ReplaceAll[ selected_, table_ ] ] /; ! FreeQ[ Unevaluated @ selected, sizeSymbol ] :>
            With[ { v = ( selected /. sizeSymbol -> size ) /. table }, v /; True ],
          sizeSymbol -> size } },
    { inlined = FixedPoint[ expr |-> expr /. inliningRules @ expr, baked ] /. $inlined[ x_ ] :> x },
    If[ inflate === None, inlined,
      Join[ HoldComplete @ InflateGraph, inlined,
        HoldComplete @@ Replace[ inflate, amount : Except[ { ___Rule } ] :> { "ExtraVertices" -> amount } ] ] /.
        HoldComplete[ head_, rest__ ] :> HoldComplete @ head[ rest ] ]
  ]

(* the rules that beta-reduce one layer of private-helper calls, so the printed code carries no
   name the reader cannot see.  Head constraints are relaxed to plain blanks -- this is syntactic
   substitution and the argument in the held code is still an unevaluated expression, so g_Graph
   would never match -- and a memoizing right-hand side is unwrapped to the value it caches.
   Unlike the size table, these bodies must land unevaluated, which a RuleDelayed inside a
   HoldComplete does on its own; the $inlined marker only keeps the outer HoldComplete apart. *)
inliningRules[ expr_ ] :=
  With[
    { helpers = DeleteDuplicates @ Cases[ expr,
        s_Symbol /; StringEndsQ[ Context @ s, "`PackagePrivate`" ] && DownValues @ s =!= { },
        Infinity, Heads -> True ] },
    Select[ Catenate[ DownValues /@ helpers ], rule |-> ! FreeQ[ First @ rule, Verbatim @ Pattern ] ] /.
      { Verbatim[ RuleDelayed ][ lhs_, Verbatim[ Set ][ _, body_ ] ] :>
          RuleDelayed[ lhs /. Verbatim[ Blank ][ _ ] -> Blank[ ], $inlined @ body ],
        Verbatim[ RuleDelayed ][ lhs_, body_ ] :>
          RuleDelayed[ lhs /. Verbatim[ Blank ][ _ ] -> Blank[ ], $inlined @ body ] }
  ]

(* InputForm, with the paclet's own contexts dropped so exported symbols read as the bare names a
   loaded paclet resolves, and with the trailing $ that ReplaceAll leaves on the scoped locals of
   an inlined body taken back off -- the emitted code binds nothing those names could capture *)
codeString[ HoldComplete[ e_ ] ] :=
  StringReplace[
    StringDelete[ ToString[ Unevaluated @ e, InputForm ],
      { "WolframInstitute`Infrageometry`" ~~ Shortest[ ___ ] ~~ "`PackagePrivate`",
        "WolframInstitute`Infrageometry`" } ],
    RegularExpression[ "([A-Za-z][A-Za-z0-9]*)\\$+(?![A-Za-z0-9`])" ] -> "$1" ]

(* the kept-coordinates clause carries the whole embedding, so it is printed as the call that
   produced it rather than as a thousand literal points -- the one clause that names the graph a
   second time, and so the one that costs the printed code its single line *)
coordinateCode[ g_, keep_ ] :=
  Replace[ { substrateCoordinates[ g, keep ] },
    { { } -> { },
      { VertexCoordinates -> _List } -> { "VertexCoordinates -> GraphEmbedding[graph]" },
      clauses_ :> Map[ codeString @ HoldComplete @ # &, clauses ] } ]


(* ===================== Roster helpers ===================== *)

(* trimPendants[g] repeatedly deletes degree-1 vertices: a mesh patch has a few corner vertices
   left hanging by one edge, and a vertex with a single neighbour models nothing in the plane *)
trimPendants[ g_Graph ] :=
  FixedPoint[ h |-> VertexDelete[ h, Pick[ VertexList @ h, VertexDegree @ h, 1 ] ], g ]

(* the tessellation's vertices are lattice indices {i, j} (plus a sublattice index for the
   honeycomb), so the torus they wrap around is drawn as an actual torus rather than guessed
   at by a spring layout *)
torusEmbedded[ shape_, { m_, n_ } ] :=
  With[ { g = TorusTessellation[ { m, n }, shape ] },
    Graph[ g, VertexCoordinates ->
      Map[
        v |-> With[ { s = If[ Length @ v >= 3, v[[ 3 ]], 0 ] },
          { u = 2 Pi ( v[[ 1 ]] + s / 2 ) / m, w = 2 Pi ( v[[ 2 ]] + s / 2 ) / n },
          { ( 1 + 0.4 Cos[ w ] ) Cos[ u ], ( 1 + 0.4 Cos[ w ] ) Sin[ u ], 0.4 Sin[ w ] } ],
        VertexList @ g ] ] ]

(* binary hyperedges are already a graph; higher arities go through the 2-section.  The argument is
   bound before it is read three times so that InfraSubstrateCode, which inlines this body into the
   call, prints one evolution rather than three copies of it *)
hypergraphGraph[ state_ ] :=
  With[ { hyperedges = state },
    If[ AllTrue[ hyperedges, Length @ # === 2 & ],
      Graph @ DeleteDuplicates[ UndirectedEdge @@@ Sort /@ DeleteCases[ hyperedges, { v_, v_ } ] ],
      UndirectedGraph @ ResourceFunction[ "HypergraphToGraph" ][ hyperedges ] ] ]

registryUniverse[ id_ ] := registryUniverse[ id ] =
  <| "Rule" -> First @ Flatten[ { ResourceFunction[ "WolframModelData" ][ id, "Rule" ] }, 2 ],
     "Init" -> ResourceFunction[ "WolframModelData" ][ id, "InitialCondition" ] |>


(* ===================== InfraSubstrateStyle ===================== *)

(* InfraSubstrateStyle[name] is the Graph option list rendering a substrate as a backdrop, so that a
   highlighted construction drawn on top of it stands out; splice it in with
   Graph[g, Sequence @@ InfraSubstrateStyle["GrayFaint"]]. *)

(* The vertex size is scaled -- a fraction of the coordinate diagonal -- so one style draws
   one dot on a 100-vertex patch and on a 1000-vertex plane, which the default sizing does
   not: every other VertexSize value is a fraction of the graph's own nearest-neighbour
   spacing, so the dot tracks the density instead of the picture.  Vertices are outlined
   disks (EdgeForm) over a faint fill.  Both ladders step down as the picture gets denser,
   "GrayFaint" being the reference styling for the large graphs, since one value across all
   three turns a 1000-vertex substrate into touching rings with the edges swallowed.

   Scaled is a fraction of the picture, not of the page: enlarge a figure and the dot grows
   with it, while an InfraSceneHighlight mark drawn over it stays at its AbsolutePointSize.
   The two only keep their relative size at one ImageSize. *)

substrateStyle[ edge_, fill_, rim_, dot_ ] := {
  EdgeStyle -> Directive[ StandardGray, Opacity[ edge ] ],
  VertexStyle -> Directive[ StandardGray, Opacity[ fill ], EdgeForm[ { GrayLevel[ 0 ], Opacity[ rim ] } ] ],
  VertexSize -> { "Scaled", dot } }

(* The colours and the shape of the spec are Jeremy's design sheet verbatim -- StandardGray
   edges, a StandardGray fill under a black EdgeForm rim -- and so is the 0.35 top of the edge
   ladder.  The one departure: his vertices sit at 0.13 under 0.35 edges, which leaves the dot
   invisible on anything drawn in three dimensions, so the vertex channel is lifted just past
   the edges instead of far past them.  The dot is what an observer stands on and the edge only
   says which dots are adjacent, so the vertices lead; keeping the lead small is what holds the
   backdrop to his gray web.  This also removes any need to style by embedding dimension -- the
   same numbers carry the plane patch, the torus and the tetrahedralised box. *)

infraSubstrateStyles = <|
  "Default"    -> { },
  "GrayFaint"  -> substrateStyle[ 0.22, 0.33, 0.45, 0.006 ],
  "GrayOpaque" -> substrateStyle[ 0.3, 0.45, 0.6, 0.009 ],
  "Gray"       -> substrateStyle[ 0.35, 0.5, 0.65, 0.013 ]
|>;

InfraSubstrateStyle[ ] := Keys @ infraSubstrateStyles

(* the size names are accepted as aliases, so a hand-drawn figure can ask for the same
   look its size would get: Small -> "Gray", Medium -> "GrayOpaque", Large -> "GrayFaint" *)
InfraSubstrateStyle[ name_String ] := infraSubstrateStyles @ Replace[ name,
  { "Small" -> "Gray", "Medium" -> "GrayOpaque", "Large" -> "GrayFaint" } ]
