Package["WolframInstitute`Infrageometry`"]

PackageExport[InfraSubstrate]
PackageExport[InfraSubstrateStyle]
PackageExport[AmbientGraphStyle]


(* ===================== InfraSubstrate ===================== *)

(* InfraSubstrate[name, size, style] is the named example substrate at tier "Small" | "Medium" |
   "Large" or a raw spec, drawn as an AmbientGraphStyle backdrop for a construction on top of it.
   InfraSubstrate[] lists the roster by class; InfraSubstrate[All] is the flat name list.
   Generation is seeded per (name, size), so repeated calls return the same graph.

   Tiers are calibrated to a common ladder of roughly 100 / 300 / 1000 vertices (about
   x3 per step, so the percentual growth corresponds across substrates), as closely as
   each generator's granularity allows -- exceptions are noted on their lines.

   The roster below is the definition: one substrate[name, size] line per name, the tier table
   inline (a raw spec passes through the replacement untouched), and every exception -- an
   interior strip, a kept embedding, special coordinates -- written into the definition itself.
   To add a substrate, add a line and its name to the list; to change one, edit its line. *)

(* The roster is classified by what a substrate models: an OPEN manifold (a boundaryless
   patch -- an open subset of an unbounded or cut geometry, delivered with the rim contour
   edges removed), a CLOSED manifold (a compact tessellated surface, nothing to strip), an
   exotic graph (no manifold model), or a Wolfram-model universe. *)

$substrateClasses = <|
  "OpenManifold" -> {
    "PlanePatch", "BoxPatch",
    "TriangularPatch", "SquarePatch", "HexagonalPatch", "HyperbolicPatch", "InflatedSquarePatch",
    "SquareGridPatch", "CubicGridPatch" },
  "ClosedManifold" -> {
    "SphereMesh",
    "SquareTorus", "TriangularTorus", "HexagonalTorus",
    "RoundEllipsoid", "ProlateEllipsoid", "TriaxialEllipsoid", "Buckyball" },
  "Exotic" -> { "BinaryTree", "DilutedTree", "CompleteGraph", "SierpinskiTriangle" },
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

(* inflation roughly doubles a patch, so it takes its own smaller radii *)
substrate[ "InflatedSquarePatch", size_ ] :=
  InflateGraph @ substrate[ "SquarePatch", size /. { "Small" -> 5, "Medium" -> 8, "Large" -> 15 } ]

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

substrate[ "RoundEllipsoid", size_ ] :=
  UniformLengthGraph[ BoundaryDiscretizeRegion @ Ellipsoid[ { 0, 0, 0 }, { 1, 1, 1 } ],
    size /. { "Small" -> 100, "Medium" -> 300, "Large" -> 1000 }, "KeepCoordinates" -> True ]

substrate[ "ProlateEllipsoid", size_ ] :=
  UniformLengthGraph[ BoundaryDiscretizeRegion @ Ellipsoid[ { 0, 0, 0 }, { 5, 1, 1 } ],
    size /. { "Small" -> 100, "Medium" -> 300, "Large" -> 1000 }, "KeepCoordinates" -> True ]

substrate[ "TriaxialEllipsoid", size_ ] :=
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

Options[ InfraSubstrate ] = { "KeepCoordinates" -> False };

InfraSubstrate[ name_String, size_, style : ( _String | Automatic ) : Automatic,
    opts : OptionsPattern[ { InfraSubstrate, Graph } ] ] :=
  With[
    { g = substrateGraph[ name, size ] },
    Graph[ g, FilterRules[ { opts }, Options @ Graph ],
      substrateCoordinates[ g, TrueQ @ OptionValue[ InfraSubstrate,
        FilterRules[ { opts }, Options @ InfraSubstrate ], "KeepCoordinates" ] ],
      Sequence @@ AmbientGraphStyle @ Replace[ style, Automatic :> defaultAmbientStyle[ g, size ] ] ] ]

(* memoized: substrates are rebuilt across many figure cells, and seeding makes the cache honest *)
substrateGraph[ name_, size_ ] := substrateGraph[ name, size ] =
  BlockRandom[ substrate[ name, size ], RandomSeeding -> Hash @ { name, size } ]

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
   to read; a raw spec is placed on that scale by its vertex count *)
defaultAmbientStyle[ _, "Small" ] := "Gray"
defaultAmbientStyle[ _, "Medium" ] := "GrayOpaque"
defaultAmbientStyle[ _, "Large" ] := "GrayFaint"
defaultAmbientStyle[ g_, _ ] := Which[ VertexCount @ g <= 250, "Gray", VertexCount @ g <= 800, "GrayOpaque", True, "GrayFaint" ]


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

(* binary hyperedges are already a graph; higher arities go through the 2-section *)
hypergraphGraph[ state_ ] :=
  If[ AllTrue[ state, Length @ # === 2 & ],
    Graph @ DeleteDuplicates[ UndirectedEdge @@@ Sort /@ DeleteCases[ state, { v_, v_ } ] ],
    UndirectedGraph @ ResourceFunction[ "HypergraphToGraph" ][ state ] ]

registryUniverse[ id_ ] := registryUniverse[ id ] =
  <| "Rule" -> First @ Flatten[ { ResourceFunction[ "WolframModelData" ][ id, "Rule" ] }, 2 ],
     "Init" -> ResourceFunction[ "WolframModelData" ][ id, "InitialCondition" ] |>


(* ===================== InfraSubstrateStyle ===================== *)

(* InfraSubstrateStyle[g, style] applies the substrate backdrop styling to ANY graph, so
   a hand-built object -- a Wolfram-model final state included -- draws exactly like the
   roster substrates.  style Automatic (default) picks the ambient gray by vertex count,
   the same rule InfraSubstrate uses for a raw spec; a list of hyperedges goes through
   the same 2-section conversion as the wm substrates.  Graph options are forwarded. *)

InfraSubstrateStyle[ g_Graph, style : ( _String | Automatic ) : Automatic,
    opts : OptionsPattern[ Graph ] ] :=
  Graph[ g, FilterRules[ { opts }, Options @ Graph ],
    Sequence @@ AmbientGraphStyle @ Replace[ style, Automatic :> defaultAmbientStyle[ g, None ] ] ]

InfraSubstrateStyle[ state : { __List }, style : ( _String | Automatic ) : Automatic,
    opts : OptionsPattern[ Graph ] ] :=
  InfraSubstrateStyle[ hypergraphGraph @ state, style, opts ]


(* ===================== Ambient styles ===================== *)

(* AmbientGraphStyle[name] is the Graph option list rendering a substrate as a backdrop, so that a
   highlighted construction drawn on top of it stands out; splice it in with
   Graph[g, Sequence @@ AmbientGraphStyle["GrayFaint"]]. *)

(* The vertex size is absolute ({"AbsolutePointSize", 4}) and identical in every style, so
   the dots read the same across all graphs and tiers; a highlight mark stays legible above
   them with AbsolutePointSize[5].  Vertices are outlined disks (EdgeForm) over a faint
   fill; the opacity ladder steps down as the picture gets denser, "GrayFaint" being the
   reference styling for the large graphs. *)

ambientGraphStyles = <|
  "Default"    -> { },
  "GrayFaint"  -> { EdgeStyle -> Directive[ StandardGray, Opacity[ 0.35 ] ],
                    VertexStyle -> Directive[ StandardGray, Opacity[ 0.13 ], EdgeForm[ { GrayLevel[ 0 ], Opacity[ 0.25 ] } ] ],
                    VertexSize -> { "AbsolutePointSize", 4 } },
  "GrayOpaque" -> { EdgeStyle -> Directive[ StandardGray, Opacity[ 0.5 ] ],
                    VertexStyle -> Directive[ StandardGray, Opacity[ 0.2 ], EdgeForm[ { GrayLevel[ 0 ], Opacity[ 0.35 ] } ] ],
                    VertexSize -> { "AbsolutePointSize", 4 } },
  "Gray"       -> { EdgeStyle -> Directive[ StandardGray, Opacity[ 0.7 ] ],
                    VertexStyle -> Directive[ StandardGray, Opacity[ 0.3 ], EdgeForm[ { GrayLevel[ 0 ], Opacity[ 0.5 ] } ] ],
                    VertexSize -> { "AbsolutePointSize", 4 } }
|>;

AmbientGraphStyle[ ] := Keys @ ambientGraphStyles

(* the tier names are accepted as aliases, so a hand-drawn figure can ask for the same
   look its tier would get: Small -> "Gray", Medium -> "GrayOpaque", Large -> "GrayFaint" *)
AmbientGraphStyle[ name_String ] := ambientGraphStyles @ Replace[ name,
  { "Small" -> "Gray", "Medium" -> "GrayOpaque", "Large" -> "GrayFaint" } ]
