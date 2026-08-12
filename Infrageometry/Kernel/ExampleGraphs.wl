Package["WolframInstitute`Infrageometry`"]

PackageExport[PunchHole]
PackageScope[TorusTessellation]

PackageExport[SierpinskiGraph]
PackageExport[BetheGraph]
PackageExport[BranchingSequenceTree]
PackageScope[triangleReplace]

PackageExport[ExampleGraphData]
PackageExport[$ExampleGraphs]
PackageExport[AmbientGraphStyle]
PackageExport[InflateGraph]
PackageExport[InflatedVertex]
PackageScope[trimPendants]
PackageScope[exampleGraphTiers]


(* ===================== Hole punching ===================== *)

(* PunchHole[g, r] removes the closed r-ball around a random vertex of g.
   PunchHole[g, c -> r] removes the closed r-ball around vertex c.
   For multiple holes, fold over a list:
     Fold[PunchHole, g, {3, 2, 1}]                  -- three random holes
     Fold[PunchHole, g, {c1 -> 2, c2 -> 1, 3}]      -- explicit + random  *)

PunchHole[ g_Graph, r_Integer ] :=
  PunchHole[ g, RandomChoice @ VertexList @ g -> r ]

PunchHole[ g_Graph, c_ -> r_Integer ] :=
  Subgraph[ g, Complement[ VertexList @ g, VertexList @ NeighborhoodGraph[ g, c, r ] ] ]


(* ===================== Torus tessellations ===================== *)

(* TorusTessellation[{m, n}, shape] returns the vertex-transitive flat-torus
   Cayley graph carrying the regular {p, q}-tessellation indicated by shape;
   shape defaults to "Triangular" (the most isotropic discrete plane).
     "Square"     -- {4, 4}, 4-regular, Cay(Z_m x Z_n, {+-e_1, +-e_2})
     "Triangular" -- {3, 6}, 6-regular, Cay(Z_m x Z_n, {+-e_1, +-e_2, +-(e_1+e_2)})
     "Hexagonal"  -- {6, 3}, 3-regular, two-orbit Cay on Z_m x Z_n x Z_2 *)

TorusTessellation[ { m_Integer, n_Integer }, opts : OptionsPattern[ ] ] :=
  TorusTessellation[ { m, n }, "Triangular", opts ]

TorusTessellation[ { m_Integer, n_Integer }, "Square", opts : OptionsPattern[ ] ] :=
  Graph[ GraphProduct[ CycleGraph[ m ], CycleGraph[ n ], "Cartesian" ], opts, VertexCoordinates -> Automatic ]

TorusTessellation[ { m_Integer, n_Integer }, "Triangular", opts : OptionsPattern[ ] ] :=
  Graph[
    Flatten @ Table[
      { { i, j } <-> { Mod[ i + 1, m ], j }
      , { i, j } <-> { i, Mod[ j + 1, n ] }
      , { i, j } <-> { Mod[ i + 1, m ], Mod[ j + 1, n ] } },
      { i, 0, m - 1 }, { j, 0, n - 1 }
    ],
    opts
  ]

TorusTessellation[ { m_Integer, n_Integer }, "Hexagonal", opts : OptionsPattern[ ] ] :=
  Graph[
    Flatten @ Table[
      { { i, j, 0 } <-> { i, j, 1 }
      , { i, j, 0 } <-> { Mod[ i - 1, m ], j, 1 }
      , { i, j, 0 } <-> { i, Mod[ j - 1, n ], 1 } },
      { i, 0, m - 1 }, { j, 0, n - 1 }
    ],
    opts
  ]


(* ===================== Sierpinski graph (trivalent) ===================== *)

(* SierpinskiGraph[n] is the trivalent Sierpinski graph: start from the 3-simplex K_4
   (the tetrahedron) and iterate corner-cutting (truncation) n-1 times.  triangleReplace
   replaces every vertex by a triangle, its three incident edges reattaching to the
   corners -- on a cubic graph this is truncation, so the graph stays 3-regular at every
   generation (4*3^(n-1) vertices; n=2 is the truncated tetrahedron).  Combinatorial
   only; the default 2-D layout is Graph's -- pass VertexCoordinates / GraphLayout (or
   wrap in Graph3D) for the tetrahedral picture. *)

SierpinskiGraph[ n_Integer, opts : OptionsPattern[ Graph ] ] :=
  Graph[ Nest[ triangleReplace, CompleteGraph[ 4 ], n - 1 ], opts ]

triangleReplace[ g_ ] :=
  Graph[
    Join[
      Flatten @ Table[ UndirectedEdge[ { v, i }, { v, j } ], { v, VertexList @ g }, { i, 1, 2 }, { j, i + 1, 3 } ],
      ( e |-> UndirectedEdge[
          { e[[ 1 ]], First @ FirstPosition[ AdjacencyList[ g, e[[ 1 ]] ], e[[ 2 ]] ] },
          { e[[ 2 ]], First @ FirstPosition[ AdjacencyList[ g, e[[ 2 ]] ], e[[ 1 ]] ] } ] ) /@ EdgeList @ g ]
  ]


(* ===================== Bethe lattice ===================== *)

(* BetheGraph[n, z] is the finite Bethe lattice / Cayley tree of n shells and coordination
   number z (argument order matching CompleteKaryTree[n, k]): the root branches z ways,
   every other internal node branches z-1 (its remaining edge goes to its parent), so all
   interior vertices are z-valent and only the depth-n boundary is 1-valent.  Distinct
   from the rooted, irregular CompleteKaryTree (root degree k, internal degree k+1).
   Atomic root (NestGraph reads an empty-list seed as an empty vertex set). *)

BetheGraph[ n_Integer, z_Integer, opts : OptionsPattern[ Graph ] ] :=
  NestGraph[ w |-> If[ w === 0, List /@ Range @ z, Append[ w, # ] & /@ Range[ z - 1 ] ], 0, n, opts, DirectedEdges -> False ]


(* ===================== Spherically symmetric tree ===================== *)

(* BranchingSequenceTree[b] is the spherically symmetric (radially homogeneous) rooted
   tree whose offspring count depends only on depth: a vertex at depth l has b[[l+1]]
   children, so all vertices at a given depth share the same degree.  Length[b]+1 levels;
   FoldList[Times, 1, b] vertices per shell.  Constant b is CompleteKaryTree; the
   coordination-fixed cousin is BetheGraph.  Vertices are {depth, position} pairs. *)

BranchingSequenceTree[ b_List, opts : OptionsPattern[ Graph ] ] :=
  NestGraph[
    With[ { l = First @ #, p = Last @ #, c = b[[ First @ # + 1 ]] },
      { l + 1, ( p - 1 ) c + # } & /@ Range @ c ] &,
    { { 0, 1 } }, Length @ b, opts, DirectedEdges -> False ]


(* ===================== Example substrates ===================== *)

(* ExampleGraphData[name, size] is the named example substrate at tier "Small" | "Medium" |
   "Large"; either slot also takes a raw value (ExampleGraphData["Square", 11]).  A patch models
   an *open* subset of the plane, so it necessarily has a rim: pendant vertices -- the one real
   artifact, a vertex with a single neighbour models nothing in the plane -- are trimmed away, and
   constructions belong in a ball around the centre, clear of the rim.  Coordinates are kept, since
   the tiling embedding is what makes a figure legible; the metric layer never reads them. *)

exampleGraphTiers = <|
  "Plane"          -> <| "Small" -> 0.02, "Medium" -> 0.005, "Large" -> 0.001 |>,
  "Triangular"     -> <| "Small" -> 6, "Medium" -> 9, "Large" -> 12 |>,
  "Square"         -> <| "Small" -> 6, "Medium" -> 9, "Large" -> 12 |>,
  "Hexagonal"      -> <| "Small" -> 6, "Medium" -> 9, "Large" -> 12 |>,
  "InflatedSquare" -> <| "Small" -> 6, "Medium" -> 9, "Large" -> 12 |>
|>;

(* $ExampleGraphs is the frozen table of substrates shipped with the paclet, keyed by name and
   tier.  A named tier reads from it rather than rebuilding, so a figure is reproducible
   independently of DiscretizeRegion's random triangulation and of layout changes between Wolfram
   versions; a raw value in either slot rebuilds live. *)

$ExampleGraphs := Dataset @ exampleGraphAssets

exampleGraphAssets := exampleGraphAssets =
  Import[ PacletObject[ "WolframInstitute/Infrageometry" ][ "AssetLocation", "ExampleGraphs" ], "WXF" ]

ExampleGraphData[ ] := Keys @ exampleGraphTiers

ExampleGraphData[ name_String ] :=
  AssociationMap[ ExampleGraphData[ name, # ] &, Keys @ exampleGraphTiers @ name ]

ExampleGraphData[ name_String, size_String, opts : OptionsPattern[ Graph ] ] :=
  Graph[ exampleGraphAssets[ name, size ], opts ]

ExampleGraphData[ "Plane", cellMeasure_?NumericQ, opts : OptionsPattern[ Graph ] ] :=
  Graph[
    trimPendants @ InteriorMeshGraph @ DiscretizeRegion[
      Rectangle[ ], MaxCellMeasure -> cellMeasure, PrecisionGoal -> Infinity ],
    opts ]

ExampleGraphData[ name : "Triangular" | "Square" | "Hexagonal", r_Integer, opts : OptionsPattern[ Graph ] ] :=
  Graph[ trimPendants @ TessellationNeighborhoodGraph[ tilingSymbol @ name, r ], opts ]

ExampleGraphData[ "InflatedSquare", r_Integer, opts : OptionsPattern[ Graph ] ] :=
  Graph[ InflateGraph @ ExampleGraphData[ "Square", r ], opts ]

tilingSymbol[ name_ ] := First @ Keys @ Select[ shapeName, # === name & ]

(* trimPendants[g] repeatedly deletes degree-1 vertices, the fixed point being the 2-core-with-
   branches: a patch of a tiling has a few corner vertices left hanging by one edge, and those are
   the only rim vertices that misrepresent the plane. *)
trimPendants[ g_Graph ] :=
  FixedPoint[ h |-> VertexDelete[ h, Pick[ VertexList @ h, VertexDegree @ h, 1 ] ], g ]


(* ===================== Ambient styles ===================== *)

(* AmbientGraphStyle[name] is the Graph option list rendering a substrate as a backdrop, so that a
   highlighted construction drawn on top of it stands out; splice it in with
   Graph[g, Sequence @@ AmbientGraphStyle["GrayFaint"]]. *)

ambientGraphStyles = <|
  "Default"    -> { },
  "GrayFaint"  -> { EdgeStyle -> Directive[ StandardGray, Opacity[ 0.15 ] ], VertexStyle -> Directive[ StandardGray, Opacity[ 0.3 ] ] },
  "GrayOpaque" -> { EdgeStyle -> Directive[ StandardGray, Opacity[ 0.4 ] ], VertexStyle -> Directive[ StandardGray, Opacity[ 0.6 ] ] },
  "Gray"       -> { EdgeStyle -> StandardGray, VertexStyle -> StandardGray }
|>;

AmbientGraphStyle[ ] := Keys @ ambientGraphStyles

AmbientGraphStyle[ name_String ] := ambientGraphStyles @ name


(* ===================== Inflation ===================== *)

(* InflateGraph[g] grows a fiber of extra vertices over each vertex of g: every new vertex is joined
   to its base vertex, fibers get "ExtraEdges" internal edges, and random edges are added between
   fibers whose base vertices lie within "Radius" in g.  The base survives as the induced subgraph
   on VertexList[g], so g is recoverable and the perturbation only adds local dimensional noise.
   Each option takes a constant or a {min, max} range sampled per base vertex. *)

Options[ InflateGraph ] = {
  "ExtraVertices" -> { 0, 2 },
  "ExtraEdges"    -> 0,
  "Radius"        -> 1,
  "Density"       -> 1
};

InflateGraph[ g_Graph, opts : OptionsPattern[ { InflateGraph, Graph } ] ] :=
  With[
    { radius = inflationSample @ OptionValue[ InflateGraph, { opts }, "Radius" ],
      density = OptionValue[ InflateGraph, { opts }, "Density" ],
      innerSpec = OptionValue[ InflateGraph, { opts }, "ExtraEdges" ],
      extraSpec = OptionValue[ InflateGraph, { opts }, "ExtraVertices" ] },
    { fibers = AssociationMap[
        v |-> Array[ InflatedVertex[ v, # ] &, inflationSample @ extraSpec ],
        VertexList @ g ] },
    Graph[
      EdgeAdd[ g,
        Join[
          Catenate @ KeyValueMap[ { v, fiber } |-> ( UndirectedEdge[ v, # ] & /@ fiber ), fibers ],
          Catenate @ Map[ fiber |-> inflationFiberEdges[ fiber, inflationSample @ innerSpec ], Values @ fibers ],
          Catenate @ Map[ v |-> inflationCrossEdges[ g, fibers, v, radius, density ], VertexList @ g ] ] ],
      Sequence @@ FilterRules[ { opts }, Options @ Graph ],
      VertexCoordinates -> inflationCoordinates[ g, fibers ] ]
  ]

inflationSample[ n_?NumericQ ] := Round @ n

inflationSample[ { a_, b_ } ] := RandomInteger[ { Round @ a, Round @ b } ]

(* fiber vertices are scattered close to their base vertex, at a fraction of the mean edge length,
   so the inflated graph still reads as a thickened patch of the plane rather than as spikes flung
   outward by the layout engine *)
inflationCoordinates[ g_, fibers_ ] :=
  With[ { coords = AssociationThread[ VertexList @ g, GraphEmbedding @ g ] },
    { scale = 0.3 Mean[ EuclideanDistance @@ Lookup[ coords, List @@ # ] & /@ EdgeList @ g ] },
    Normal @ Join[
      coords,
      Association @ Catenate @ KeyValueMap[
        { v, fiber } |-> ( # -> coords[ v ] + RandomPoint @ Disk[ { 0, 0 }, scale ] & /@ fiber ),
        fibers ] ]
  ]

inflationFiberEdges[ fiber_, m_ ] :=
  With[ { pairs = Subsets[ fiber, { 2 } ] },
    UndirectedEdge @@@ RandomSample[ pairs, Min[ m, Length @ pairs ] ] ]

(* edges from v's fiber out to the fibers within radius: the count scales with how much fiber is in
   reach per base vertex in reach, so density is a per-neighbour rate rather than a raw edge count *)
inflationCrossEdges[ g_, fibers_, v_, radius_, density_ ] :=
  With[ { near = VertexList @ NeighborhoodGraph[ g, v, radius ] },
    { reach = Catenate @ Lookup[ fibers, near ] },
    { candidates = DeleteCases[ Tuples[ { fibers @ v, Join[ reach, near ] } ], { x_, x_ } ] },
    UndirectedEdge @@@ RandomSample[
      candidates,
      Min[ RandomInteger @ Round[ density Length[ reach ] / Max[ Length @ near, 1 ] ], Length @ candidates ] ]
  ]
