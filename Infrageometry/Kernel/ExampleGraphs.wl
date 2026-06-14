Package["WolframInstitute`Infrageometry`"]

PackageExport[PunchHole]
PackageScope[TorusTessellation]

PackageExport[SierpinskiGraph]
PackageExport[BetheGraph]
PackageExport[BranchingSequenceTree]
PackageScope[triangleReplace]


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
