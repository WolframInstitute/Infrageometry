Package["WolframInstitute`Infrageometry`"]

PackageExport[PunchHole]
PackageExport[TorusTessellation]


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
