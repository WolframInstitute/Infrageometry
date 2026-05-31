Package["WolframInstitute`Infrageometry`"]


PackageExport[SymmetricRelationGraph]
PackageExport[GraphEdgeWeights]
PackageExport[GraphVertexWeights]
PackageExport[GraphBoundary]
PackageExport[GraphInterior]

PackageExport[FormanRicciCurvature]
PackageExport[OllivierRicciCurvature]
PackageExport[WolframRicciCurvature]
PackageExport[WolframDimension]
PackageExport[SectionalCurvatures]
PackageScope[wasserstein1]

PackageExport[EffectiveResistance]
PackageExport[ResistanceQ]

PackageExport[SpacetimeGraph]
PackageExport[SpacetimeTorusGraph]
PackageExport[RotateEdge]
PackageExport[KickEdge]
PackageExport[PersistEdge]
PackageExport[RightMatrix]
PackageExport[ToroidalRightMatrix]
PackageExport[ToroidalLeftMatrix]
PackageExport[DiracWalk]
PackageExport[VertexAmplitudes]
PackageExport[GraphSuspension]
PackageExport[RandomGraphAutomorphism]



SymmetricRelationGraph[f_, v_Association, opts___] :=
	Graph[Keys[v], UndirectedEdge @@@ Pick[Subsets[Keys[v], {2}], f @@@ Subsets[Values[v], {2}]], opts]


GraphEdgeWeights[g_Graph] := Replace[
	Replace[
		AnnotationValue[g, EdgeWeight],
		Automatic -> ConstantArray[1, EdgeCount[g]]
	],
	Automatic -> 1,
	1
]

GraphVertexWeights[g_Graph] := Replace[
	Replace[
		AnnotationValue[g, VertexWeight],
		Automatic -> ConstantArray[1, VertexCount[g]]
	],
	Automatic -> 1,
	1
]

(* ===================== Inner vertex boundary / interior ===================== *)

(* GraphBoundary[g, S]: inner vertex boundary {v in S : v has a neighbor outside S}. *)
GraphBoundary[g_Graph, subgraph_Graph] :=
	GraphBoundary[g, VertexList[subgraph]]

GraphBoundary[g_Graph, subset_List] :=
	With[{outside = Complement[VertexList[g], subset]},
		Select[subset, IntersectingQ[AdjacencyList[g, #], outside] &]
	]

(* GraphInterior[g, S]: {v in S : all neighbors of v lie in S} = S \ GraphBoundary[g, S]. *)
GraphInterior[g_Graph, subgraph_Graph] :=
	GraphInterior[g, VertexList[subgraph]]

GraphInterior[g_Graph, subset_List] :=
	Complement[subset, GraphBoundary[g, subset]]

(* ===================== Forman-Ricci curvature ===================== *)

(* Forman's combinatorial Ricci curvature on the clique complex of a graph.
   For a p-cell alpha (a (p+1)-clique encoded as a sorted vertex list):

       F(alpha) = w(alpha) [   Sum_{beta > alpha} w(alpha)/w(beta)
                             + Sum_{gamma < alpha} w(gamma)/w(alpha)
                             - Sum_{aHat || alpha}
                                 | Sum_{beta > alpha, beta > aHat} Sqrt[w(alpha) w(aHat)] / w(beta)
                                 - Sum_{gamma < alpha, gamma < aHat} Sqrt[w(alpha) w(aHat)] / w(gamma) | ]

   where beta > alpha denotes a (p+1)-coface, gamma < alpha a (p-1)-subface,
   and aHat || alpha (parallel) means: aHat is a distinct p-cell sharing
   either a coface or a subface with alpha but not both (Forman's strong
   parallelism).  Higher-cell weights default to 1.

   Options:
     "OnCells" -> p_Integer | All | {p1, p2, ...}      output cell dimension(s)
     "MaxCellDimension" -> Automatic | k_Integer       clique-complex truncation;
                                                       Automatic = Max[OnCells] + 1
                                                       (or full clique complex when All).

   Output keys:
     p = 0  ->  vertices
     p = 1  ->  UndirectedEdge[u, v] (in EdgeList[g] order, for backward
                compatibility with edge-keyed consumers)
     p >= 2 ->  sorted vertex lists {v1, ..., v_{p+1}}
   Single-integer "OnCells"     ->  flat Association[cell -> kappa].
   List or All "OnCells"        ->  Association[p -> Association[cell -> kappa]]. *)

Options[FormanRicciCurvature] = {"OnCells" -> 1, "MaxCellDimension" -> Automatic};

FormanRicciCurvature[g_Graph, OptionsPattern[]] := Module[{
	onCells, dMax, complex, byDim, vw, ew, weightAt, formanAt,
	outputDims, dimResult, upMap
},
	onCells = OptionValue["OnCells"];
	dMax = Replace[OptionValue["MaxCellDimension"],
		Automatic :> If[onCells === All,
			ComplexDimension @ GraphComplex[g],
			Max[Flatten[{onCells}]] + 1
		]
	];
	outputDims = Switch[onCells,
		All,      Range[0, dMax],
		_List,    onCells,
		_Integer, {onCells}
	];
	complex = GraphComplex[g, dMax + 1];
	byDim = AssociationMap[SimplexList[complex, {#}] &, Range[0, dMax]];

	(* upMap[q][gamma] = list of (q+1)-cells containing gamma; one pass over (q+1)-cells.
	   Replaces O(|cells_p|^2) Subset-scans inside formanAt. *)
	upMap = AssociationMap[
		q |-> Module[{acc = <||>},
			Do[
				(acc[#] = If[KeyExistsQ[acc, #], Append[acc[#], beta], {beta}]) & /@
					Subsets[beta, {q + 1}],
				{beta, byDim[q + 1]}
			];
			acc
		],
		Range[0, dMax - 1]
	];

	vw = AssociationThread[VertexList[g] -> GraphVertexWeights[g]];
	ew = AssociationThread[Sort /@ List @@@ EdgeList[g] -> GraphEdgeWeights[g]];

	weightAt[cell_List] := Which[
		Length[cell] == 1, vw[First[cell]],
		Length[cell] == 2, ew[Sort @ cell],
		True,              1
	];

	formanAt[alpha_List] := With[{p = Length[alpha] - 1, wA = weightAt[alpha]},
		With[{
			cofaces = If[p + 1 <= dMax, Lookup[upMap[p], Key @ alpha, {}], {}],
			subfaces = If[p >= 1, SimplexBoundary[alpha], {}]
		},
			With[{
				cofShared = If[cofaces === {}, {},
					DeleteCases[Union @@ Map[SimplexBoundary, cofaces], alpha]
				],
				subfShared = If[subfaces === {}, {},
					DeleteCases[
						Union @@ Map[gamma |-> Lookup[upMap[p - 1], Key @ gamma, {}], subfaces],
						alpha
					]
				]
			},
				With[{
					parallels = Complement[
						Union[cofShared, subfShared],
						Intersection[cofShared, subfShared]
					]
				},
					wA (
						Total[(beta |-> wA / weightAt[beta]) /@ cofaces]
						+ Total[(gamma |-> weightAt[gamma] / wA) /@ subfaces]
						- Total[
							(aHat |-> With[{sqrtW = Sqrt[wA * weightAt[aHat]]},
								Abs[
									Total[(beta |-> sqrtW / weightAt[beta]) /@
										Select[cofaces, SubsetQ[#, aHat] &]]
									- Total[(gamma |-> sqrtW / weightAt[gamma]) /@
										Intersection[subfaces, SimplexBoundary[aHat]]]
								]
							]) /@ parallels
						]
					)
				]
			]
		]
	];

	dimResult[p_] := Switch[p,
		0, AssociationThread[Catenate @ byDim[0] -> formanAt /@ byDim[0]],
		1, With[{kappaBySorted = AssociationThread[byDim[1] -> formanAt /@ byDim[1]]},
			AssociationMap[edge |-> kappaBySorted[Sort[List @@ edge]], EdgeList[g]]
		],
		_, AssociationThread[byDim[p] -> formanAt /@ byDim[p]]
	];

	If[IntegerQ[onCells],
		dimResult[onCells],
		AssociationMap[dimResult, outputDims]
	]
]


(* ===================== Ollivier-Ricci curvature ===================== *)

(* kappa(u, v) = 1 - W_1(mu_u, mu_v) / d(u, v),
   mu_x = uniform on the open neighborhood N(x); idleness alpha = 0;
   W_1 is the Wasserstein-1 (Earth-Mover) distance under graph distance,
   solved as a transport LP via LinearOptimization. *)

OllivierRicciCurvature[g_Graph] := Module[{vs, idx, adj, dist},
	vs   = VertexList[g];
	idx  = AssociationThread[vs, Range @ Length @ vs];
	adj  = AssociationMap[AdjacencyList[g, #] &, vs];
	dist = GraphDistanceMatrix[g];
	AssociationMap[
		e |-> With[{nu = adj[e[[1]]], nv = adj[e[[2]]]},
			1 - wasserstein1[
				ConstantArray[1.0 / Length[nu], Length[nu]],
				ConstantArray[1.0 / Length[nv], Length[nv]],
				dist[[idx /@ nu, idx /@ nv]]
			] / dist[[idx[e[[1]]], idx[e[[2]]]]]
		],
		EdgeList[g]
	]
]


(* Wasserstein-1 distance between probability vectors mu, nu on finite
   point sets given the m-by-n cost matrix.  Solved as a transport LP. *)

wasserstein1[mu_List, nu_List, costs_List] := Module[{m = Length[mu], n = Length[nu], vars},
	vars = Array[t, {m, n}];
	LinearOptimization[
		Total[Flatten[costs * vars]],
		Join[
			Table[Total[vars[[i, All]]] == mu[[i]], {i, m}],
			Table[Total[vars[[All, j]]] == nu[[j]], {j, n}],
			Thread[Flatten[vars] >= 0]
		],
		Flatten[vars],
		"PrimalMinimumValue"
	]
]


(* ===================== Wolfram-Ricci scalar curvature ===================== *)

(* Volume-comparison Ricci scalar at vertex v and integer radius r:
       R(v, r) = 6 (d + 2) / r^2 (1 - V(r) / V_E(d, r)),
       V_E(d, r) = pi^(d/2) r^d / Gamma[d/2 + 1],
   with V(r) = |B_r(v)|.  Local dimension d is supplied via "Dimension" -> d
   or read off from the log-difference (Log V(r+1) - Log V(r)) / (Log(r+1) - Log r)
   when "Dimension" -> Automatic (default; caps the per-vertex valid radius
   range at eccentricity(v) - 1 since V(r+1) must exist).

   WolframRicciCurvature[g]                  -> Association[v -> mean_r R(v, r)] over r = 1..ecc(v)
   WolframRicciCurvature[g, {rmin, rmax}]    -> averages over [rmin, rmax] cap valid range
   WolframRicciCurvature[g, r_Integer]       -> Association[v -> R(v, r)], no averaging.
   Vertices whose valid range is empty -> Indeterminate. *)

Options[WolframRicciCurvature] = {"Dimension" -> Automatic};

WolframRicciCurvature[g_Graph,
	range : (_Integer | {_Integer, _Integer} | All) : All,
	OptionsPattern[]
] := With[{dim = OptionValue["Dimension"]},
	AssociationMap[v |-> wolframRicciAtVertex[g, v, range, dim], VertexList[g]]
]


(* Per-vertex helper: builds V(r) by accumulating distance counts, picks the
   valid radius window (capped at ecc(v), or ecc(v) - 1 in Automatic dim mode),
   and returns Mean over that window of the volume-comparison scalar.  Empty
   window -> Indeterminate. *)

wolframRicciAtVertex[g_Graph, v_, range_, dim_] := Module[{vols, top, rs},
	vols = With[{c = KeySort @ Counts @ DeleteCases[GraphDistance[g, v], Infinity]},
		AssociationThread[Keys[c] -> Accumulate[Values[c]]]
	];
	top = Max[Keys[vols]] - Boole[dim === Automatic];
	rs = Switch[range,
		All,                  Range[1, top],
		_Integer,             If[1 <= range <= top, {range}, {}],
		{_Integer, _Integer}, Range[Max[1, range[[1]]], Min[top, range[[2]]]]
	];
	If[rs === {},
		Indeterminate,
		Mean[(r |-> With[{
				dr = If[dim === Automatic,
					N[(Log[vols[r + 1]] - Log[vols[r]]) / (Log[r + 1] - Log[r])],
					dim
				],
				vr = vols[r]
			},
			N[6 (dr + 2) / r^2 (1 - vr Gamma[dr / 2 + 1] / (Pi^(dr / 2) r^dr))]
		]) /@ rs]
	]
]


(* Volume-growth local dimension at vertex v and integer radius r:
       d(v, r) = (Log V(r+1) - Log V(r)) / (Log(r+1) - Log r),
   with V(r) = |B_r(v)|.  The same estimate used internally by
   WolframRicciCurvature[..., "Dimension" -> Automatic], exposed on its own
   with the matching calling convention.

   WolframDimension[g]               -> Association[v -> mean_r d(v, r)] over r = 1..ecc(v) - 1
   WolframDimension[g, {rmin, rmax}] -> averages over [rmin, rmax] cap valid range
   WolframDimension[g, r_Integer]    -> Association[v -> d(v, r)], no averaging.
   Vertices whose valid range is empty -> Indeterminate. *)

WolframDimension[g_Graph,
	range : (_Integer | {_Integer, _Integer} | All) : All
] := AssociationMap[v |-> wolframDimensionAtVertex[g, v, range], VertexList[g]]


wolframDimensionAtVertex[g_Graph, v_, range_] := Module[{vols, top, rs},
	vols = With[{c = KeySort @ Counts @ DeleteCases[GraphDistance[g, v], Infinity]},
		AssociationThread[Keys[c] -> Accumulate[Values[c]]]
	];
	top = Max[Keys[vols]] - 1;
	rs = Switch[range,
		All,                  Range[1, top],
		_Integer,             If[1 <= range <= top, {range}, {}],
		{_Integer, _Integer}, Range[Max[1, range[[1]]], Min[top, range[[2]]]]
	];
	If[rs === {},
		Indeterminate,
		Mean[(r |-> N[(Log[vols[r + 1]] - Log[vols[r]]) / (Log[r + 1] - Log[r])]) /@ rs]
	]
]


SectionalCurvatures[g_Graph] := With[{c = GraphComplex[g], deg = VertexDegree[g], vs = PositionIndex[VertexList[g]]},
    Total[Total[1 / Extract[deg, Lookup[vs, #]]] - 1 / 2 & /@ SimplexList[SimplexStar[c, {#}], {2}]] / 3 & /@ Keys[vs]
]


(* ===================== Effective resistance ===================== *)

(* Klein-Randic resistance distance R(u, v) = (e_u - e_v)^T L^+ (e_u - e_v),
   where L^+ is the Moore-Penrose pseudoinverse of the graph Laplacian
   (= the 0-block of GreenOperatorMatrix[GraphComplex[g]]).  Three forms:
   pair, full V x V matrix, submatrix on a vertex list. *)

EffectiveResistance[g_Graph] := Module[{lp, n, d},
    lp = PseudoInverse[N @ Normal @ KirchhoffMatrix[g]];
    n = VertexCount[g];
    d = Diagonal[lp];
    Table[d[[i]] + d[[j]] - 2 lp[[i, j]], {i, n}, {j, n}]
]

EffectiveResistance[g_Graph, u_, v_] /; MemberQ[VertexList[g], u] && MemberQ[VertexList[g], v] :=
    With[{lp = PseudoInverse[N @ Normal @ KirchhoffMatrix[g]],
          idx = AssociationThread[VertexList[g], Range @ VertexCount[g]]},
        With[{i = idx[u], j = idx[v]},
            lp[[i, i]] + lp[[j, j]] - 2 lp[[i, j]]
        ]
    ]

EffectiveResistance[g_Graph, vs_List] /; SubsetQ[VertexList[g], vs] :=
    With[{full = EffectiveResistance[g],
          ix = AssociationThread[VertexList[g], Range @ VertexCount[g]] /@ vs},
        full[[ix, ix]]
    ]


(* Klein-Randic / Schoenberg negative-type predicate: a real symmetric
   n x n matrix R with zero diagonal is realisable as a resistance
   distance matrix iff the centred Gram matrix
       B[i, j] = (R[1, j] + R[i, 1] - R[i, j]) / 2,  i, j >= 2
   is positive semidefinite. *)

ResistanceQ[r_ ? MatrixQ] :=
    SquareMatrixQ[r] &&
    (Transpose[r] === r || N @ Transpose[r] == N @ r) &&
    AllTrue[Diagonal[r], # == 0 &] &&
    With[{n = Length[r]},
        n <= 1 || AllTrue[
            Eigenvalues[N @ Table[(r[[1, j]] + r[[i, 1]] - r[[i, j]]) / 2, {i, 2, n}, {j, 2, n}]],
            # >= -10^-9 &
        ]
    ]

ResistanceQ[_] := False



SpacetimeGraph[{m_Integer, n_Integer}, opts___] := Block[{
	vs = Join[
		2 Tuples[{Range[m], Range[n]}] - 1,
		2 Tuples[{Range[m - 1], Range[n - 1]}]
	]
},
	RelationGraph[
		#2[[2]] - #1[[2]] == 1 && Abs[#2[[1]] - #1[[1]]] == 1 &,
		vs,
		opts,
		VertexCoordinates -> vs,
		DirectedEdges -> True
	]
]

SpacetimeTorusGraph[{m_Integer, n_Integer}, opts___] := Block[{
	vs = Join[
		2 Tuples[{Range[m], Range[n]}] - 1,
		2 Tuples[{Range[m], Range[n]}]
	]
},
	RelationGraph[
		Mod[#2[[2]] - #1[[2]], 2 n, 1] == 1 && MatchQ[Abs[#2[[1]] - #1[[1]]], 1 | 2 m - 1] &,
		vs,
		opts,
		VertexCoordinates -> vs,
		DirectedEdges -> True
	]
]

RotateEdge[DirectedEdge[i_, j_], {m_Integer : 1, n_Integer : 1}] :=
	DirectedEdge[i,
		MapThread[Mod[##, 1] &,
			{i + Cross[{If[Abs[j[[1]] - i[[1]]] > 1, -1, 1],
			If[Abs[i[[2]] - j[[2]]] > 1, -1, 1]} * Sign[j - i]],
			2 {m, n}}
		]
	]

KickEdge[DirectedEdge[i_, j_]] := If[i[[1]] < j[[1]],
	DirectedEdge[j, j + Cross[j - i], 1],
	DirectedEdge[j, j - Cross[j - i], -1]
]

PersistEdge[DirectedEdge[i_, j_]] := DirectedEdge[j, j + j - i]

RightMatrix[g_Graph, {m_Integer : 1, n_Integer : 1}] := With[{index = First /@ PositionIndex[EdgeList[g]]},
	SparseArray[
		MapIndexed[
			Enclose[Prepend[#2, Confirm @ Lookup[index, DirectedEdge[#, MapThread[Mod[##, 1] &, {# + {1, 1}, 2 {m, n}}]]]] -> 1, Nothing &] &,
			VertexList[g]
		],
		{Length[index], VertexCount[g]}
	]
]

ToroidalRightMatrix[g_Graph] := Block[{
    index = First /@ PositionIndex[EdgeList[g]], edges, weights
},
    edges = 
        Lookup[index, {DirectedEdge[#, # + {1, 1}], DirectedEdge[#, # + {-5, 1}]}, Nothing] & /@ 
            VertexList[g] // Flatten;
    weights = SparseArray[
        Table[{#, i} -> 1, {i, VertexCount[g]}] & /@ edges // Flatten,
        {Length[index], VertexCount[g]}
    ];
    weights Ramp[- Transpose@IncidenceMatrix[g]]
]

ToroidalLeftMatrix[g_Graph] := Block[{
    index = First /@ PositionIndex[EdgeList[g]], edges, weights
},
    edges = 
        Lookup[index, {DirectedEdge[#, # + {-1, 1}],DirectedEdge[#, # + {5, 1}]}, Nothing] & /@
            VertexList[g] // Flatten;
    weights = SparseArray[
        Table[{#, i} -> 1, {i, VertexCount[g]}] & /@ edges // Flatten,
        {Length[index], VertexCount[g]}
    ];
    weights Ramp[- Transpose@IncidenceMatrix[g]]
]

DiracWalk[g_Graph, p_ : 1 / 2] := With[{index = First /@ PositionIndex[EdgeList[g]]},
	SparseArray[
		Map[
			Enclose[
				With[{
					j1 = #[[3]] -> Confirm @ Lookup[index, #[[;; 2]]] & @ KickEdge[#],
					j2 = Confirm @ Lookup[index, PersistEdge[#]],
					i = Lookup[index, #]},
					Splice[{
						{j1[[2]], i} -> j1[[1]] * Sqrt[p],
						{j2, i} -> Sqrt[1 - p]
					}]
				],
				Nothing &
			] &,
			Keys[index]
		],
		{1, 1} * Length[index]
	]
]

VertexAmplitudes[g_Graph, edgeWeights_, {m_Integer : 1, n_Integer : 1}] := Map[
	# -> Chop[
		{1, I} . Lookup[
			edgeWeights,
			NestList[
				RotateEdge[#, {m, n}] &,
				DirectedEdge[#, MapThread[Mod[##, 1] &, {# + {1, 1}, 2 {m, n}}]],
				1
			],
			0
		]
	] &,
	VertexList[g]
]


GraphSuspension[g_ ? GraphQ] := With[{v1 = Unique[\[FormalV]], v2 = Unique[\[FormalV]]},
	Graph3D[EdgeAdd[g, Catenate[{UndirectedEdge[v1, #], UndirectedEdge[#, v2]} & /@ VertexList[g]]]]
]


RandomGraphAutomorphism[g_ ? GraphQ, n : _Integer | Automatic | All : Automatic] :=
	With[{gr = GraphAutomorphismGroup[g]}, {order = GroupOrder[gr]},
		GroupElements[gr, RandomSample[;; order, UpTo[Replace[n, {Automatic -> 1, All -> order}]]]] //
			If[n === Automatic, First, Identity]
	]

RandomGraphAutomorphism[g : {___List}, args___] := RandomGraphAutomorphism[ComplexGraph[g], args]

