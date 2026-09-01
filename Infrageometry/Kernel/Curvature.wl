Package["WolframInstitute`Infrageometry`"]

PackageExport[FormanRicciCurvature]
PackageExport[OllivierRicciCurvature]
PackageExport[EffectiveResistance]
PackageExport[ResistanceQ]
PackageScope[wasserstein1]


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
