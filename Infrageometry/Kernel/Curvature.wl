Package["WolframInstitute`Infrageometry`"]

PackageExport[FormanRicciCurvature]


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
