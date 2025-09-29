Package["WolframInstitute`Infrageometry`"]


PackageExport[GraphEdgeWeights]
PackageExport[GraphVertexWeights]
PackageExport[FormanRicciCurvature]
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

FormanRicciCurvature[g_Graph] := With[{
	vws = Thread[VertexList[g] -> GraphVertexWeights[g]],
	ews = Thread[EdgeList[g] -> GraphEdgeWeights[g]]
},
	Table[
		e -> With[{
			vs = List @@ e[[;; 2]],
			ew = Lookup[ews, Key[e]]
		},
			Sum[
				With[{
					vw = Lookup[vws, Key[v]],
					vew = Lookup[ews, DeleteCases[IncidenceList[g, v, 1], e]]
				},
					vw * (1 - Total[1 / Sqrt[vew]] / Sqrt[ew])
				],
				{v, vs}
			]
		],
		{e, EdgeList[g]}
	]
]


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

