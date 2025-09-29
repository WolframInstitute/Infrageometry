Package["WolframInstitute`Infrageometry`"]


PackageExport[SymmetricRelationGraph]
PackageExport[SpatialReconstruction]
PackageExport[CausalGraphSimpleChains]
PackageExport[CoordinatizeCausalGraph]
PackageExport[CoordinatizedCausalGraph]



SymmetricRelationGraph[f_, v_Association, opts___] :=
	Graph[Keys[v], UndirectedEdge @@@ Pick[Subsets[Keys[v], {2}], f @@@ Subsets[Values[v], {2}]], opts]


SpatialReconstruction[g_Graph, slice_List, n_Integer : 1, opts___] :=
	SymmetricRelationGraph[
		IntersectingQ,
		AssociationMap[VertexInComponent[g, #, {n}] &, slice],
		opts,
		DirectedEdges -> False,
		VertexStyle -> ResourceFunction["WolframPhysicsProjectStyleData"]["CausalGraph", "VertexStyle"],
		EdgeStyle -> Blend[{
			First[ResourceFunction["WolframPhysicsProjectStyleData"]["SpatialGraph", "EdgeLineStyle"]],
			ResourceFunction["WolframPhysicsProjectStyleData"]["BranchialGraph", "EdgeStyle"]}
		]
	]

CausalGraphSimpleChains[g_Graph] :=
	ReverseSortBy[Length] @ DeleteCases[{}] @ Association @ Outer[
		{##} -> FindPath[g, ##, Infinity, Infinity] &,
		Pick[VertexList[g], VertexInDegree[g], 0],
		Pick[VertexList[g], VertexOutDegree[g], 0],
		1
	]


CoordinatizeCausalGraph[g_Graph, observers : {_List, _List}] := With[{
	ts = Map[First] @* PositionIndex /@ observers
},
	Association @ Catenate @ KeyValueMap[
		{ts, vs} |-> With[{coordinates = {Subtract @@ ts, Plus @@ ts}},
			# -> coordinates & /@ vs
		],
		KeyMap[
			MapThread[Lookup, {ts, #}] &, 
			ResourceFunction["LowestCommonAncestors"][ReverseGraph[g], Tuples[observers]]
		]
	]
]

CoordinatizeCausalGraph[g_Graph, observers : {__List}] :=
	Merge[KeyUnion[CoordinatizeCausalGraph[g, #] & /@ Subsets[observers, {2}]], Identity]


CoordinatizedCausalGraph[g_Graph, observers : {_List, _List}, opts___] := With[{
	coordinates = CoordinatizeCausalGraph[g, observers]
},
	HighlightGraph[
		TransitiveReductionGraph @ Subgraph[
			TransitiveClosureGraph[g],
			Keys[coordinates],
			VertexCoordinates -> Normal[{1, -1} / 10 # & /@ coordinates],
			VertexSize -> {"Scaled", 0.01}
		],
		Style[Subgraph[g, #], Thick, Green] & /@ observers,
		opts
	]
]