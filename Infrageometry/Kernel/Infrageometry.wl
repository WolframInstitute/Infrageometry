Package["WolframInstitute`Infrageometry`"]


PackageExport[NormalComplex]

PackageExport[SimplexList]
PackageExport[SimplexStar]
PackageExport[SimplexCore]
PackageExport[SimplexOrder]
PackageExport[SimplexSign]
PackageExport[SimplicialMap]

PackageExport[ComplexEulerCharacteristic]
PackageExport[ComplexFermiCharacteristic]

PackageExport[GraphComplex]
PackageExport[FaceGraph]
PackageExport[BarycentricRefinement]

PackageExport[GraphTopology]

PackageExport[GraphSuspension]
PackageExport[RandomGraphAutomorphism]


PackageExport[ConnectionMatrix]
PackageExport[GreenFunctionMatrix]



ClearAll["WolframInstitute`Infrageometry`**`*", "WolframInstitute`Infrageometry`*"]



NormalComplex[g : {___List}] := Union[Map[Union, g]]


SimplexList[g : {___List}, k_Integer] := Select[g, Length[#] == k &]

SimplexStar[g : {___List}, x_List] := Select[g, SubsetQ[#, x] &]

SimplexCore[g : {___List}, x_List] := Select[g, SubsetQ[x, #] &]

SimplexOrder[x_List, y_List] := With[{c = Complement[x, y]},
	If[Length[c] == 1, Signature[Prepend[y, First[c]]] * Signature[x], 0]
]

SimplexSign[x_List] := - (-1) ^ Length[x]

SimplicialMap[g : {___List}, perm_Cycles] :=
	With[{vs = Catenate[SimplexList[g, 1]]}, {rules = Thread[vs -> Permute[vs, perm]]},
		Function[Replace[#, rules, 1]]
	]

ComplexEulerCharacteristic[g : {___List}] := Plus @@ Map[SimplexSign, g]

ComplexFermiCharacteristic[g : {___List}] := Times @@ Map[SimplexSign, g]


GraphComplex[g_ ? GraphQ, k : _Integer | Infinity : Infinity] := GraphComplex[g, {1, k}]

GraphComplex[g_ ? GraphQ, {n : _Integer | Infinity}] := GraphComplex[g, {n, n}]

GraphComplex[g_ ? GraphQ, {n : _Integer, m : _Integer | Infinity}] := Union @@ (Subsets[#, {n, m}] & /@ FindClique[g, {n, Infinity}, All])


Options[FaceGraph] = Options[Graph]

FaceGraph[g : {___List}, opts : OptionsPattern[]] :=
    Graph[
        g,
        Catenate[Thread[UndirectedEdge[Rest[Subsets[#, Length[#] - 1]], #], List, 1] & /@ g],
        opts
    ]


BarycentricRefinement[g : {___List}] := GraphComplex[FaceGraph[g]]

BarycentricRefinement[g_ ? GraphQ, opts : OptionsPattern[]] := FaceGraph[GraphComplex[g], opts]

BarycentricRefinement[x_, n_Integer, opts : OptionsPattern[]] := Nest[BarycentricRefinement[#, opts] &, x, n]


GraphTopology[g_ ? GraphQ] := With[{c = GraphComplex[g]}, SimplexStar[c, #] & /@ c]


GraphSuspension[g_ ? GraphQ] := With[{v1 = Unique[\[FormalV]], v2 = Unique[\[FormalV]]},
	Graph3D[EdgeAdd[g, Catenate[{UndirectedEdge[v1, #], UndirectedEdge[#, v2]} & /@ VertexList[g]]]]
]



RandomGraphAutomorphism[g_ ? GraphQ, n : _Integer | Automatic | All : Automatic] :=
	With[{gr = GraphAutomorphismGroup[g]}, {order = GroupOrder[gr]},
		GroupElements[gr, RandomSample[;; order, UpTo[Replace[n, {Automatic -> 1, All -> order}]]]] //
			If[n === Automatic, First, Identity]
	]





ConnectionMatrix[g : {___List}, x : {___List}, y : {___List}] := Outer[
	ComplexEulerCharacteristic[Intersection[SimplexCore[g, #1], SimplexCore[g, #2]]] &, 
	x, y,
	1
]

ConnectionMatrix[g : {___List}, x : {___List}] := ConnectionMatrix[g, x, x]

ConnectionMatrix[g : {___List}, x : {___List}, perm_Cycles] := ConnectionMatrix[g, x, SimplicialMap[g, perm]]

ConnectionMatrix[g : {___List}, x : {___List}, f_Function] := ConnectionMatrix[g, x, f /@ x]

ConnectionMatrix[g : {___List}, args___] := ConnectionMatrix[g, g, args]


GreenFunctionMatrix[g : {___List}, x : {___List}, y : {___List}] := Outer[
    SimplexSign[#1] * SimplexSign[#2] * ComplexEulerCharacteristic[Intersection[SimplexStar[g, #1], SimplexStar[g, #2]]] &, 
    x, y,
    1
]

GreenFunctionMatrix[g : {___List}, x : {___List}] := GreenFunctionMatrix[g, x, x]

GreenFunctionMatrix[g : {___List}, x : {___List}, perm_Cycles] := GreenFunctionMatrix[g, x, SimplicialMap[g, perm]]

GreenFunctionMatrix[g : {___List}, x : {___List}, f_Function] := GreenFunctionMatrix[g, x, f /@ x]

GreenFunctionMatrix[g : {___List}, args___] := GreenFunctionMatrix[g, g, args]

