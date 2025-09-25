Package["WolframInstitute`Infrageometry`"]


PackageExport[CanonicalComplex]

PackageExport[SimplexDimension]
PackageExport[ComplexDimension]
PackageExport[SimplexList]
PackageExport[SimplexCardinalities]
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


PackageExport[OrderMatrix]
PackageExport[ConnectionMatrix]
PackageExport[GreenFunctionMatrix]
PackageExport[DiracMatrix]
PackageExport[DiracHodgeMatrix]
PackageExport[HodgeMatrix]
PackageExport[BettiVector]

PackageExport[MatrixBlocks]
PackageExport[MatrixNullity]
PackageExport[SuperTrace]



ClearAll["WolframInstitute`Infrageometry`**`*", "WolframInstitute`Infrageometry`*"]



CanonicalComplex[g : {___List}] := Union[Map[Union, g]]

SimplexDimension[x_List] := Length[x] - 1

ComplexDimension[g : {___List}] := Max[Map[SimplexDimension, g], 0]

SimplexList[g : {___List}, {n_Integer, m_Integer}] := Select[g, Between[SimplexDimension[#], {n, m}] &]

SimplexList[g : {___List}, n_Integer] := SimplexList[g, {0, n}]

SimplexList[g : {___List}, {n_Integer}] := SimplexList[g, {n, n}]

SimplexCardinalities[g : {___List}] := Lookup[#, Range[Max[Keys[#]]], 0] & @ Counts[Map[Length, g]]

SimplexStar[g : {___List}, x_List] := Select[g, SubsetQ[#, x] &]

SimplexCore[g : {___List}, x_List] := Select[g, SubsetQ[x, #] &]

SimplexOrder[x_List, y_List] := Replace[UniqueElements[{x, y}], {{{c_}, {}} :> Signature[Prepend[y, c]] * Signature[x], _ -> 0}]

SimplexSign[x_List] := - (-1) ^ Length[x]

SimplicialMap[g : {___List}, perm_Cycles] :=
	With[{vs = Catenate[SimplexList[g, 0]]}, {rules = Thread[vs -> Permute[vs, perm]]},
		Function[Replace[#, rules, 1]]
	]

SimplicialMap[g : {___List}, perm_List] := SimplicialMap[g, PermutationCycles[perm]]


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


OrderMatrix[_, x : {___List}, y : {___List}] := Outer[SimplexOrder, x, y, 1]

ConnectionMatrix[g : {___List}, x : {___List}, y : {___List}] := Outer[
	ComplexEulerCharacteristic[Intersection[SimplexCore[g, #1], SimplexCore[g, #2]]] &, 
	x, y,
	1
]

GreenFunctionMatrix[g : {___List}, x : {___List}, y : {___List}] := Outer[
    SimplexSign[#1] * SimplexSign[#2] * ComplexEulerCharacteristic[Intersection[SimplexStar[g, #1], SimplexStar[g, #2]]] &, 
    x, y,
    1
]

Scan[f |-> (
    f[g : {___List}, x : {___List}] := f[g, x, x];
    f[g : {___List}, x : {___List}, perm_Cycles] := f[g, x, SimplicialMap[g, perm]];
    f[g : {___List}, x : {___List}, map_Function] := f[g, x, map /@ x];
    f[g : {___List}, x : _Integer | {_Integer} | {_Integer, _Integer}, args___] := f[g, SimplexList[g, x], args];
    f[g : {___List}, x_, y : _Integer | {_Integer} | {_Integer, _Integer}] := f[g, x, SimplexList[g, y]];
    f[g : {___List}, args___] := f[g, g, args];
)
    ,
    {OrderMatrix, ConnectionMatrix, GreenFunctionMatrix}
]

DiracHodgeMatrix[args___] := Enclose @ With[{d = ConfirmBy[OrderMatrix[args], SquareMatrixQ]},
    d + Transpose[d]
]

DiracMatrix[args___] := Enclose @ With[{l = ConfirmBy[ConnectionMatrix[args], SquareMatrixQ]},
    l + Transpose[l]
]


HodgeMatrix[g : {___List}] := Enclose @ With[{d = ConfirmBy[DiracHodgeMatrix[g], SquareMatrixQ]},
    BlockDiagonalMatrix[MatrixBlocks[d . d, SimplexCardinalities[g]]]
]


BettiVector[g : {___List}] := MatrixNullity /@ HodgeMatrix[g]["Blocks"]



MatrixBlocks[mat_ ? SquareMatrixQ, blocks : {__Integer}] := Map[Take[mat, #, #] &, Threaded[{1, 0}] + Partition[Prepend[Accumulate[blocks], 0], 2, 1]]

MatrixNullity[mat_ ? SquareMatrixQ] := Length[NullSpace[mat]]


SuperTrace[vec_ ? VectorQ] := Total[vec * (-1) ^ Range[0, Length[vec] - 1]]

SuperTrace[mat_BlockDiagonalMatrix] := SuperTrace[Tr /@ mat["Blocks"]]

SuperTrace[mat_ ? SquareMatrixQ, blocks : {__Integer}] := SuperTrace[Tr /@ MatrixBlocks[mat, blocks]]



GraphSuspension[g_ ? GraphQ] := With[{v1 = Unique[\[FormalV]], v2 = Unique[\[FormalV]]},
	Graph3D[EdgeAdd[g, Catenate[{UndirectedEdge[v1, #], UndirectedEdge[#, v2]} & /@ VertexList[g]]]]
]

RandomGraphAutomorphism[g_ ? GraphQ, n : _Integer | Automatic | All : Automatic] :=
	With[{gr = GraphAutomorphismGroup[g]}, {order = GroupOrder[gr]},
		GroupElements[gr, RandomSample[;; order, UpTo[Replace[n, {Automatic -> 1, All -> order}]]]] //
			If[n === Automatic, First, Identity]
	]

