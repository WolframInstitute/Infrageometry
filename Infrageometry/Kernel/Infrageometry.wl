Package["WolframInstitute`Infrageometry`"]


PackageExport[ComplexClosure]
PackageExport[CanonicalComplex]

PackageExport[SimplexDimension]
PackageExport[ComplexDimension]
PackageExport[ComplexDimensions]
PackageExport[ComplexInductiveDimension]
PackageExport[SimplexList]
PackageExport[ComplexFacets]
PackageExport[ComplexVertexList]
PackageExport[SimplexCardinality]
PackageExport[SimplexCardinalities]
PackageExport[SimplexStar]
PackageExport[SimplexCore]
PackageExport[ComplexUnitSphere]
PackageExport[SimplexBoundary]
PackageExport[SimplexIndex]
PackageExport[SimplexSign]
PackageExport[SimplexWeight]
PackageExport[ContractibleQ]
PackageExport[SimplicialMap]

PackageExport[ComplexJoin]

PackageExport[ComplexEulerCharacteristic]
PackageExport[ComplexFermiCharacteristic]
PackageExport[LefschetzNumber]
PackageExport[LefschetzCurvature]

PackageExport[ComplexPolynomial]
PackageExport[PoincarePolynomial]
PackageExport[ComplexCurvature]
PackageExport[ComplexCurvatures]
PackageExport[DehnSommervilleQ]

PackageExport[GraphComplex]
PackageExport[SkeletonComplex]
PackageExport[ComplexGraph]
PackageExport[FaceGraph]
PackageExport[BarycentricRefinement]

PackageExport[GraphTopology]

PackageExport[IndexMatrix]
PackageExport[SignMatrix]
PackageExport[ComplexIncidenceMatrix]
PackageExport[ConnectionMatrix]
PackageExport[GreenFunctionMatrix]
PackageExport[DiracConnectionMatrix]
PackageExport[DiracHodgeMatrix]
PackageExport[DiracBlockMatrix]
PackageExport[DiracDualBlockMatrix]
PackageExport[DiracColumns]
PackageExport[DiracDualColumns]
PackageExport[HodgeBlock]
PackageExport[HodgeMatrix]
PackageExport[HodgeLaplacianMatrix]
PackageExport[BettiVector]

PackageExport[MatrixBlocks]
PackageExport[MatrixNullity]
PackageExport[SuperTrace]
PackageExport[PseudoDeterminant]
PackageExport[SuperDeterminant]

PackageExport[GraphSuspension]
PackageExport[RandomGraphAutomorphism]



ClearAll["WolframInstitute`Infrageometry`**`*", "WolframInstitute`Infrageometry`*"]



ComplexClosure[g : {___List}, {n_Integer, m : _Integer | Infinity}] :=
    Union[Catenate[Union[Subsets[Sort[#], {n, m}]] & /@ g]]

ComplexClosure[g : {___List}, n : _Integer | Infinity] := ComplexClosure[g, {1, n}]

ComplexClosure[g : {___List}, {n_Integer}] := ComplexClosure[g, {n, n}]

ComplexClosure[g : {___List}] := ComplexClosure[g, {1, Infinity}]


CanonicalComplex[g : {___List}] := ComplexClosure[Replace[g, Thread[# -> Range[Length[#]]], {2}] & @ DeleteDuplicates[Catenate[g]]]

SimplexDimension[x_List] := Length[x] - 1

ComplexDimension[g : {___List}] := Max[Map[SimplexDimension, g], -1]

ComplexDimensions[g : {___List}] := 1 + ComplexInductiveDimension[ComplexUnitSphere[g, #]] & /@ SimplexList[g, 0]

ComplexInductiveDimension[g : {___List}] := If[g === {}, -1, Mean[ComplexDimensions[g]]]

SimplexList[g : {___List}, {n_Integer, m : _Integer | Infinity}] := Select[g, Between[SimplexDimension[#], {n, m}] &]

SimplexList[g : {___List}, n : _Integer | Infinity] := SimplexList[g, {0, n}]

SimplexList[g : {___List}, {n_Integer}] := SimplexList[g, {n, n}]

SimplexList[g : {___List}] := SimplexList[g, {2, Infinity}]

ComplexFacets[g : {___List}] := SimplexList[g, {ComplexDimension[g]}]

ComplexVertexList[g : {___List}] := Union[Catenate[g]]

SimplexCardinality[g : {___List}, d_Integer] := Count[g, x_ /; SimplexDimension[x] == d]

SimplexCardinalities[g : {___List}] := Rest[BinCounts[Length /@ g]]

SimplexStar[g : {___List}, x_List] := Select[g, SubsetQ[#, x] &]

SimplexCore[g : {___List}, x_List] := Select[g, SubsetQ[x, #] &]

ComplexUnitSphere[g : {___List}, x_List] := Complement[ComplexClosure[#], #] & @ SimplexStar[g, x]

SimplexBoundary[x_List] := Subsets[x, {Length[x] - 1}]


SimplexSign[x_List, y_List] := Replace[UniqueElements[{x, y}], {{{c_}, {}} :> Signature[Prepend[y, c]] * Signature[x], _ -> 0}]

SimplexWeight[x_List] := - (-1) ^ Length[x]

SimplexIndex[x_List, y_List] := If[Sort[x] === Sort[y], SimplexWeight[x] * Signature[x] * Signature[y], 0]


ContractibleQ[g : {___List}] := MatchQ[g, {{_}}] || AnyTrue[g, With[{s = SimplexStar[g, #]}, ContractibleQ[Complement[ComplexClosure[s], s]] && ContractibleQ[Complement[g, s]]] &]

SimplicialMap[g : {___List}, perm_Cycles] :=
	With[{vs = Catenate[SimplexList[g, 0]]}, {rules = Thread[vs -> Permute[vs, perm]]},
		Function[Replace[#, rules, 1]]
	]

SimplicialMap[g : {___List}, perm_List] := SimplicialMap[g, PermutationCycles[perm]]


ComplexJoin[f : {___List}, g : {___List}] := Block[{g1 = Map[Subscript[#, 1] &, f, {2}], g2 = Map[Subscript[#, 2] &, g, {2}]},
    Join[g1, g2, Catenate[Outer[Join, g1, g2, 1]]]
]


ComplexEulerCharacteristic[g : {___List}] := Plus @@ Map[SimplexWeight, g]

ComplexFermiCharacteristic[g : {___List}] := Times @@ Map[SimplexWeight, g]


LefschetzNumber[g : {___List}, map_Function] := Total[SimplexIndex[#, map[#]] & /@ g]

LefschetzNumber[g : {___List}, map_] := Enclose @ LefschetzNumber[g, ConfirmMatch[SimplicialMap[g, map], _Function]]

LefschetzNumber[g : {___List}] := Mean[LefschetzNumber[g, #] & /@ RandomGraphAutomorphism[g, All]]


LefschetzCurvature[g : {___List}, xs : {___List}] := With[{maps = SimplicialMap[g, #] & /@ RandomGraphAutomorphism[g, All]},
    Table[Mean[SimplexIndex[x, #[x]] & /@ maps], {x, xs}]
]

LefschetzCurvature[g : {___List}] := LefschetzCurvature[g, g]


ComplexPolynomial[g : {___List}, t_ : \[FormalT]] := 1 + # . t ^ Range[Length[#]] & @ SimplexCardinalities[g]

PoincarePolynomial[g : {___List}, t_ : \[FormalT]] := 1 + # . t ^ Range[Length[#]] & @ BettiVector[g]

ComplexCurvature[g : {___List}, t_ : \[FormalT]] := Integrate[ComplexPolynomial[g], {\[FormalT], 0, t}]

ComplexCurvatures[g : {___List}, t_ : \[FormalT]] := If[SimplexDimension[#] == 0, 0, ComplexCurvature[ComplexUnitSphere[g, #], t]] & /@ g

DehnSommervilleQ[g : {___List}] := With[{f = ComplexPolynomial[g]}, TrueQ[Expand[f == (f /. \[FormalT] -> - 1 - \[FormalT])]]]


GraphComplex[g_ ? GraphQ, k : _Integer | Infinity : Infinity] := GraphComplex[g, {1, k}]

GraphComplex[g_ ? GraphQ, {n : _Integer | Infinity}] := GraphComplex[g, {n, n}]

GraphComplex[g_ ? GraphQ, {n : _Integer, m : _Integer | Infinity}] := Union @@ (Subsets[#, {n, m}] & /@ FindClique[g, {n, Infinity}, All])


SkeletonComplex[g_ ? GraphQ] := GraphComplex[g, 2]


Options[ComplexGraph] = Options[FaceGraph] = Options[Graph]

ComplexGraph[g : {___List}, opts : OptionsPattern[]] := Graph[ComplexVertexList[g], UndirectedEdge @@@ SimplexList[g, {1}], opts]

FaceGraph[g : {___List}, opts : OptionsPattern[]] :=
    Graph[
        g,
        Catenate[Thread[UndirectedEdge[Subsets[#, {1, Length[#] - 1}], #], List, 1] & /@ g],
        opts
    ]


BarycentricRefinement[g : {___List}] := GraphComplex[FaceGraph[g]]

BarycentricRefinement[g_ ? GraphQ, opts : OptionsPattern[]] := FaceGraph[GraphComplex[g], opts]

BarycentricRefinement[x_, n_Integer, opts : OptionsPattern[]] := Nest[BarycentricRefinement[#, opts] &, x, n]


GraphTopology[g_ ? GraphQ] := With[{c = GraphComplex[g]}, SimplexStar[c, #] & /@ c]


IndexMatrix[_, x : {___List}, y : {___List}] := Outer[SimplexIndex &, x, y, 1]

SignMatrix[_, x : {___List}, y : {___List}] := Outer[SimplexSign, x, y, 1]

ComplexIncidenceMatrix[g : {___List}, k_Integer : 0] := SignMatrix[g, {k + 1}, {k}]

ComplexIncidenceMatrix[g : {___List}, All] := ComplexIncidenceMatrix[g, #] & /@ Range[0, ComplexDimension[g] - 1]

ConnectionMatrix[g : {___List}, x : {___List}, y : {___List}] := Outer[
	ComplexEulerCharacteristic[Intersection[SimplexCore[g, #1], SimplexCore[g, #2]]] &, 
	x, y,
	1
]

GreenFunctionMatrix[g : {___List}, x : {___List}, y : {___List}] := Outer[
    SimplexWeight[#1] * SimplexWeight[#2] * ComplexEulerCharacteristic[Intersection[SimplexStar[g, #1], SimplexStar[g, #2]]] &, 
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
    {IndexMatrix, SignMatrix, ConnectionMatrix, GreenFunctionMatrix}
]

DiracHodgeMatrix[args___] := Enclose @ With[{d = ConfirmBy[SignMatrix[args], SquareMatrixQ]},
    d + Transpose[d]
]

DiracConnectionMatrix[args___] := Enclose @ With[{l = ConfirmBy[ConnectionMatrix[args], SquareMatrixQ]},
    l + Transpose[l]
]


DiracBlockMatrix[g : {___List}] := BlockDiagonalMatrix[Transpose[#] . # & /@ ComplexIncidenceMatrix[g, All]]

DiracDualBlockMatrix[g : {___List}] := BlockDiagonalMatrix[# . Transpose[#] & /@ ComplexIncidenceMatrix[g, All]]

DiracColumns[g : {___List}] := MatrixColumns[DiracHodgeMatrix[g], SimplexCardinalities[g]]

DiracDualColumns[g : {___List}] := Transpose /@ DiracColumns[g]


HodgeBlock[args___] := Enclose @ With[{d = ConfirmBy[DiracHodgeMatrix[args], SquareMatrixQ]}, d . d]

HodgeMatrix[g : {___List}] := With[{d = DiracHodgeMatrix[g]}, BlockDiagonalMatrix[MatrixBlocks[d . d, SimplexCardinalities[g]]]]

HodgeLaplacianMatrix[g : {___List}] := With[{d = DiracBlockMatrix[g]}, d . d]


BettiVector[g : {___List}] := MatrixNullity /@ HodgeMatrix[g]["Blocks"]



TakeBlocks[blocks : {__Integer}] := Threaded[{1, 0}] + Partition[Prepend[Accumulate[blocks], 0], 2, 1]

MatrixBlocks[mat_ ? SquareMatrixQ, blocks : {__Integer}] := Map[Take[mat, #, #] &, TakeBlocks[blocks]]

MatrixColumns[mat_ ? SquareMatrixQ, blocks : {__Integer}] := Map[Take[mat, All, #] &, TakeBlocks[blocks]]

MatrixNullity[mat_ ? SquareMatrixQ] := Length[NullSpace[mat]]


SuperTrace[vec_ ? VectorQ] := Total[vec * (-1) ^ Range[0, Length[vec] - 1]]

SuperTrace[mat_BlockDiagonalMatrix] := SuperTrace[Tr /@ mat["Blocks"]]

SuperTrace[mat_ ? SquareMatrixQ, blocks : {__Integer}] := SuperTrace[Tr /@ MatrixBlocks[mat, blocks]]


PseudoDeterminant[mat_ ? SquareMatrixQ] := Times @@ Select[Eigenvalues[mat], UnequalTo[0]]

SuperDeterminant[vec_ ? VectorQ] := Times @@ (vec ^ ((-1) ^ Range[Length[vec]]))

SuperDeterminant[mat_BlockDiagonalMatrix] := SuperDeterminant[PseudoDeterminant /@ mat["Blocks"]]

SuperDeterminant[mat_ ? SquareMatrixQ, blocks : {__Integer}] := SuperDeterminant[PseudoDeterminant /@ MatrixBlocks[mat, blocks]]


GraphSuspension[g_ ? GraphQ] := With[{v1 = Unique[\[FormalV]], v2 = Unique[\[FormalV]]},
	Graph3D[EdgeAdd[g, Catenate[{UndirectedEdge[v1, #], UndirectedEdge[#, v2]} & /@ VertexList[g]]]]
]


RandomGraphAutomorphism[g_ ? GraphQ, n : _Integer | Automatic | All : Automatic] :=
	With[{gr = GraphAutomorphismGroup[g]}, {order = GroupOrder[gr]},
		GroupElements[gr, RandomSample[;; order, UpTo[Replace[n, {Automatic -> 1, All -> order}]]]] //
			If[n === Automatic, First, Identity]
	]

RandomGraphAutomorphism[g : {___List}, args___] := RandomGraphAutomorphism[ComplexGraph[g], args]

