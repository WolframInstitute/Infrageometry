BeginTestSection["InfrageometryTests"]

(* ===== Test Fixtures ===== *)

triangle = ComplexClosure[{{1, 2, 3}}]
edge = ComplexClosure[{{1, 2}}]
tetrahedron = ComplexClosure[{{1, 2, 3, 4}}]
empty = {}
twoEdges = ComplexClosure[{{1, 2}, {3, 4}}]
path3 = ComplexClosure[{{1, 2}, {2, 3}}]
square = ComplexClosure[{{1, 2}, {2, 3}, {3, 4}, {1, 4}}]

(* ===== 1. ComplexClosure & Indexing ===== *)

VerificationTest[
    ComplexClosure[{{1, 2, 3}}],
    {{1}, {2}, {3}, {1, 2}, {1, 3}, {2, 3}, {1, 2, 3}},
    TestID -> "ComplexClosure-Triangle"
]

VerificationTest[
    ComplexClosure[{{2, 1}}],
    {{1}, {2}, {1, 2}},
    TestID -> "ComplexClosure-SortedOutput"
]

VerificationTest[
    ComplexClosure[{}],
    {},
    TestID -> "ComplexClosure-Empty"
]

VerificationTest[
    ComplexClosure[{{1, 2, 3}}, {2}],
    {{1, 2}, {1, 3}, {2, 3}},
    TestID -> "ComplexClosure-Dimension2"
]

(* vertices are indexed in order of first appearance, not sorted order: b, a, c -> 1, 2, 3 *)
VerificationTest[
    IndexComplex[{{"b", "a"}, {"a", "c"}}],
    {{1}, {2}, {3}, {1, 2}, {2, 3}},
    TestID -> "IndexComplex-AppearanceOrder"
]

VerificationTest[
    IndexComplex[{{"x", "y", "z"}}],
    {{1}, {2}, {3}, {1, 2}, {1, 3}, {2, 3}, {1, 2, 3}},
    TestID -> "IndexComplex-Triangle"
]

(* ===== 2. Dimensions ===== *)

VerificationTest[
    SimplexDimension[{1, 2, 3}],
    2,
    TestID -> "SimplexDimension-Triangle"
]

VerificationTest[
    SimplexDimension[{5}],
    0,
    TestID -> "SimplexDimension-Vertex"
]

VerificationTest[
    ComplexDimension[triangle],
    2,
    TestID -> "ComplexDimension-Triangle"
]

VerificationTest[
    ComplexDimension[edge],
    1,
    TestID -> "ComplexDimension-Edge"
]

VerificationTest[
    ComplexDimension[empty],
    -1,
    TestID -> "ComplexDimension-Empty"
]

VerificationTest[
    ComplexInductiveDimension[triangle],
    _?(# >= 0 &),
    SameTest -> MatchQ,
    TestID -> "ComplexInductiveDimension-NonNegative"
]

(* ===== 3. Simplex Lists & Cardinalities ===== *)

VerificationTest[
    SimplexCardinalities[triangle],
    {3, 3, 1},
    TestID -> "SimplexCardinalities-Triangle"
]

VerificationTest[
    SimplexCardinalities[tetrahedron],
    {4, 6, 4, 1},
    TestID -> "SimplexCardinalities-Tetrahedron"
]

VerificationTest[
    ComplexVertexList[triangle],
    {1, 2, 3},
    TestID -> "ComplexVertexList-Triangle"
]

VerificationTest[
    ComplexFacets[triangle],
    {{1, 2, 3}},
    TestID -> "ComplexFacets-Triangle"
]

VerificationTest[
    ComplexFacets[tetrahedron],
    {{1, 2, 3, 4}},
    TestID -> "ComplexFacets-Tetrahedron"
]

VerificationTest[
    Sort[ComplexWalls[tetrahedron]],
    Sort[{{1, 2, 3}, {1, 2, 4}, {1, 3, 4}, {2, 3, 4}}],
    TestID -> "ComplexWalls-Tetrahedron"
]

VerificationTest[
    Length[SimplexList[triangle, {1}]],
    3,
    TestID -> "SimplexList-Edges"
]

VerificationTest[
    SimplexList[triangle, {0}],
    {{1}, {2}, {3}},
    TestID -> "SimplexList-Vertices"
]

(* ===== 4. Star, Core, UnitSphere ===== *)

VerificationTest[
    Sort[SimplexStar[triangle, {1}]],
    Sort[{{1}, {1, 2}, {1, 3}, {1, 2, 3}}],
    TestID -> "SimplexStar-VertexInTriangle"
]

VerificationTest[
    Sort[SimplexCore[triangle, {1, 2, 3}]],
    Sort[{{1}, {2}, {3}, {1, 2}, {1, 3}, {2, 3}, {1, 2, 3}}],
    TestID -> "SimplexCore-Facet"
]

VerificationTest[
    SimplexCore[triangle, {1}],
    {{1}},
    TestID -> "SimplexCore-Vertex"
]

VerificationTest[
    Sort[SimplexUnitSphere[triangle, {1}]],
    Sort[{{2}, {3}, {2, 3}}],
    TestID -> "SimplexUnitSphere-VertexInTriangle"
]

VerificationTest[
    Sort[SimplexBoundary[{1, 2, 3}]],
    Sort[{{1, 2}, {1, 3}, {2, 3}}],
    TestID -> "SimplexBoundary-Triangle"
]

VerificationTest[
    SimplexBoundary[{1, 2}],
    {{1}, {2}},
    TestID -> "SimplexBoundary-Edge"
]

(* ===== 5. Topology Tests ===== *)

VerificationTest[
    ContractibleQ[triangle],
    True,
    TestID -> "ContractibleQ-FilledTriangle"
]

VerificationTest[
    ContractibleQ[edge],
    True,
    TestID -> "ContractibleQ-Edge"
]

VerificationTest[
    ContractibleQ[{{1}}],
    True,
    TestID -> "ContractibleQ-SingleVertex"
]

VerificationTest[
    ContractibleQ[empty],
    True,
    TestID -> "ContractibleQ-Empty"
]

VerificationTest[
    (* Boundary of a triangle (3 edges, no face) is S^1 = sphere *)
    ComplexSphereQ[ComplexClosure[{{1, 2}, {2, 3}, {1, 3}}]],
    True,
    TestID -> "ComplexSphereQ-CircleS1"
]

(* ===== 6. Signs & Weights ===== *)

VerificationTest[
    SimplexSign[{1, 2, 3}],
    1,
    TestID -> "SimplexSign-Sorted"
]

VerificationTest[
    SimplexSign[{2, 1, 3}],
    -1,
    TestID -> "SimplexSign-Transposition"
]

VerificationTest[
    SimplexWeight[{1}],
    1,
    TestID -> "SimplexWeight-Vertex"
]

VerificationTest[
    SimplexWeight[{1, 2}],
    -1,
    TestID -> "SimplexWeight-Edge"
]

VerificationTest[
    SimplexWeight[{1, 2, 3}],
    1,
    TestID -> "SimplexWeight-Face"
]

VerificationTest[
    SimplexIndex[{1, 2, 3}],
    SimplexWeight[{1, 2, 3}] * SimplexSign[{1, 2, 3}],
    TestID -> "SimplexIndex-Consistency"
]

(* ===== 7. Characteristics ===== *)

VerificationTest[
    ComplexEulerCharacteristic[triangle],
    1,
    TestID -> "EulerChar-Triangle"
]

VerificationTest[
    ComplexEulerCharacteristic[tetrahedron],
    1,
    TestID -> "EulerChar-Tetrahedron"
]

VerificationTest[
    ComplexEulerCharacteristic[empty],
    0,
    TestID -> "EulerChar-Empty"
]

VerificationTest[
    (* S^1 boundary: chi = 0 *)
    ComplexEulerCharacteristic[ComplexClosure[{{1, 2}, {2, 3}, {1, 3}}]],
    0,
    TestID -> "EulerChar-S1"
]

VerificationTest[
    ComplexEulerCharacteristic[twoEdges],
    2,
    TestID -> "EulerChar-TwoDisjointEdges"
]

VerificationTest[
    ComplexFermiCharacteristic[triangle],
    -1,
    TestID -> "FermiChar-Triangle"
]

VerificationTest[
    ComplexFermiCharacteristic[edge],
    -1,
    TestID -> "FermiChar-Edge"
]

(* ===== 8. Polynomials ===== *)

VerificationTest[
    ComplexPolynomial[triangle, t],
    1 + 3 t + 3 t^2 + t^3,
    TestID -> "ComplexPolynomial-Triangle"
]

VerificationTest[
    ComplexPolynomial[edge, t],
    1 + 2 t + t^2,
    TestID -> "ComplexPolynomial-Edge"
]

VerificationTest[
    (* Paclet convention: 1 + sum(b_i * t^(i+1)) *)
    PoincarePolynomial[triangle, t],
    1 + t,
    TestID -> "PoincarePolynomial-FilledTriangle"
]

(* ===== 9. Graph Construction ===== *)

VerificationTest[
    GraphQ[ComplexGraph[triangle]],
    True,
    TestID -> "ComplexGraph-IsGraph"
]

VerificationTest[
    VertexCount[ComplexGraph[triangle]],
    3,
    TestID -> "ComplexGraph-VertexCount"
]

VerificationTest[
    EdgeCount[ComplexGraph[triangle]],
    3,
    TestID -> "ComplexGraph-EdgeCount"
]

VerificationTest[
    GraphQ[FaceGraph[triangle]],
    True,
    TestID -> "FaceGraph-IsGraph"
]

VerificationTest[
    VertexCount[FaceGraph[triangle]],
    7,
    TestID -> "FaceGraph-VertexCount"
]

VerificationTest[
    GraphQ[ComplexGraph[SkeletonComplex[CycleGraph[4]]]],
    True,
    TestID -> "SkeletonComplex-CycleGraph"
]

VerificationTest[
    Length[BarycentricRefinement[triangle]] > Length[triangle],
    True,
    TestID -> "BarycentricRefinement-Finer"
]

(* ===== 10. Matrices ===== *)

VerificationTest[
    (* Incidence matrix d for triangle should be 3x3 (edges x vertices) *)
    Dimensions[ComplexIncidenceMatrix[triangle, 0]],
    {3, 3},
    TestID -> "IncidenceMatrix-TriangleDim"
]

VerificationTest[
    SquareMatrixQ[DiracHodgeMatrix[triangle]],
    True,
    TestID -> "DiracHodgeMatrix-Square"
]

VerificationTest[
    Dimensions[DiracHodgeMatrix[triangle]],
    {7, 7},
    TestID -> "DiracHodgeMatrix-7x7"
]

VerificationTest[
    With[{d = DiracHodgeMatrix[triangle]}, d === Transpose[d]],
    True,
    TestID -> "DiracHodgeMatrix-Symmetric"
]

VerificationTest[
    SquareMatrixQ[ConnectionMatrix[triangle]],
    True,
    TestID -> "ConnectionMatrix-Square"
]

VerificationTest[
    Dimensions[ConnectionMatrix[triangle]],
    {7, 7},
    TestID -> "ConnectionMatrix-7x7"
]

VerificationTest[
    Dimensions[GreenFunctionMatrix[triangle]],
    {7, 7},
    TestID -> "GreenFunctionMatrix-7x7"
]

VerificationTest[
    BettiVector[triangle],
    {1, 0, 0},
    TestID -> "BettiVector-FilledTriangle"
]

VerificationTest[
    BettiVector[tetrahedron],
    {1, 0, 0, 0},
    TestID -> "BettiVector-Tetrahedron"
]

VerificationTest[
    (* S^1 boundary: Betti = {1, 1} *)
    BettiVector[ComplexClosure[{{1, 2}, {2, 3}, {1, 3}}]],
    {1, 1},
    TestID -> "BettiVector-S1"
]

VerificationTest[
    (* Path: Betti = {1, 0} *)
    BettiVector[path3],
    {1, 0},
    TestID -> "BettiVector-Path"
]

(* ===== 11. Matrix Utilities ===== *)

VerificationTest[
    MatrixNullity[IdentityMatrix[3]],
    0,
    TestID -> "MatrixNullity-Identity"
]

VerificationTest[
    MatrixNullity[ConstantArray[0, {3, 3}]],
    3,
    TestID -> "MatrixNullity-Zero"
]

VerificationTest[
    SuperTrace[{1, 2, 3}],
    1 - 2 + 3,
    TestID -> "SuperTrace-Vector"
]

VerificationTest[
    PseudoDeterminant[DiagonalMatrix[{1, 0, 2}]],
    2,
    TestID -> "PseudoDeterminant-DiagWithZero"
]

VerificationTest[
    PseudoDeterminant[IdentityMatrix[4]],
    1,
    TestID -> "PseudoDeterminant-Identity"
]

(* ===== 12. Algebraic Identities ===== *)

VerificationTest[
    (* Euler = alternating sum of Betti numbers *)
    ComplexEulerCharacteristic[triangle] == Total[(-1) ^ Range[0, Length[BettiVector[triangle]] - 1] * BettiVector[triangle]],
    True,
    TestID -> "Identity-EulerBetti-Triangle"
]

VerificationTest[
    With[{s1 = ComplexClosure[{{1, 2}, {2, 3}, {1, 3}}]},
        ComplexEulerCharacteristic[s1] == Total[(-1) ^ Range[0, Length[BettiVector[s1]] - 1] * BettiVector[s1]]
    ],
    True,
    TestID -> "Identity-EulerBetti-S1"
]

VerificationTest[
    (* det(L) = Fermi characteristic for any simplicial complex *)
    Det[ConnectionMatrix[triangle]] == ComplexFermiCharacteristic[triangle],
    True,
    TestID -> "Identity-DetConnection-Fermi-Triangle"
]

VerificationTest[
    Det[ConnectionMatrix[tetrahedron]] == ComplexFermiCharacteristic[tetrahedron],
    True,
    TestID -> "Identity-DetConnection-Fermi-Tetrahedron"
]

VerificationTest[
    (* Unimodularity: |det(L)| = 1 *)
    Abs[Det[ConnectionMatrix[triangle]]],
    1,
    TestID -> "Identity-Unimodularity-Triangle"
]

VerificationTest[
    Abs[Det[ConnectionMatrix[tetrahedron]]],
    1,
    TestID -> "Identity-Unimodularity-Tetrahedron"
]

VerificationTest[
    (* Energy theorem: Total[Flatten[g]] == chi(G) *)
    Total[Flatten[GreenFunctionMatrix[triangle]]] == ComplexEulerCharacteristic[triangle],
    True,
    TestID -> "Identity-EnergyTheorem-Triangle"
]

VerificationTest[
    Total[Flatten[GreenFunctionMatrix[tetrahedron]]] == ComplexEulerCharacteristic[tetrahedron],
    True,
    TestID -> "Identity-EnergyTheorem-Tetrahedron"
]

VerificationTest[
    (* Inverse[L] == Transpose[g] — the Green function is the transpose of the inverse connection *)
    With[{l = ConnectionMatrix[triangle], g = GreenFunctionMatrix[triangle]},
        Inverse[l] == Transpose[g]
    ],
    True,
    TestID -> "Identity-GreenInverseConnection-Triangle"
]

(* ===== 13. ComplexJoin & ComplexDual ===== *)

VerificationTest[
    (* Join of two points = edge (with subscripted vertex labels) *)
    Length[ComplexJoin[{{1}}, {{1}}]],
    3,
    TestID -> "ComplexJoin-TwoPoints"
]

VerificationTest[
    ListQ[ComplexDual[triangle]],
    True,
    TestID -> "ComplexDual-ReturnsList"
]

(* ===== 14. Geodesics ===== *)

VerificationTest[
    ListQ[ComplexGeodesicFlow[triangle, {1, 2, 3}]],
    True,
    TestID -> "GeodesicFlow-ReturnsFrame"
]

VerificationTest[
    Length[ComplexGeodesicFlow[triangle, {1, 2, 3}]],
    3,
    TestID -> "GeodesicFlow-PreservesFrameSize"
]

VerificationTest[
    With[{orbit = SimplexOrbit[triangle, {1, 2, 3}]},
        ListQ[orbit] && Length[orbit] >= 2
    ],
    True,
    TestID -> "SimplexOrbit-NonTrivial"
]

VerificationTest[
    (* ComplexGeodesics partitions frames *)
    With[{geos = ComplexGeodesics[triangle]},
        AllTrue[geos, ListQ]
    ],
    True,
    TestID -> "ComplexGeodesics-ReturnsList"
]

(* ===== 15. Experimental ===== *)

VerificationTest[
    FaceVector[triangle],
    {3, 3, 1},
    TestID -> "FaceVector-Triangle"
]

VerificationTest[
    Length[HVector[triangle]],
    4,
    TestID -> "HVector-Triangle-Length"
]

VerificationTest[
    (* Link of vertex {1} in tetrahedron should be a triangle *)
    Sort[LinkComplex[tetrahedron, {1}]],
    Sort[ComplexClosure[{{2, 3, 4}}]],
    TestID -> "LinkComplex-TetrahedronVertex"
]

VerificationTest[
    PureComplexQ[triangle],
    True,
    TestID -> "PureComplexQ-Triangle"
]

VerificationTest[
    (* Closed complex with single top-dim: facets all same dimension *)
    PureComplexQ[tetrahedron],
    True,
    TestID -> "PureComplexQ-Tetrahedron"
]

VerificationTest[
    EulerBettiConsistencyQ[triangle],
    True,
    TestID -> "EulerBettiConsistencyQ-Triangle"
]

VerificationTest[
    EulerBettiConsistencyQ[tetrahedron],
    True,
    TestID -> "EulerBettiConsistencyQ-Tetrahedron"
]

VerificationTest[
    (* Dirichlet energy for constant function should be 0 *)
    DiscreteDirichletEnergy[triangle, <|1 -> 1, 2 -> 1, 3 -> 1|>],
    0,
    TestID -> "DirichletEnergy-Constant"
]

VerificationTest[
    (* Dirichlet energy for non-constant function should be positive *)
    DiscreteDirichletEnergy[triangle, <|1 -> 0, 2 -> 1, 3 -> 0|>] > 0,
    True,
    TestID -> "DirichletEnergy-Positive"
]

(* ===== 16. SimplicialSet ===== *)

VerificationTest[
    Head[SimplicialSet[{{1, 2}, {2, 3}}]],
    SimplicialData,
    TestID -> "SimplicialSet-ReturnsSimplicialData"
]

VerificationTest[
    SimplicialSet[{}]["Dimension"],
    -1,
    TestID -> "SimplicialSet-EmptyDimension"
]

VerificationTest[
    SimplicialSet[{{1, 2, 3}}]["Dimension"],
    2,
    TestID -> "SimplicialSet-TriangleDimension"
]

VerificationTest[
    SimplicialSetQ[SimplicialSet[{{1, 2, 3}}]],
    True,
    TestID -> "SimplicialSetQ-Triangle"
]

VerificationTest[
    SimplicialComplexQ[SimplicialSet[{{1, 2, 3}}]],
    True,
    TestID -> "SimplicialComplexQ-Triangle"
]

VerificationTest[
    SimplicialSet[{{1, 2}, {2, 3}}]["Dimensions"],
    _List,
    SameTest -> MatchQ,
    TestID -> "SimplicialSet-DimensionsList"
]

(* ===== 17. Hypergraph Utilities ===== *)

VerificationTest[
    HypergraphVertexSet[{{1, 2, 3}, {2, 3, 4}}],
    {1, 2, 3, 4},
    TestID -> "HypergraphVertexSet-Basic"
]

VerificationTest[
    HypergraphVertexCount[{{1, 2, 3}, {2, 3, 4}}],
    4,
    TestID -> "HypergraphVertexCount-Basic"
]

VerificationTest[
    HyperedgeCount[{{1, 2}, {3, 4}, {5, 6}}],
    3,
    TestID -> "HyperedgeCount-Basic"
]

VerificationTest[
    HyperedgeSizes[{{1, 2}, {3, 4, 5}}],
    {2, 3},
    TestID -> "HyperedgeSizes-Basic"
]

VerificationTest[
    Sort[HyperedgeSizeDistribution[{{1, 2}, {3, 4}, {5, 6, 7}}]],
    Sort[{{2, 2}, {3, 1}}],
    TestID -> "HyperedgeSizeDistribution-Basic"
]

VerificationTest[
    Sort[Keys[HypergraphDegree[{{1, 2}, {2, 3}}]]],
    {1, 2, 3},
    TestID -> "HypergraphDegree-Keys"
]

VerificationTest[
    HypergraphDegree[{{1, 2}, {2, 3}}][2],
    2,
    TestID -> "HypergraphDegree-SharedVertex"
]

VerificationTest[
    Sort[HypergraphMaximalEdges[{{1, 2}, {1, 2, 3}, {4, 5}}]],
    Sort[{{1, 2, 3}, {4, 5}}],
    TestID -> "HypergraphMaximalEdges-Basic"
]

VerificationTest[
    PopularHypergraphNames[] =!= {},
    True,
    TestID -> "PopularHypergraphNames-NonEmpty"
]

VerificationTest[
    HypergraphMaximalEdges[{{1, 2}, {1, 2, 3}, {4, 5}}] // Sort,
    Sort[{{1, 2, 3}, {4, 5}}],
    TestID -> "HypergraphMaximalEdges-Correct"
]

VerificationTest[
    With[{s = HypergraphSummary[{{1, 2}, {2, 3, 4}}]},
        s["VertexCount"] == 4 && s["HyperedgeCount"] == 2
    ],
    True,
    TestID -> "HypergraphSummary-Counts"
]

(* ===== 18. Alexandrov Topology ===== *)

VerificationTest[
    Length[AlexandrovTopology[triangle]],
    Length[triangle],
    TestID -> "AlexandrovTopology-LengthMatchesComplex"
]

(* ===== 19. DehnSommerville ===== *)

VerificationTest[
    (* Edge complex satisfies Dehn-Sommerville: f(t) = 1 + 2t + t^2 vs f(-1-t) = 1 + 2(-1-t) + (-1-t)^2 = t^2 => not equal. Check for a known case. *)
    BooleanQ[DehnSommervilleQ[triangle]],
    True,
    TestID -> "DehnSommervilleQ-ReturnsBool"
]

(* ===== 20. Lefschetz ===== *)

VerificationTest[
    (* Lefschetz number for identity map = Euler characteristic *)
    LefschetzNumber[triangle, SimplicialMap[triangle, Cycles[{}]]],
    ComplexEulerCharacteristic[triangle],
    TestID -> "Lefschetz-IdentityEqualsEuler"
]

VerificationTest[
    ListQ[LefschetzCurvature[triangle]],
    True,
    TestID -> "LefschetzCurvature-ReturnsList"
]

VerificationTest[
    (* Gauss-Bonnet: sum of Lefschetz curvatures = Euler char *)
    Total[LefschetzCurvature[triangle]],
    ComplexEulerCharacteristic[triangle],
    TestID -> "LefschetzCurvature-GaussBonnet"
]


(* ================================================================ *)
(* ================================================================ *)

(* ===== 21. Wave Equation ===== *)

VerificationTest[
    (* At small t with zero velocity, solution is close to u0 *)
    With[{n = Length[triangle]},
        With[{
            u0 = ReplacePart[ConstantArray[0., n], 1 -> 1.],
            v0 = ConstantArray[0., n]
        },
            Max[Abs[WaveEquationSolution[triangle, u0, v0, 0.001] - u0]] < 10^-3
        ]
    ],
    True,
    TestID -> "Wave-InitialCondition"
]

VerificationTest[
    Dimensions[WavePropagator[triangle, 0.1]],
    {7, 7},
    TestID -> "Wave-PropagatorDimension"
]

VerificationTest[
    (* Wave propagator at t=0 is identity *)
    Chop[WavePropagator[triangle, 0.] - IdentityMatrix[7]] === ConstantArray[0, {7, 7}],
    True,
    TestID -> "Wave-PropagatorIdentity"
]

VerificationTest[
    (* Propagator is unitary: U U* = I *)
    With[{u = WavePropagator[triangle, 0.5]},
        Max[Abs[Chop[u . ConjugateTranspose[u] - IdentityMatrix[7]]]] < 10^-8
    ],
    True,
    TestID -> "Wave-PropagatorUnitary"
]

VerificationTest[
    With[{
        n = Length[triangle],
        u = ConstantArray[0., 7], v = ConstantArray[0., 7]
    },
        With[{result = DiscreteWaveStep[triangle, ReplacePart[u, 1 -> 1.], v]},
            ListQ[result] && Length[result] == 2 && Length[result[[1]]] == n
        ]
    ],
    True,
    TestID -> "Wave-DiscreteStepStructure"
]

VerificationTest[
    Head[DiscreteWaveStep[triangle]],
    Function,
    TestID -> "Wave-DiscreteStepFunction"
]

(* ===== 22. Wu Characteristic ===== *)

VerificationTest[
    (* Wu_1 = Euler characteristic *)
    WuCharacteristic[triangle, 1],
    ComplexEulerCharacteristic[triangle],
    TestID -> "Wu-Order1EqualsEuler"
]

VerificationTest[
    WuCharacteristic[tetrahedron, 1],
    ComplexEulerCharacteristic[tetrahedron],
    TestID -> "Wu-Order1EqualsEuler-Tetra"
]

VerificationTest[
    (* Wu_2 for triangle = 1 (verified manually) *)
    WuCharacteristic[triangle, 2],
    1,
    TestID -> "Wu-Order2-Triangle"
]

VerificationTest[
    IntegerQ[WuCharacteristic[tetrahedron, 2]],
    True,
    TestID -> "Wu-Order2-Integer"
]

VerificationTest[
    WuCharacteristic[triangle] == WuCharacteristic[triangle, 2],
    True,
    TestID -> "Wu-DefaultOrder"
]

(* ===== 23. Isospectral Deformation ===== *)

VerificationTest[
    SquareMatrixQ[IsospectralDeformation[edge, 0.01, 10]],
    True,
    TestID -> "IsoDef-Square"
]

VerificationTest[
    (* Isospectrality: eigenvalues are preserved under deformation *)
    With[{
        eOrig = Sort[Eigenvalues[N[DiracHodgeMatrix[edge]]]],
        eDef = Sort[Eigenvalues[IsospectralDeformation[edge, 0.05, 100]]]
    },
        Max[Abs[eOrig - eDef]] < 10^-2
    ],
    True,
    TestID -> "IsoDef-EigenvaluesPreserved"
]

VerificationTest[
    VectorQ[DiracMass[edge, 0.01, 10], NumericQ],
    True,
    TestID -> "DiracMass-IsVector"
]

VerificationTest[
    (* DiracMass at t=0 should be approximately zero (no mass initially) *)
    Max[Abs[DiracMass[edge, 0., 1]]] < 10^-10,
    True,
    TestID -> "DiracMass-ZeroAtStart"
]

(* ===== 24. Connes Distance ===== *)

VerificationTest[
    With[{d = ConnesDistance[edge]},
        SquareMatrixQ[d] && Dimensions[d] == {2, 2}
    ],
    True,
    TestID -> "ConnesDistance-SquareMatrix"
]

VerificationTest[
    With[{d = ConnesDistance[edge]},
        Max[Abs[d - Transpose[d]]] < 10^-10
    ],
    True,
    TestID -> "ConnesDistance-Symmetric"
]

VerificationTest[
    With[{d = ConnesDistance[edge]},
        Max[Abs[Diagonal[d]]] < 10^-10
    ],
    True,
    TestID -> "ConnesDistance-DiagonalZero"
]

(* ===== 25. Zeta Functions ===== *)

VerificationTest[
    NumericQ[DiracZetaFunction[triangle, 2.]],
    True,
    TestID -> "DiracZeta-Numeric"
]

VerificationTest[
    NumericQ[ConnectionZetaFunction[triangle, 2.]],
    True,
    TestID -> "ConnectionZeta-Numeric"
]

VerificationTest[
    (* Connection zeta is sum of |eigenvalue|^{-s}; for triangle with 7 eigenvalues *)
    ConnectionZetaFunction[triangle, 2.] > 0,
    True,
    TestID -> "ConnectionZeta-Positive"
]

VerificationTest[
    NumericQ[LefschetzZetaFunction[triangle, Cycles[{}], 0.1, 5]],
    True,
    TestID -> "LefschetzZeta-Numeric"
]

(* ===== 26. Analytic Torsion ===== *)

VerificationTest[
    NumericQ[AnalyticTorsion[triangle]],
    True,
    TestID -> "AnalyticTorsion-Numeric"
]

VerificationTest[
    AnalyticTorsion[triangle] > 0,
    True,
    TestID -> "AnalyticTorsion-Positive"
]

VerificationTest[
    NumericQ[AnalyticTorsion[tetrahedron]],
    True,
    TestID -> "AnalyticTorsion-Tetrahedron"
]

(* ===== 27. Index-Expectation Curvature ===== *)

VerificationTest[
    VectorQ[IndexExpectationCurvature[triangle], NumericQ],
    True,
    TestID -> "IndexCurvature-IsVector"
]

VerificationTest[
    Length[IndexExpectationCurvature[triangle]],
    Length[triangle],
    TestID -> "IndexCurvature-CorrectLength"
]

VerificationTest[
    (* Gauss-Bonnet: sum of curvatures = Euler characteristic *)
    Abs[Total[IndexExpectationCurvature[triangle]] - ComplexEulerCharacteristic[triangle]] < 10^-10,
    True,
    TestID -> "IndexCurvature-GaussBonnet"
]

VerificationTest[
    Abs[Total[IndexExpectationCurvature[tetrahedron]] - ComplexEulerCharacteristic[tetrahedron]] < 10^-10,
    True,
    TestID -> "IndexCurvature-GaussBonnet-Tetra"
]

VerificationTest[
    With[{s1 = ComplexClosure[{{1, 2}, {2, 3}, {1, 3}}]},
        Abs[Total[IndexExpectationCurvature[s1]] - ComplexEulerCharacteristic[s1]] < 10^-10
    ],
    True,
    TestID -> "IndexCurvature-GaussBonnet-S1"
]


(* ===== FormanRicciCurvature ===== *)

VerificationTest[
    Values @ FormanRicciCurvature[CycleGraph[6]],
    ConstantArray[0, 6],
    TestID -> "FormanRicciCurvature-CycleGraph6-zero"
]

VerificationTest[
    Values @ FormanRicciCurvature[PathGraph[Range[5]]],
    {1, 0, 0, 1},
    TestID -> "FormanRicciCurvature-PathGraph5-leaf-one-interior-zero"
]

VerificationTest[
    Values @ FormanRicciCurvature[CompleteGraph[4]],
    ConstantArray[4, 6],
    TestID -> "FormanRicciCurvature-K4-default-triangles"
]

VerificationTest[
    Values @ FormanRicciCurvature[CompleteGraph[4], "MaxCellDimension" -> 1],
    ConstantArray[-2, 6],
    TestID -> "FormanRicciCurvature-K4-1skeleton"
]

VerificationTest[
    Values @ FormanRicciCurvature[CompleteGraph[4], "OnCells" -> 0],
    ConstantArray[0, 4],
    TestID -> "FormanRicciCurvature-K4-vertices-zero"
]

VerificationTest[
    Values @ FormanRicciCurvature[CompleteGraph[4], "OnCells" -> 2],
    ConstantArray[4, 4],
    TestID -> "FormanRicciCurvature-K4-triangles"
]

VerificationTest[
    Values @ FormanRicciCurvature[CompleteGraph[5], "OnCells" -> 2],
    ConstantArray[5, 10],
    TestID -> "FormanRicciCurvature-K5-triangles"
]

VerificationTest[
    With[{result = FormanRicciCurvature[CompleteGraph[4], "OnCells" -> All]},
        Keys[result]
    ],
    {0, 1, 2, 3},
    TestID -> "FormanRicciCurvature-K4-all-keys"
]

VerificationTest[
    With[{result = FormanRicciCurvature[CompleteGraph[4], "OnCells" -> {1, 2}]},
        {Keys[result], Length /@ Values[result]}
    ],
    {{1, 2}, {6, 4}},
    TestID -> "FormanRicciCurvature-K4-list-output-shape"
]

VerificationTest[
    Keys @ FormanRicciCurvature[CycleGraph[6]],
    EdgeList[CycleGraph[6]],
    TestID -> "FormanRicciCurvature-edge-keying-undirected-edges"
]


(* ===== GreenOperatorMatrix: Moore-Penrose pseudoinverse of the Hodge Laplacian ===== *)

VerificationTest[
    With[{g = ComplexClosure[{{1, 2, 3}, {3, 4}}]},
        With[{h = Normal @ HodgeBlock[g], gp = Normal @ GreenOperatorMatrix[g]},
            Max[Abs[h . gp . h - h]] < 10^-9 &&
            Max[Abs[gp . h . gp - gp]] < 10^-9 &&
            Max[Abs[h . gp - Transpose[h . gp]]] < 10^-9 &&
            Max[Abs[gp . h - Transpose[gp . h]]] < 10^-9
        ]
    ],
    True,
    TestID -> "GreenOperatorMatrix-MP-identities"
]

VerificationTest[
    With[{gr = CycleGraph[6]},
        With[{
            block0 = Normal @ GreenOperatorMatrix[GraphComplex[gr]][[;; VertexCount[gr], ;; VertexCount[gr]]],
            kp = PseudoInverse[N @ Normal @ KirchhoffMatrix[gr]]
        },
            Max[Abs[block0 - kp]] < 10^-9
        ]
    ],
    True,
    TestID -> "GreenOperatorMatrix-graph-0-block-equals-Kirchhoff-pseudoinverse"
]


(* ===== HodgePropagatorMatrix: Moore-Penrose pseudoinverse of d_k ===== *)

VerificationTest[
    With[{g = ComplexClosure[{{1, 2, 3}, {3, 4}}]},
        With[{
            d = Normal @ ComplexIncidenceMatrix[g, 0],
            p = Normal @ HodgePropagatorMatrix[g, 0]
        },
            Max[Abs[d . p . d - d]] < 10^-9 && Max[Abs[p . d . p - p]] < 10^-9
        ]
    ],
    True,
    TestID -> "HodgePropagatorMatrix-MP-d0"
]

VerificationTest[
    With[{g = ComplexClosure[{{1, 2, 3}, {3, 4}}]},
        With[{
            d = Normal @ ComplexIncidenceMatrix[g, 1],
            p = Normal @ HodgePropagatorMatrix[g, 1]
        },
            Max[Abs[d . p . d - d]] < 10^-9 && Max[Abs[p . d . p - p]] < 10^-9
        ]
    ],
    True,
    TestID -> "HodgePropagatorMatrix-MP-d1"
]

VerificationTest[
    Length @ HodgePropagatorMatrix[ComplexClosure[{{1, 2, 3}, {3, 4}}], All],
    ComplexDimension[ComplexClosure[{{1, 2, 3}, {3, 4}}]],
    TestID -> "HodgePropagatorMatrix-All-length"
]


(* ===== Ball-intersection complexes (Vietoris-Rips <-> Cech) ===== *)

(* equilateral triangle side 1: miniball radius = circumradius = 1/Sqrt[3] *)
VerificationTest[
    MiniballRadius[N @ {{0, 0}, {1, 0}, {1/2, Sqrt[3]/2}}],
    1/Sqrt[3.],
    SameTest -> (Abs[#1 - #2] < 10.^-9 &),
    TestID -> "MiniballRadius-equilateral-circumradius"
]

(* at 1/2 < r < 1/Sqrt[3] the triangle is a Rips simplex but not a Cech simplex *)
VerificationTest[
    With[{tri = N @ {{0, 0}, {1, 0}, {1/2, Sqrt[3]/2}}},
        {MemberQ[BallIntersectionComplex[tri, 0.55, 2], {1, 2, 3}],
         MemberQ[BallIntersectionComplex[tri, 0.55, Infinity], {1, 2, 3}]}
    ],
    {True, False},
    TestID -> "BallIntersectionComplex-triangle-discriminator"
]

(* order 2 reproduces Vietoris-Rips at scale 2 r (closed balls meet iff d <= 2 r) *)
VerificationTest[
    With[{pts = N @ CirclePoints[8]},
        Sort[BallIntersectionComplex[pts, 0.45, 2]] === Sort[VietorisRipsComplex[pts, 0.9]]
    ],
    True,
    TestID -> "BallIntersectionComplex-order2-equals-rips-at-2r"
]

(* every Cech simplex has a genuine common point: miniball <= r *)
VerificationTest[
    With[{pts = N @ {{0, 0}, {1, 0}, {1, 1}, {0, 1}, {1/2, 1/2}}},
        AllTrue[CechComplex[pts, 0.7], MiniballRadius[pts[[#]]] <= 0.7 + 10.^-9 &]
    ],
    True,
    TestID -> "CechComplex-every-simplex-has-common-point"
]

(* Helly ladder: C^(2) contains C^(3) and saturates to Cech at k = d + 1 = 3 in R^2 *)
VerificationTest[
    With[{pts = N @ CirclePoints[6], r = 0.65},
        With[{lad = Sort[BallIntersectionComplex[pts, r, #]] & /@ {2, 3, Infinity}},
            {SubsetQ[lad[[1]], lad[[2]]], lad[[2]] === lad[[3]]}
        ]
    ],
    {True, True},
    TestID -> "BallIntersectionComplex-helly-ladder-saturation-R2"
]

(* filtration value is monotone under faces, the legitimacy condition *)
VerificationTest[
    With[{sq = N @ {{0, 0}, {1, 0}, {1, 1}, {0, 1}, {1/2, 1/2}}},
        With[{fk = BallIntersectionFiltrationValue[sq, #, 2] &},
            fk[{1, 2}] <= fk[{1, 2, 3}] + 10.^-12
        ]
    ],
    True,
    TestID -> "BallIntersectionFiltrationValue-monotone-under-faces"
]

(* order-2 persistence equals Rips persistence; ball is keyed by r and Rips by 2 r,
   so diagrams coincide after scaling births/deaths by 2 *)
VerificationTest[
    With[{pts = N @ CirclePoints[8], radii = Range[0.1, 0.7, 0.1]},
        With[{b = Sort[PersistenceDiagram[BallIntersectionFiltration[pts, radii, 2]] /. x_Real :> 2 x],
              v = Sort[PersistenceDiagram[VietorisRipsFiltration[pts, 2 * radii]]]},
            Length[b] === Length[v] && Max[Abs[Cases[b - v, _ ? NumericQ, Infinity]]] < 10.^-9
        ]
    ],
    True,
    TestID -> "BallIntersectionFiltration-order2-persistence-matches-rips"
]

(* intrinsic graph-metric oracle: on C6 (non-convex balls) Helly does not collapse *)
VerificationTest[
    With[{m = GraphDistanceMatrix[CycleGraph[6]], v = Range[6]},
        With[{rips = Sort[BallIntersectionComplex[v, 2, 2, "Metric" -> m]],
              cech = Sort[BallIntersectionComplex[v, 2, Infinity, "Metric" -> m]]},
            SubsetQ[rips, cech] && rips =!= cech
        ]
    ],
    True,
    TestID -> "BallIntersectionComplex-metric-oracle-no-collapse"
]

(* quality knob: a measure threshold on the common region refines Cech *)
VerificationTest[
    With[{sq = N @ {{0, 0}, {1, 0}, {1, 1}, {0, 1}, {1/2, 1/2}}, r = 0.72},
        SubsetQ[
            Sort[CechComplex[sq, r]],
            Sort[BallIntersectionComplex[sq, r, Infinity, "IntersectionTest" -> (RegionMeasure[#] >= 0.05 &)]]
        ]
    ],
    True,
    TestID -> "BallIntersectionComplex-quality-measure-refines-cech"
]


(* ===== Persistent homology correctness (vs BettiVector oracle) ===== *)

(* infinite bars of a one-step filtration = Betti numbers; must agree with
   BettiVector, including the TOP dimension (H2 of a sphere) and with no
   over-counting of destroyer simplices in dimensions >= 1 *)
VerificationTest[
    Module[{betti = Values[Count[#, {_, Infinity}] &
        /@ PersistenceIntervals[<|0. -> #|>]] &},
        {betti[GraphComplex[CycleGraph[6]]],
         betti[ComplexClosure[{{1, 2, 3}}]],
         betti[ComplexClosure[{{1, 3, 5}, {1, 3, 6}, {1, 4, 5}, {1, 4, 6}, {2, 3, 5}, {2, 3, 6}, {2, 4, 5}, {2, 4, 6}}]],
         betti[ComplexClosure[{{1, 2, 3}, {4, 5, 6}}]]}
    ],
    {{1, 1}, {1, 0, 0}, {1, 0, 1}, {2, 0, 0}},
    TestID -> "PersistenceIntervals-betti-matches-BettiVector"
]

(* a clean VR circle has exactly one persistent H1 generator (no spurious bars) *)
VerificationTest[
    With[{circ = Table[{Cos[t], Sin[t]}, {t, 0., 2 Pi - 0.01, 2 Pi/12}]},
        Length[PersistenceIntervals[VietorisRipsFiltration[circ, Range[0.2, 2.2, 0.2]]][1]]
    ],
    1,
    TestID -> "PersistenceIntervals-circle-one-H1"
]

(* nerve of chosen graph-metric centres: B(2),B(4),B(6) at r=1 on the path meet
   consecutively (shared vertices 3, 5) but the ends miss, giving the path complex *)
VerificationTest[
    Sort @ BallIntersectionComplex[{2, 4, 6}, 1, Infinity, "Metric" -> PathGraph[Range[7]]],
    {{1}, {2}, {3}, {1, 2}, {2, 3}},
    TestID -> "BallIntersectionComplex-graph-chosen-centres"
]

EndTestSection[]

EndTestSection[]
