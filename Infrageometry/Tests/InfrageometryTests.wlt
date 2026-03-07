BeginTestSection["InfrageometryTests"]

(* ===== Test Fixtures ===== *)

(* Triangle complex: {{1},{2},{3},{1,2},{1,3},{2,3},{1,2,3}} *)
triangle = ComplexClosure[{{1, 2, 3}}]
(* Edge: {{1},{2},{1,2}} *)
edge = ComplexClosure[{{1, 2}}]
(* Tetrahedron *)
tetrahedron = ComplexClosure[{{1, 2, 3, 4}}]
(* Empty complex *)
empty = {}
(* Two disjoint edges *)
twoEdges = ComplexClosure[{{1, 2}, {3, 4}}]
(* Path graph (3 vertices) *)
path3 = ComplexClosure[{{1, 2}, {2, 3}}]
(* Circular graph C4 *)
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

VerificationTest[
    IndexHypergraph[{{"a", "b"}, {"b", "c"}}],
    {{1, 2}, {2, 3}},
    TestID -> "IndexHypergraph-Basic"
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
    (* Barycentric refinement produces a finer complex *)
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
    (* D is symmetric *)
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
    (* Euler = alternating sum of Betti numbers for S^1 *)
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
    (* One geodesic step on a triangle from frame {1,2,3} *)
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
    (* Orbit is a list of frames *)
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
(* Quantum Calculus — New functionality tests                       *)
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
    (* Wave propagator is square matrix of correct dimension *)
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
    (* DiscreteWaveStep returns pair of vectors *)
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
    (* DiscreteWaveStep functional form returns a Function *)
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
    (* Default m=2 *)
    WuCharacteristic[triangle] == WuCharacteristic[triangle, 2],
    True,
    TestID -> "Wu-DefaultOrder"
]

(* ===== 23. Isospectral Deformation ===== *)

VerificationTest[
    (* Deformed Dirac is a square matrix *)
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
    (* DiracMass returns a vector *)
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
    (* Connes distance is a square matrix *)
    With[{d = ConnesDistance[edge]},
        SquareMatrixQ[d] && Dimensions[d] == {2, 2}
    ],
    True,
    TestID -> "ConnesDistance-SquareMatrix"
]

VerificationTest[
    (* Connes distance is symmetric *)
    With[{d = ConnesDistance[edge]},
        Max[Abs[d - Transpose[d]]] < 10^-10
    ],
    True,
    TestID -> "ConnesDistance-Symmetric"
]

VerificationTest[
    (* Diagonal is zero (self-distance) *)
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
    (* Lefschetz zeta for identity permutation *)
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
    (* Gauss-Bonnet for tetrahedron *)
    Abs[Total[IndexExpectationCurvature[tetrahedron]] - ComplexEulerCharacteristic[tetrahedron]] < 10^-10,
    True,
    TestID -> "IndexCurvature-GaussBonnet-Tetra"
]

VerificationTest[
    (* Gauss-Bonnet for S^1 (chi=0) *)
    With[{s1 = ComplexClosure[{{1, 2}, {2, 3}, {1, 3}}]},
        Abs[Total[IndexExpectationCurvature[s1]] - ComplexEulerCharacteristic[s1]] < 10^-10
    ],
    True,
    TestID -> "IndexCurvature-GaussBonnet-S1"
]

EndTestSection[]
