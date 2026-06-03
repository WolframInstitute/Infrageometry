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

(* ===== 5a. Mesh Tests ===== *)

VerificationTest[
    With[{g = InteriorMeshGraph @ MeshRegion[
        {{0}, {1}, {2}, {3}},
        Line /@ {{1, 2}, {2, 3}, {3, 4}}
    ]},
        Sort[VertexList[g]] == {1, 2, 3, 4} && Sort[List @@@ EdgeList[g]] == {{1, 2}, {2, 3}, {3, 4}}
    ],
    True,
    TestID -> "InteriorMeshGraph-Path"
]

VerificationTest[
    With[{g = InteriorMeshGraph @ MeshRegion[
        {{0, 0}, {1, 0}, {1, 1}, {0, 1}, {1/2, 1/2}},
        Triangle /@ {{1, 2, 5}, {2, 3, 5}, {3, 4, 5}, {4, 1, 5}}
    ]},
        Sort[VertexList[g]] == {1, 2, 3, 4, 5} && Sort[Sort /@ (List @@@ EdgeList[g])] == {{1, 5}, {2, 5}, {3, 5}, {4, 5}}
    ],
    True,
    TestID -> "InteriorMeshGraph-Disk"
]

VerificationTest[
    With[{g = InteriorMeshGraph @ MeshRegion[
        {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}},
        Triangle /@ {{1, 2, 3}, {1, 2, 4}, {1, 3, 4}, {2, 3, 4}}
    ]},
        Sort[VertexList[g]] == {1, 2, 3, 4} && Length[EdgeList[g]] == 6
    ],
    True,
    TestID -> "InteriorMeshGraph-ClosedSurface"
]

VerificationTest[
    (* solid tetrahedron split into 4 sub-tets around interior vertex 5: only the 4 spokes survive *)
    With[{g = InteriorMeshGraph @ MeshRegion[
        {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {1/4, 1/4, 1/4}},
        Tetrahedron /@ {{1, 2, 3, 5}, {1, 2, 4, 5}, {1, 3, 4, 5}, {2, 3, 4, 5}}
    ]},
        Sort[VertexList[g]] == {1, 2, 3, 4, 5} && Sort[Sort /@ (List @@@ EdgeList[g])] == {{1, 5}, {2, 5}, {3, 5}, {4, 5}}
    ],
    True,
    TestID -> "InteriorMeshGraph-SolidVolume"
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


(* ===== OllivierRicciCurvature ===== *)

VerificationTest[
    Values @ OllivierRicciCurvature[CompleteGraph[4]],
    ConstantArray[2 / 3, 6],
    SameTest -> (Max @ Abs[#1 - #2] < 10^-8 &),
    TestID -> "OllivierRicciCurvature-K4"
]

VerificationTest[
    Values @ OllivierRicciCurvature[PathGraph[Range[5]]],
    ConstantArray[0, 4],
    SameTest -> (Max @ Abs[#1 - #2] < 10^-8 &),
    TestID -> "OllivierRicciCurvature-P5-zero"
]

VerificationTest[
    Values @ OllivierRicciCurvature[CycleGraph[6]],
    ConstantArray[0, 6],
    SameTest -> (Max @ Abs[#1 - #2] < 10^-8 &),
    TestID -> "OllivierRicciCurvature-C6-zero"
]


(* ===== WolframRicciCurvature ===== *)

(* vertex slot 2, radius slot 3; a single vertex gives a scalar *)
VerificationTest[
    WolframRicciCurvature[CycleGraph[12], 1, {1, 3}, "Dimension" -> 1],
    Mean[{-9., -1.125, -1./3}],
    SameTest -> (Abs[#1 - #2] < 10^-8 &),
    TestID -> "WolframRicciCurvature-C12-d1-window-mean"
]

VerificationTest[
    WolframRicciCurvature[CycleGraph[12], 1, 2, "Dimension" -> 1],
    -1.125,
    SameTest -> (Abs[#1 - #2] < 10^-8 &),
    TestID -> "WolframRicciCurvature-C12-d1-single-radius"
]

(* All / no vertex arg gives a list over VertexList[g] *)
VerificationTest[
    ListQ @ WolframRicciCurvature[GridGraph[{5, 5}]],
    True,
    TestID -> "WolframRicciCurvature-Grid5x5-default-list"
]

VerificationTest[
    Length @ WolframRicciCurvature[GridGraph[{5, 5}]],
    25,
    TestID -> "WolframRicciCurvature-Grid5x5-all-vertices"
]

(* the All-form is the single-vertex scalar form mapped over VertexList[g] *)
VerificationTest[
    WolframRicciCurvature[PathGraph[Range[7]]],
    WolframRicciCurvature[PathGraph[Range[7]], #] & /@ Range[7],
    TestID -> "WolframRicciCurvature-P7-all-equals-per-vertex"
]

(* a vertex list gives a list of matching length *)
VerificationTest[
    Length @ WolframRicciCurvature[GridGraph[{5, 5}], {1, 2, 3}],
    3,
    TestID -> "WolframRicciCurvature-Grid5x5-vertex-list"
]

VerificationTest[
    WolframRicciCurvature[HypercubeGraph[3], 1, {5, 7}, "Dimension" -> 3],
    Indeterminate,
    TestID -> "WolframRicciCurvature-Q3-empty-window-indeterminate"
]


(* ===== WolframHausdorffDimension ===== *)

VerificationTest[
    ListQ @ WolframHausdorffDimension[GridGraph[{5, 5}]],
    True,
    TestID -> "WolframHausdorffDimension-Grid5x5-list"
]

VerificationTest[
    Length @ WolframHausdorffDimension[GridGraph[{5, 5}]],
    25,
    TestID -> "WolframHausdorffDimension-Grid5x5-all-vertices"
]

VerificationTest[
    WolframHausdorffDimension[PathGraph[Range[7]]],
    WolframHausdorffDimension[PathGraph[Range[7]], #] & /@ Range[7],
    TestID -> "WolframHausdorffDimension-P7-all-equals-per-vertex"
]

(* d(v, r) = (Log V(r+1) - Log V(r)) / (Log(r+1) - Log r); on a cycle V(r) = 2r+1 *)
VerificationTest[
    WolframHausdorffDimension[CycleGraph[40], 1, 10],
    N[(Log[23] - Log[21]) / (Log[11] - Log[10])],
    TestID -> "WolframHausdorffDimension-C40-single-radius-formula"
]

(* a long cycle is 1-dimensional: volume-growth dimension -> 1 as r grows *)
VerificationTest[
    Abs[WolframHausdorffDimension[CycleGraph[60], 1, 15] - 1] < 0.1,
    True,
    TestID -> "WolframHausdorffDimension-C60-approaches-one"
]

(* window past ecc(v) - 1 has no valid radii *)
VerificationTest[
    WolframHausdorffDimension[HypercubeGraph[3], 1, {5, 7}],
    Indeterminate,
    TestID -> "WolframHausdorffDimension-Q3-empty-window-indeterminate"
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


(* ===== EffectiveResistance: closed-form values ===== *)

VerificationTest[
    Chop[EffectiveResistance[PathGraph[Range[6]], 1, 6] - 5, 10^-10] == 0,
    True,
    TestID -> "EffectiveResistance-Path-endpoints"
]

VerificationTest[
    Chop[EffectiveResistance[PathGraph[Range[5]], 2, 4] - 2, 10^-10] == 0,
    True,
    TestID -> "EffectiveResistance-Path-interior"
]

VerificationTest[
    With[{r = EffectiveResistance[CompleteGraph[5]]},
        Max[Abs[(r + DiagonalMatrix[ConstantArray[2/5, 5]]) - ConstantArray[2/5, {5, 5}]]] < 10^-10
    ],
    True,
    TestID -> "EffectiveResistance-K5-uniform"
]

VerificationTest[
    Chop[EffectiveResistance[CycleGraph[6], 1, 4] - 6/4, 10^-10] == 0,
    True,
    TestID -> "EffectiveResistance-C6-antipodal"
]

VerificationTest[
    With[{tree = Graph[{1 <-> 2, 2 <-> 3, 3 <-> 4, 2 <-> 5}]},
        Max[Abs[EffectiveResistance[tree] - GraphDistanceMatrix[tree]]] < 10^-10
    ],
    True,
    TestID -> "EffectiveResistance-tree-equals-distance"
]

VerificationTest[
    With[{r = EffectiveResistance[PetersenGraph[]]},
        Max[Abs[r - Transpose[r]]] < 10^-10 && Max[Abs @ Diagonal[r]] < 10^-10
    ],
    True,
    TestID -> "EffectiveResistance-symmetry"
]

(* Foster's theorem: sum over edges = n - 1 *)
VerificationTest[
    With[{g = PetersenGraph[]},
        Chop[Total[EffectiveResistance[g, #1, #2] & @@@ (List @@@ EdgeList[g])] - (VertexCount[g] - 1), 10^-9] == 0
    ],
    True,
    TestID -> "Foster-Petersen"
]

VerificationTest[
    With[{g = GridGraph[{3, 3}]},
        Chop[Total[EffectiveResistance[g, #1, #2] & @@@ (List @@@ EdgeList[g])] - (VertexCount[g] - 1), 10^-9] == 0
    ],
    True,
    TestID -> "Foster-Grid3x3"
]

(* Triangle inequality for sqrt(R) *)
VerificationTest[
    With[{g = PetersenGraph[], n = 10},
        With[{r = Sqrt @ EffectiveResistance[g]},
            Max[Flatten @ Table[r[[i, k]] - r[[i, j]] - r[[j, k]], {i, n}, {j, n}, {k, n}]] < 10^-9
        ]
    ],
    True,
    TestID -> "ResistanceMetric-triangle-Petersen"
]

VerificationTest[
    Dimensions @ EffectiveResistance[PetersenGraph[], {1, 3, 5}],
    {3, 3},
    TestID -> "EffectiveResistance-submatrix-shape"
]


(* ===== ResistanceQ ===== *)

VerificationTest[
    ResistanceQ @ EffectiveResistance @ PetersenGraph[],
    True,
    TestID -> "ResistanceQ-Petersen"
]

VerificationTest[
    ResistanceQ[{{0, 1, 5}, {1, 0, 1}, {5, 1, 0}}],
    False,
    TestID -> "ResistanceQ-violates-negative-type"
]

VerificationTest[
    ResistanceQ["nonsense"],
    False,
    TestID -> "ResistanceQ-non-matrix"
]


(* ===== Unit-length discretization & embedding ===== *)

ulEdgeLengths[g_] := With[{p = GraphEmbedding[g]},
    EuclideanDistance[p[[#[[1]]]], p[[#[[2]]]]] & /@ (List @@@ EdgeList[g])
]

VerificationTest[
    With[{e = UnitLengthEmbedding[CycleGraph[6]]}, MatrixQ[e] && Dimensions[e] === {6, 3}],
    True,
    TestID -> "UnitLengthEmbedding-matrix-shape"
]

VerificationTest[
    With[{g = CycleGraph[6], e = UnitLengthEmbedding[CycleGraph[6]]},
        Max @ Abs[(EuclideanDistance[e[[#[[1]]]], e[[#[[2]]]]] & /@ (List @@@ EdgeList[g])) - 1] < 0.01
    ],
    True,
    TestID -> "UnitLengthEmbedding-unit-edges"
]

VerificationTest[
    Dimensions @ UnitLengthEmbedding[GridGraph[{3, 3}], "Dimension" -> 2],
    {9, 2},
    TestID -> "UnitLengthEmbedding-dimension-option"
]

VerificationTest[
    With[{g = UnitLengthGraph[Sphere[], 40]},
        GraphQ[g] && VertexCount[g] == 40 && Mean[N @ VertexDegree[g]] >= 4
    ],
    True,
    TestID -> "UnitLengthGraph-sphere-jammed"
]

VerificationTest[
    With[{lens = ulEdgeLengths @ UnitLengthGraph[Sphere[], 40]}, Max[lens] / Min[lens] < 1.8],
    True,
    TestID -> "UnitLengthGraph-sphere-edges-near-uniform"
]

VerificationTest[
    Block[{}, SeedRandom[1];
        With[{g = UnitLengthGraph[Circle[{0, 0}, 1], 20]},
            GraphQ[g] && VertexCount[g] == 20 && Mean[N @ VertexDegree[g]] > 1.8
        ]
    ],
    True,
    TestID -> "UnitLengthGraph-circle-cycle"
]

VerificationTest[
    Block[{}, SeedRandom[3];
        With[{lens = ulEdgeLengths @ Quiet @ UnitLengthGraph[Circle[{0, 0}, 1], 12, Method -> "ConstrainedPacking"]},
            Max[lens] - Min[lens] < 0.05
        ]
    ],
    True,
    TestID -> "UnitLengthGraph-constrained-uniform"
]


(* ===== GraphBoundary & GraphInterior ===== *)

(* Worked example: path 1-2-3-4-5, S = {2,3,4} -> boundary {2,4}, interior {3} *)
VerificationTest[
    With[{g = PathGraph[Range[5]]},
        {GraphBoundary[g, {2, 3, 4}], GraphInterior[g, {2, 3, 4}]}
    ],
    {{2, 4}, {3}},
    TestID -> "GraphBoundary-path-arc"
]

(* Partition: boundary and interior are disjoint and union to S *)
VerificationTest[
    With[{g = GridGraph[{4, 4}], s = {1, 2, 3, 6, 7, 11}},
        {Sort @ Union[GraphBoundary[g, s], GraphInterior[g, s]],
         Intersection[GraphBoundary[g, s], GraphInterior[g, s]]}
    ],
    {{1, 2, 3, 6, 7, 11}, {}},
    TestID -> "GraphBoundary-partition"
]

(* Interior characterization: every interior vertex has all neighbors inside S *)
VerificationTest[
    With[{g = GridGraph[{4, 4}], s = {1, 2, 3, 6, 7, 11}},
        AllTrue[GraphInterior[g, s], SubsetQ[s, AdjacencyList[g, #]] &]
    ],
    True,
    TestID -> "GraphInterior-characterization"
]

(* Boundary characterization: every boundary vertex has a neighbor outside S *)
VerificationTest[
    With[{g = GridGraph[{4, 4}], s = {1, 2, 3, 6, 7, 11}},
        AllTrue[GraphBoundary[g, s], IntersectingQ[AdjacencyList[g, #], Complement[VertexList[g], s]] &]
    ],
    True,
    TestID -> "GraphBoundary-characterization"
]

(* Full vertex set: boundary empty, interior is everything *)
VerificationTest[
    With[{g = GridGraph[{3, 3}], v = VertexList @ GridGraph[{3, 3}]},
        {GraphBoundary[g, v], Sort @ GraphInterior[g, v]}
    ],
    {{}, Sort @ VertexList @ GridGraph[{3, 3}]},
    TestID -> "GraphBoundary-full-set"
]

(* Grid plus-shape: center is interior, the four arms are boundary *)
VerificationTest[
    With[{g = GridGraph[{3, 3}]},
        {Sort @ GraphBoundary[g, {2, 4, 5, 6, 8}], GraphInterior[g, {2, 4, 5, 6, 8}]}
    ],
    {{2, 4, 6, 8}, {5}},
    TestID -> "GraphBoundary-grid-plus"
]

(* Cycle arc: the two ends are boundary, the middle is interior *)
VerificationTest[
    With[{g = CycleGraph[6]},
        {Sort @ GraphBoundary[g, {1, 2, 3}], GraphInterior[g, {1, 2, 3}]}
    ],
    {{1, 3}, {2}},
    TestID -> "GraphBoundary-cycle-arc"
]

(* Subgraph form agrees with the vertex-list form *)
VerificationTest[
    With[{g = GridGraph[{4, 4}], s = {1, 2, 3, 6, 7, 11}},
        GraphBoundary[g, Subgraph[g, s]] === GraphBoundary[g, s] &&
        GraphInterior[g, Subgraph[g, s]] === GraphInterior[g, s]
    ],
    True,
    TestID -> "GraphBoundary-subgraph-form"
]

(* Empty subset; isolated vertex is interior *)
VerificationTest[
    With[{g = Graph[{1, 2, 3, 4}, {1 <-> 2, 2 <-> 3}]},
        {GraphBoundary[g, {}], GraphInterior[g, {}],
         GraphBoundary[g, {1, 4}], GraphInterior[g, {1, 4}]}
    ],
    {{}, {}, {1}, {4}},
    TestID -> "GraphBoundary-empty-isolated"
]

(* Complement identity: outer boundary of S = inner boundary of V\S *)
VerificationTest[
    With[{g = PathGraph[Range[5]], s = {2, 3, 4}},
        With[{outer = GraphBoundary[g, Complement[VertexList[g], s]]},
            outer === {1, 5} &&
            AllTrue[outer, ! MemberQ[s, #] && IntersectingQ[AdjacencyList[g, #], s] &]
        ]
    ],
    True,
    TestID -> "GraphBoundary-complement-identity"
]


(* ===== BallHull ===== *)

(* Defining property: w is in BallHull[g, S] iff d(c, w) <= max_{s in S} d(c, s)
   for every center c -- the intersection of every smallest enclosing ball. *)
VerificationTest[
    With[{g = GridGraph[{6, 4}], s = {1, 6, 22}},
        BallHull[g, s] === Fold[Intersection, VertexList[g],
            Table[With[{r = Max[GraphDistance[g, c, #] & /@ s]},
                Select[VertexList[g], GraphDistance[g, c, #] <= r &]], {c, VertexList[g]}]]
    ],
    True,
    TestID -> "BallHull-equals-ball-intersection"
]

(* Closure operator: extensive (contains S) and idempotent. *)
VerificationTest[
    With[{g = GridGraph[{6, 4}], s = {1, 6, 22}},
        {SubsetQ[BallHull[g, s], s], BallHull[g, BallHull[g, s]] === BallHull[g, s]}
    ],
    {True, True},
    TestID -> "BallHull-extensive-idempotent"
]

(* A singleton and a closed ball are ball-convex (equal to their own hull). *)
VerificationTest[
    With[{g = GridGraph[{5, 5}]},
        {BallHull[g, {13}] === {13},
         With[{ball = Sort @ Select[VertexList[g], GraphDistance[g, 13, #] <= 2 &]},
            Sort @ BallHull[g, ball] === ball]}
    ],
    {True, True},
    TestID -> "BallHull-singleton-and-ball-fixed"
]

(* Subgraph form agrees with the vertex-list form. *)
VerificationTest[
    With[{g = GridGraph[{6, 4}], s = {1, 6, 22}},
        BallHull[g, Subgraph[g, s]] === BallHull[g, s]
    ],
    True,
    TestID -> "BallHull-subgraph-form"
]


(* ===== Alexandrov topology: BallTopology / Topological* / ContinuousMapQ ===== *)

(* Carrier set is recoverable from the preorder digraph alone, incl. isolated vertices *)
VerificationTest[
    With[{g = Graph[{1, 2, 3, 4, 5}, {1 <-> 2, 2 <-> 3, 3 <-> 4}]},
        Sort @ VertexList @ BallTopology[g, 1] === Sort @ VertexList[g]
    ],
    True,
    TestID -> "BallTopology-carrier"
]

(* int(S) subset S subset cl(S) *)
VerificationTest[
    With[{topo = BallTopology[GridGraph[{4, 4}], 1], s = {1, 2, 3, 6, 7}},
        SubsetQ[s, TopologicalInterior[topo, s]] && SubsetQ[TopologicalClosure[topo, s], s]
    ],
    True,
    TestID -> "Topological-sandwich"
]

(* Duality: cl(V\S) == V \ int(S) *)
VerificationTest[
    With[{topo = BallTopology[CycleGraph[8], 1], s = {1, 2, 3}},
        With[{v = VertexList[topo]},
            Sort @ TopologicalClosure[topo, Complement[v, s]] === Sort @ Complement[v, TopologicalInterior[topo, s]]
        ]
    ],
    True,
    TestID -> "Topological-duality"
]

(* Boundary is two-sided: bd(S) = cl(S)\int(S) and bd(S) = bd(V\S) *)
VerificationTest[
    With[{topo = BallTopology[GridGraph[{4, 4}], 1], s = {1, 2, 3, 6, 7}},
        With[{v = VertexList[topo]},
            Sort @ TopologicalBoundary[topo, s] === Sort @ Complement[TopologicalClosure[topo, s], TopologicalInterior[topo, s]] &&
            Sort @ TopologicalBoundary[topo, s] === Sort @ TopologicalBoundary[topo, Complement[v, s]]
        ]
    ],
    True,
    TestID -> "Topological-boundary-two-sided"
]

(* Alexandrov idempotence: cl(cl(S)) = cl(S), int(int(S)) = int(S) *)
VerificationTest[
    With[{topo = BallTopology[GridGraph[{4, 4}], 2], s = {1, 2, 3, 6, 7}},
        With[{cl = TopologicalClosure[topo, s], int = TopologicalInterior[topo, s]},
            Sort @ TopologicalClosure[topo, cl] === Sort @ cl &&
            Sort @ TopologicalInterior[topo, int] === Sort @ int
        ]
    ],
    True,
    TestID -> "Topological-idempotence"
]

(* Minimal open neighborhood is the up-set; contains S and is open (= its own interior) *)
VerificationTest[
    With[{topo = BallTopology[CycleGraph[8], 1], s = {1, 4}},
        With[{nb = TopologicalNeighborhood[topo, s]},
            SubsetQ[nb, s] && Sort @ TopologicalInterior[topo, nb] === Sort @ nb
        ]
    ],
    True,
    TestID -> "Topological-neighborhood-open"
]

(* ContinuousMapQ: identity is continuous topo->topo, but not topo->dual (edges reversed) *)
VerificationTest[
    With[{topo = BallTopology[PathGraph[Range[5]], 1], dual = BallTopology[PathGraph[Range[5]], 1, "Dual" -> True]},
        With[{id = AssociationMap[Identity, VertexList[topo]]},
            {ContinuousMapQ[id, topo, topo], ContinuousMapQ[id, topo, dual]}
        ]
    ],
    {True, False},
    TestID -> "ContinuousMapQ-identity-vs-dual"
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

(* ===== Coordinatization: resolving sets, radar / resistance coords, ball covers ===== *)

VerificationTest[
    ResolvingSetQ[PathGraph[Range[5]], {1}],
    True,
    TestID -> "ResolvingSetQ-path-endpoint-resolves"
]

VerificationTest[
    ResolvingSetQ[CycleGraph[6], {1}],
    False,
    TestID -> "ResolvingSetQ-cycle-single-fails"
]

VerificationTest[
    ResolvingSetQ[CycleGraph[6], {1, 2}],
    True,
    TestID -> "ResolvingSetQ-cycle-two-resolves"
]

VerificationTest[
    MetricDimension[PathGraph[Range[5]]],
    1,
    TestID -> "MetricDimension-path"
]

VerificationTest[
    MetricDimension[CycleGraph[6]],
    2,
    TestID -> "MetricDimension-cycle"
]

VerificationTest[
    MetricDimension[PetersenGraph[]],
    3,
    TestID -> "MetricDimension-Petersen"
]

VerificationTest[
    RadarCoordinates[PathGraph[Range[5]], {1, 5}, 3],
    {2, 2},
    TestID -> "RadarCoordinates-path-vertex"
]

VerificationTest[
    With[{c = ResistanceCoordinates[PathGraph[Range[4]]]},
        Chop[Total[(c[1] - c[4])^2] - EffectiveResistance[PathGraph[Range[4]], 1, 4]]
    ],
    0,
    TestID -> "ResistanceCoordinates-matching-identity"
]

VerificationTest[
    DominationNumber[CycleGraph[7], 2],
    2,
    TestID -> "DominationNumber-C7-r2"
]

VerificationTest[
    DominationNumber[PetersenGraph[], 1],
    3,
    TestID -> "DominationNumber-Petersen-r1"
]

VerificationTest[
    BallCoverQ[CycleGraph[7], 2, FindBallCover[CycleGraph[7], 2]],
    True,
    TestID -> "FindBallCover-covers-C7"
]

VerificationTest[
    BallCoverQ[CycleGraph[7], 1, {1}],
    False,
    TestID -> "BallCoverQ-single-ball-too-small"
]

(* a cover of a vertex subset covers its targets and can be smaller than a full cover *)
VerificationTest[
    With[{g = PathGraph[Range[7]]},
        {BallCoverQ[g, 1, FindBallCover[g, 1, {1, 7}], {1, 7}], DominationNumber[g, 1, {1, 7}] < DominationNumber[g, 1]}
    ],
    {True, True},
    TestID -> "FindBallCover-subset-covers-targets"
]

(* nerve of chosen graph-metric centres: B(2),B(4),B(6) at r=1 on the path meet
   consecutively (shared vertices 3, 5) but the ends miss, giving the path complex *)
VerificationTest[
    Sort @ BallIntersectionComplex[{2, 4, 6}, 1, Infinity, "Metric" -> PathGraph[Range[7]]],
    {{1}, {2}, {3}, {1, 2}, {2, 3}},
    TestID -> "BallIntersectionComplex-graph-chosen-centres"
]


EndTestSection[]
