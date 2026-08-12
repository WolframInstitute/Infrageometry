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


(* ===== Synthetic invariants: BallVolumes / ShellAreas / LogDifferenceQuotients ===== *)

(* the closed ball volume is the cumulative shell-area series *)
VerificationTest[
    BallVolumes[CycleGraph[10], 1] === Accumulate[ShellAreas[CycleGraph[10], 1]],
    True,
    TestID -> "BallVolumes-accumulates-ShellAreas"
]

(* a fixed finite radius window over a vertex list is rectangular: every row has
   length rmax - rmin + 1 (this is the contract that makes subset statistics Transpose) *)
VerificationTest[
    Dimensions[BallVolumes[GridGraph[{5, 5}], All, {0, 6}]],
    {25, 7},
    TestID -> "BallVolumes-fixed-window-rectangular"
]

(* shells past eccentricity pad with 0 (empty sphere) *)
VerificationTest[
    ShellAreas[PathGraph[Range[5]], 1, {0, 8}],
    {1, 1, 1, 1, 1, 0, 0, 0, 0},
    TestID -> "ShellAreas-pad-zero-past-ecc"
]

(* the log-difference quotient of a clean power law r^d recovers the exponent d *)
VerificationTest[
    Round[Last @ LogDifferenceQuotients[N[Range[1, 20]^3]], 0.001],
    3.,
    TestID -> "LogDifferenceQuotients-power-law-exponent"
]

(* "Front" measure: V(t) = sum_{s<=t} |S_s(v)|.  On a path from an endpoint the front
   is a single vertex marching out and reflecting, so |S_s| == 1 and V(t) == t + 1 *)
VerificationTest[
    BallVolumes[PathGraph[Range[7]], 1, {0, 12}, "Measure" -> "Front"],
    Range[1, 13],
    TestID -> "BallVolumes-Front-single-vertex-on-path"
]

(* the front starts as the origin alone, so V(0) == 1 for any graph *)
VerificationTest[
    BallVolumes[GridGraph[{4, 4}], 5, 0, "Measure" -> "Front"],
    1,
    TestID -> "BallVolumes-Front-V0-is-one"
]

(* unlike the metric ball (which saturates at the component size past eccentricity),
   the momentum front keeps sweeping, so V(2 ecc) strictly exceeds |component| *)
VerificationTest[
    Last[BallVolumes[CycleGraph[8], 1, All, "Measure" -> "Front"]] > VertexCount[CycleGraph[8]],
    True,
    TestID -> "BallVolumes-Front-propagates-past-ecc"
]

(* V is the running total of the front sizes, which are >= 1 (the front never empties):
   the step differences are exactly the foliation cardinalities, all positive *)
VerificationTest[
    With[{v = BallVolumes[CycleGraph[10], 1, {0, 15}, "Measure" -> "Front"]},
        Min[Differences[v]] >= 1 && OrderedQ[v]],
    True,
    TestID -> "BallVolumes-Front-monotone-never-empties"
]

(* slope-of-mean: average the volume profiles over the vertex slot, then one slope.
   On a vertex-transitive graph every profile is identical, so it equals the
   single-vertex slope -- aggregation is caller-side composition, no option *)
VerificationTest[
    Max @ Abs[LogDifferenceQuotients[Mean /@ Transpose[BallVolumes[CycleGraph[40], All, {0, 18}]]]
              - LogDifferenceQuotients[BallVolumes[CycleGraph[40], 1, {0, 18}]]] < 10.^-10,
    True,
    TestID -> "LogDifferenceQuotients-slope-of-mean-vertex-transitive"
]

(* averaging over a vertex SUBSET reproduces the single-vertex slope on a transitive graph *)
VerificationTest[
    Max @ Abs[LogDifferenceQuotients[Mean /@ Transpose[BallVolumes[CycleGraph[40], {1, 5, 9}, {0, 18}]]]
              - LogDifferenceQuotients[BallVolumes[CycleGraph[40], 1, {0, 18}]]] < 10.^-10,
    True,
    TestID -> "LogDifferenceQuotients-subset-aggregation"
]

(* MeanAround carries the per-vertex spread into Around; central values match Mean *)
VerificationTest[
    With[{w = Transpose[BallVolumes[GridGraph[{5, 5}], All, {0, 6}]]},
        Max @ Abs[(#[[1]] & /@ LogDifferenceQuotients[MeanAround /@ w]) - LogDifferenceQuotients[Mean /@ w]] < 10.^-10
    ],
    True,
    TestID -> "LogDifferenceQuotients-MeanAround-central-equals-Mean"
]


(* ===== VolumeGrowthObservables: joined ball + sphere growth fit (flat assoc) ===== *)

(* endpoint of a path: shell area A(r) = 1 (one vertex per distance) is a pure r^0 power,
   so the sphere probe reads the manifold dimension n = 1 exactly with S = 0 (the ball probe
   is no longer exact here: the Hausdorff boundary correction distorts a graph this small) *)
VerificationTest[
    With[{r = VolumeGrowthObservables[PathGraph[Range[5]], 1]},
        {Chop[r["SphereDimension"] - 1], Chop @ r["SphereScalarCurvature"]}
    ],
    {0, 0},
    TestID -> "VolumeGrowthObservables-P5-sphere-exact-line"
]

(* cycle: d = 1, R = 0; the Automatic window drops the small-r preamble bias
   q(r) ~ 1 + 1/r and beats the full-range fit on both estimates *)
VerificationTest[
    With[{auto = VolumeGrowthObservables[CycleGraph[40], 1], full = VolumeGrowthObservables[CycleGraph[40], 1, All]},
        {
            Abs[auto["BallDimension"] - 1] < 0.06,
            Abs[auto["BallDimension"] - 1] < Abs[full["BallDimension"] - 1],
            Abs[auto["BallScalarCurvature"]] < Abs[full["BallScalarCurvature"]]
        }
    ],
    {True, True, True},
    TestID -> "VolumeGrowthObservables-C40-automatic-beats-full"
]

(* flat 20x20 square torus: d ~ 2, R ~ 0; Automatic cuts the wrap-around tail.  The ball
   probe overshoots to ~2.3 on a coarse lattice (lower-order lattice terms bias Gray's
   intercept; the sphere probe is the exact one here) -- the bound reflects that honest bias *)
VerificationTest[
    With[{g = TessellationGraph[{4, 4}, {20, 20}]},
        {v = First @ VertexList @ g},
        {auto = VolumeGrowthObservables[g, v], full = VolumeGrowthObservables[g, v, All]},
        {
            Abs[auto["BallDimension"] - 2] < 0.35,
            Abs[auto["BallDimension"] - 2] < Abs[full["BallDimension"] - 2],
            Abs[auto["BallScalarCurvature"]] < Abs[full["BallScalarCurvature"]]
        }
    ],
    {True, True, True},
    TestID -> "VolumeGrowthObservables-torus-automatic-cuts-wrap-tail"
]

(* the Automatic ball fit is reproducible from its own reported window *)
VerificationTest[
    With[{auto = VolumeGrowthObservables[CycleGraph[40], 1]},
        KeyTake[auto, {"BallDimension", "BallScalarCurvature", "BallWindow"}] ===
            KeyTake[VolumeGrowthObservables[CycleGraph[40], 1, auto["BallWindow"]], {"BallDimension", "BallScalarCurvature", "BallWindow"}]
    ],
    True,
    TestID -> "VolumeGrowthObservables-automatic-window-consistency"
]

(* the per-radius ball curvature profile has one entry per radius r = 1..ecc(v) *)
VerificationTest[
    Length[VolumeGrowthObservables[HypercubeGraph[8], 1]["BallCurvatureByRadius"]] === Length[BallVolumes[HypercubeGraph[8], 1]] - 1,
    True,
    TestID -> "VolumeGrowthObservables-ball-CurvatureByRadius-length"
]

(* positively curved Hamming cube: ball R > 0 on the detected core *)
VerificationTest[
    VolumeGrowthObservables[HypercubeGraph[8], 1]["BallScalarCurvature"] > 0,
    True,
    TestID -> "VolumeGrowthObservables-Q8-ball-positive-curvature"
]

(* pinned dimension fits only the slope, on the same detected window *)
VerificationTest[
    With[{g = TessellationGraph[{4, 4}, {20, 20}]},
        {v = First @ VertexList @ g},
        {pinned = VolumeGrowthObservables[g, v, "Dimension" -> 2], auto = VolumeGrowthObservables[g, v]},
        {pinned["BallDimension"], pinned["BallWindow"] === auto["BallWindow"]}
    ],
    {2., True},
    TestID -> "VolumeGrowthObservables-pinned-dimension-window"
]

(* flat square grid: shell area A(r) = 4 r exactly on the rising part, so q == 1, the
   sphere intercept is n - 1 = 1, the reported manifold dimension is n = 2 and S = 0 *)
VerificationTest[
    With[{g = GridGraph[{15, 15}]},
        {r = VolumeGrowthObservables[g, First @ GraphCenter @ g]},
        {Abs[r["SphereDimension"] - 2] < 1.*^-6, Abs @ r["SphereScalarCurvature"] < 1.*^-6}
    ],
    {True, True},
    TestID -> "VolumeGrowthObservables-grid2D-sphere-flat"
]

(* flat cubic grid: sphere manifold dimension recovered as ~ 3 *)
VerificationTest[
    With[{g = GridGraph[{9, 9, 9}]},
        Abs[VolumeGrowthObservables[g, First @ GraphCenter @ g]["SphereDimension"] - 3] < 0.2
    ],
    True,
    TestID -> "VolumeGrowthObservables-grid3D-sphere-dimension"
]

(* dual probe: ball and sphere agree on the manifold dimension of a flat lattice *)
VerificationTest[
    With[{g = GridGraph[{15, 15}]},
        {p = VolumeGrowthObservables[g, First @ GraphCenter @ g]},
        Abs[p["BallDimension"] - p["SphereDimension"]] < 0.4
    ],
    True,
    TestID -> "VolumeGrowthObservables-ball-sphere-dimension-agreement-flat"
]

(* flat torus through the sphere probe: d = 2, S = 0 *)
VerificationTest[
    With[{g = TessellationGraph[{4, 4}, {20, 20}]},
        {r = VolumeGrowthObservables[g, First @ VertexList @ g]},
        {Abs[r["SphereDimension"] - 2] < 0.05, Abs @ r["SphereScalarCurvature"] < 1.*^-6}
    ],
    {True, True},
    TestID -> "VolumeGrowthObservables-torus-sphere-flat"
]

(* positive curvature: ball and sphere scalar-curvature estimates share sign (both > 0)
   on the positively curved Hamming cube -- the dual-probe consistency check *)
VerificationTest[
    With[{p = VolumeGrowthObservables[HypercubeGraph[8], 1]},
        Sign[p["BallScalarCurvature"]] === Sign[p["SphereScalarCurvature"]] === 1
    ],
    True,
    TestID -> "VolumeGrowthObservables-positive-curvature-sign-agreement"
]

(* the sphere fit exposes the per-radius area-curvature and mean-curvature profiles *)
VerificationTest[
    With[{r = VolumeGrowthObservables[HypercubeGraph[8], 1]},
        {Length[r["SphereCurvatureByRadius"]] === Length[ShellAreas[HypercubeGraph[8], 1]] - 1,
         Length[r["SphereMeanCurvatureByRadius"]] === Length[ShellAreas[HypercubeGraph[8], 1]] - 1}
    ],
    {True, True},
    TestID -> "VolumeGrowthObservables-sphere-profiles-length"
]

(* the bundle is self-consistent: the returned "BallVolumes" is the Hausdorff measure that
   the fit actually consumes (default), and "...LogDifferenceQuotients" is the radius-correct
   log-log slope of exactly those returned profiles *)
VerificationTest[
    With[{g = GridGraph[{9, 9}]},
        {v = First @ GraphCenter @ g},
        {r = VolumeGrowthObservables[g, v],
         rq = (f |-> Table[(Log[N @ f[[k + 2]]] - Log[N @ f[[k + 1]]]) / (Log[k + 1.] - Log[k]), {k, 1, Length[f] - 2}])},
        {
            r["ShellAreas"] === ShellAreas[g, v],
            r["BallVolumes"] === BallVolumes[g, v, All, "Measure" -> "Hausdorff"],
            Max @ Abs[r["BallLogDifferenceQuotients"] - rq[r["BallVolumes"]]] < 10.^-10,
            Max @ Abs[r["SphereLogDifferenceQuotients"] - rq[r["ShellAreas"]]] < 10.^-10
        }
    ],
    {True, True, True, True},
    TestID -> "VolumeGrowthObservables-raw-profiles-match-primitives"
]


(* ===== DimensionCurvatureFit: Bishop-Gromov regression of log-differences ===== *)

(* an exactly linear quotient sequence q(r) = d - R/(3(d+2)) r(r+1) is recovered exactly:
   here d = 2, slope = -0.1 on x = r(r+1), so R = -3 (2 + 2) (-0.1) = 1.2 *)
VerificationTest[
    With[{pairs = Table[{r, 2 - 0.1 r (r + 1)}, {r, 1, 10}]},
        {fit = DimensionCurvatureFit[pairs]},
        {Chop[fit["Dimension"] - 2], Chop[fit["ScalarCurvature"] - 1.2]}
    ],
    {0, 0},
    TestID -> "DimensionCurvatureFit-recovers-exact-line"
]

(* a bare quotient list defaults to radii 0, 1, 2, ... -- same as the explicit pairs form *)
VerificationTest[
    With[{q = {1.0, 0.92, 0.81, 0.63, 0.4}},
        DimensionCurvatureFit[q] === DimensionCurvatureFit[Transpose[{Range[0, Length[q] - 1], q}]]
    ],
    True,
    TestID -> "DimensionCurvatureFit-bare-list-radii-default"
]

(* the headline composition: the index-based LogDifferenceQuotients of the Counting ball volume,
   sliced to an inner window and regressed, reads the lattice dimension d = 2 (the off-by-one of
   the index quotient on Counting is the Hausdorff boundary shift) *)
VerificationTest[
    With[{g = GridGraph[{21, 21}]},
        {v = First @ GraphCenter @ g},
        {q = LogDifferenceQuotients[BallVolumes[g, v, All, "Measure" -> "Counting"]]},
        {pairs = Select[Transpose[{Range[0, Length[q] - 1], q}], 1 <= #[[1]] <= 7 &]},
        Abs[DimensionCurvatureFit[pairs]["Dimension"] - 2] < 0.4
    ],
    True,
    TestID -> "DimensionCurvatureFit-counting-index-reads-lattice-dimension"
]

(* Around-valued quotients (the across-vertex spread of the averaged profile) carry through the
   closed-form fit to Around dimension and curvature *)
VerificationTest[
    With[{g = GridGraph[{15, 15}]},
        {rq = (f |-> Table[(Log[f[[k + 2]]] - Log[f[[k + 1]]]) / (Log[k + 1.] - Log[k]), {k, 1, Length[f] - 2}])},
        {avg = Exp /@ (MeanAround /@ Transpose[Log[N[BallVolumes[g, All, {0, 8}, "Measure" -> "Hausdorff"]]]])},
        {fit = DimensionCurvatureFit[rq[avg]]},
        {Head[fit["Dimension"]], Head[fit["ScalarCurvature"]]}
    ],
    {Around, Around},
    TestID -> "DimensionCurvatureFit-Around-propagates"
]

(* pinning the dimension fixes the intercept and fits the slope only *)
VerificationTest[
    With[{pairs = Table[{r, 2.3 - 0.05 r (r + 1)}, {r, 1, 8}]},
        DimensionCurvatureFit[pairs, "Dimension" -> 2]["Dimension"] == 2
    ],
    True,
    TestID -> "DimensionCurvatureFit-pinned-dimension"
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
    With[{e = UniformLengthEmbedding[CycleGraph[6]]}, MatrixQ[e] && Dimensions[e] === {6, 3}],
    True,
    TestID -> "UniformLengthEmbedding-matrix-shape"
]

VerificationTest[
    With[{g = CycleGraph[6], e = UniformLengthEmbedding[CycleGraph[6]]},
        Max @ Abs[(EuclideanDistance[e[[#[[1]]]], e[[#[[2]]]]] & /@ (List @@@ EdgeList[g])) - 1] < 0.01
    ],
    True,
    TestID -> "UniformLengthEmbedding-unit-edges"
]

VerificationTest[
    Dimensions @ UniformLengthEmbedding[GridGraph[{3, 3}], "Dimension" -> 2],
    {9, 2},
    TestID -> "UniformLengthEmbedding-dimension-option"
]

VerificationTest[
    With[{g = UniformLengthGraph[Sphere[], 40]},
        GraphQ[g] && VertexCount[g] == 40 && Mean[N @ VertexDegree[g]] >= 4
    ],
    True,
    TestID -> "UniformLengthGraph-sphere-jammed"
]

VerificationTest[
    With[{lens = ulEdgeLengths @ UniformLengthGraph[Sphere[], 40]}, Max[lens] / Min[lens] < 1.8],
    True,
    TestID -> "UniformLengthGraph-sphere-edges-near-uniform"
]

VerificationTest[
    Block[{}, SeedRandom[1];
        With[{g = UniformLengthGraph[Circle[{0, 0}, 1], 20]},
            GraphQ[g] && VertexCount[g] == 20 && Mean[N @ VertexDegree[g]] > 1.8
        ]
    ],
    True,
    TestID -> "UniformLengthGraph-circle-cycle"
]

VerificationTest[
    Block[{}, SeedRandom[3];
        With[{lens = ulEdgeLengths @ Quiet @ UniformLengthGraph[Circle[{0, 0}, 1], 12, Method -> "ConstrainedPacking"]},
            Max[lens] - Min[lens] < 0.05
        ]
    ],
    True,
    TestID -> "UniformLengthGraph-constrained-uniform"
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

(* Subgraph form agrees with the vertex-list form when the subgraph is induced *)
VerificationTest[
    With[{g = GridGraph[{4, 4}], s = {1, 2, 3, 6, 7, 11}},
        GraphBoundary[g, Subgraph[g, s]] === GraphBoundary[g, s] &&
        GraphInterior[g, Subgraph[g, s]] === GraphInterior[g, s]
    ],
    True,
    TestID -> "GraphBoundary-subgraph-form"
]

(* A NON-induced subgraph (a Hamiltonian path/curve through every vertex) is
   edge-aware: as a curve it is all boundary but for two pass-through corners,
   whereas the same vertices as a list (full set) are entirely interior *)
VerificationTest[
    With[{g = GridGraph[{3, 3}],
          curve = Graph[Range[9], UndirectedEdge @@@ Partition[{1, 2, 3, 6, 5, 4, 7, 8, 9}, 2, 1]]},
        {Length @ GraphInterior[g, curve], Length @ GraphInterior[g, VertexList[g]]}
    ],
    {2, 9},
    TestID -> "GraphInterior-noninduced-curve-vs-list"
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

(* Method -> "Greedy" always returns a valid cover ... *)
VerificationTest[
    With[{g = GraphData["CuboctahedralGraph"]},
        BallCoverQ[g, 1, FindBallCover[g, 1, All, 1, Method -> "Greedy"]]
    ],
    True,
    TestID -> "FindBallCover-greedy-covers"
]

(* ... but is not minimum in general: the cuboctahedron is vertex-transitive with gamma = 3,
   yet every greedy run returns 4 *)
VerificationTest[
    With[{g = GraphData["CuboctahedralGraph"]},
        {Length @ FindBallCover[g, 1], Length @ FindBallCover[g, 1, All, 1, Method -> "Greedy"]}
    ],
    {3, 4},
    TestID -> "FindBallCover-greedy-suboptimal-cuboctahedron"
]

(* Method -> "Symmetric" recovers the orbit-shaped minimum cover greedy misses: the
   cuboctahedron's gamma = 3 is a single Aut-orbit, so the symmetric search finds 3, not 4 *)
VerificationTest[
    With[{g = GraphData["CuboctahedralGraph"]}, {s = FindBallCover[g, 1, All, 1, Method -> "Symmetric"]},
        {Length[s], BallCoverQ[g, 1, s]}
    ],
    {3, True},
    TestID -> "FindBallCover-symmetric-cuboctahedron"
]

(* nerve of chosen graph-metric centres: B(2),B(4),B(6) at r=1 on the path meet
   consecutively (shared vertices 3, 5) but the ends miss, giving the path complex *)
VerificationTest[
    Sort @ BallIntersectionComplex[{2, 4, 6}, 1, Infinity, "Metric" -> PathGraph[Range[7]]],
    {{1}, {2}, {3}, {1, 2}, {2, 3}},
    TestID -> "BallIntersectionComplex-graph-chosen-centres"
]


(* ===================== Tessellations, regular & uniform maps ===================== *)

(* TessellationGraph is the single public generator: a {p, q} Schlafli symbol gives a
   regular map, a longer vertex configuration a uniform / Archimedean map; the second
   argument sizes it (n / {m, n} torus) or supplies the carrying group. PunchHole is the
   separate surgery utility. *)

(* --- Flat-torus regular tessellations + hole punching --- *)

VerificationTest[
  { Union @ VertexDegree @ TessellationGraph[ { 3, 6 }, { 5, 5 } ],
    Union @ VertexDegree @ TessellationGraph[ { 4, 4 }, { 5, 5 } ],
    Union @ VertexDegree @ TessellationGraph[ { 6, 3 }, { 5, 5 } ] },
  { { 6 }, { 4 }, { 3 } },
  TestID -> "Torus-degrees-by-shape"
]

VerificationTest[
  VertexTransitiveGraphQ @ TessellationGraph[ { 3, 6 }, { 5, 5 } ],
  True,
  TestID -> "Torus-triangular-vertex-transitive"
]

VerificationTest[
  With[ { g = GridGraph[ { 5, 5 } ] }, VertexCount @ PunchHole[ g, 1 -> 1 ] < VertexCount @ g ],
  True,
  TestID -> "PunchHole-removes-a-ball"
]

(* --- Regular maps: spherical Platonic graphs --- *)

VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 3, 3 } ], GraphData[ "TetrahedralGraph" ] ], True, TestID -> "Schlafli-33-is-tetrahedron" ]
VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 4, 3 } ], GraphData[ "CubicalGraph" ] ], True, TestID -> "Schlafli-43-is-cube" ]
VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 3, 4 } ], GraphData[ "OctahedralGraph" ] ], True, TestID -> "Schlafli-34-is-octahedron" ]
VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 5, 3 } ], GraphData[ "DodecahedralGraph" ] ], True, TestID -> "Schlafli-53-is-dodecahedron" ]
VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 3, 5 } ], GraphData[ "IcosahedralGraph" ] ], True, TestID -> "Schlafli-35-is-icosahedron" ]

VerificationTest[ Union @ VertexDegree @ TessellationGraph[ { 3, 5 } ], { 5 }, TestID -> "Schlafli-is-q-regular" ]

VerificationTest[
  { Length @ FindCycle[ TessellationGraph[ { 4, 3 } ], { 4 }, All ], Length @ FindCycle[ TessellationGraph[ { 5, 3 } ], { 5 }, All ] },
  { 6, 12 },
  TestID -> "Schlafli-p-cycle-count-equals-faces"
]

VerificationTest[
  With[ { g = TessellationGraph[ { 3, 5 } ] },
    Length @ DeleteDuplicates[ CanonicalGraph[ NeighborhoodGraph[ g, #, 1 ] ] & /@ VertexList[ g ], IsomorphicGraphQ ] ],
  1,
  TestID -> "Schlafli-locally-isomorphic"
]

VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 4, 4 }, 5 ], TessellationGraph[ { 4, 4 }, { 5, 5 } ] ], True, TestID -> "Schlafli-44-is-torus" ]
VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 6, 3 }, 4 ], TessellationGraph[ { 6, 3 }, { 4, 4 } ] ], True, TestID -> "Schlafli-63-is-hexagonal-torus" ]

VerificationTest[
  With[ { g = TessellationGraph[ { 3, 7 } ] }, { VertexCount @ g, EdgeCount @ g, Union @ VertexDegree @ g, VertexTransitiveGraphQ @ g } ],
  { 24, 84, { 7 }, True },
  TestID -> "Schlafli-37-Klein-quartic-skeleton"
]

(* genus via Euler on the embedded map: Klein quartic {3,7} has genus 3 *)
VerificationTest[
  With[ { g = TessellationGraph[ { 3, 7 } ] }, With[ { v = VertexCount @ g, e = EdgeCount @ g, f = Length @ FindCycle[ g, { 3 }, All ] }, 1 - ( v - e + f )/2 ] ],
  3,
  TestID -> "Schlafli-37-genus-3"
]

VerificationTest[ TessellationGraph[ { 3, 7 }, 99 ], $Failed, TestID -> "Schlafli-hyperbolic-unreachable-is-Failed" ]
VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 4, 3 }, SymmetricGroup[ 4 ] ], GraphData[ "CubicalGraph" ] ], True, TestID -> "RegularMap-explicit-group-cube" ]

(* --- General coset enumeration (Todd-Coxeter / low-index) via Method --- *)

(* Todd-Coxeter index: |D(4,3,2)| = 24, and V = [D:<y>] = 8, E = [D:<xy>] = 12, F = [D:<x>] = 6 (cube) *)
VerificationTest[
  { CosetEnumeration[ 4, 3, { }, 400 ], CosetEnumeration[ 4, 3, { { 3 } }, 400 ], CosetEnumeration[ 4, 3, { { 1, 3 } }, 400 ], CosetEnumeration[ 4, 3, { { 1 } }, 400 ] },
  { 24, 8, 12, 6 },
  TestID -> "CosetEnumeration-cube-VEF"
]

(* trivial subgroup of an infinite (hyperbolic) von Dyck group has infinite index *)
VerificationTest[ CosetEnumeration[ 3, 7, { }, 60 ], $Failed, TestID -> "CosetEnumeration-hyperbolic-infinite" ]

(* the general low-index method recovers the Platonic solids (Method -> "CosetEnumeration") *)
VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 3, 3 }, Method -> "CosetEnumeration" ], GraphData[ "TetrahedralGraph" ] ], True, TestID -> "CosetEnumeration-recovers-tetrahedron" ]
VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 3, 5 }, Method -> "CosetEnumeration" ], GraphData[ "IcosahedralGraph" ] ], True, TestID -> "CosetEnumeration-recovers-icosahedron" ]

(* the explicit realiser Methods agree with the Automatic default *)
VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 4, 3 }, Method -> "Platonic" ], TessellationGraph[ { 4, 3 } ] ], True, TestID -> "Method-Platonic-matches-default" ]
VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 3, 7 }, Method -> "PSL2" ], TessellationGraph[ { 3, 7 } ] ], True, TestID -> "Method-PSL2-matches-default" ]

(* low-index enumeration: the only genuine {3,3} map up to index 12 is the tetrahedron, regular, genus 0 *)
VerificationTest[
  { Length @ #, #[[ 1, "Index" ]], #[[ 1, "Regular" ]], #[[ 1, "Genus" ]] } &@ LowIndexMaps[ 3, 3, 12 ],
  { 1, 12, True, 0 },
  TestID -> "LowIndexMaps-33-tetrahedron"
]

VerificationTest[ Head @ Quiet @ TessellationGraph[ { 3, 3 }, Method -> "Nonsense" ], Symbol, TestID -> "Method-unknown-is-Failed" ]

(* --- Uniform / Archimedean maps --- *)

VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 3, 4, 3, 4 } ], PolyhedronData[ "Cuboctahedron", "SkeletonGraph" ] ], True, TestID -> "Archimedean-3434-is-cuboctahedron" ]
VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 4, 6, 8 } ], PolyhedronData[ "GreatRhombicuboctahedron", "SkeletonGraph" ] ], True, TestID -> "Archimedean-468-is-great-rhombicuboctahedron" ]
VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 3, 3, 3, 3, 4 } ], PolyhedronData[ "SnubCube", "SkeletonGraph" ] ], True, TestID -> "Archimedean-snub-cube" ]

VerificationTest[
  { VertexCount @ #, Union @ VertexDegree @ #, VertexTransitiveGraphQ @ # } &@ TessellationGraph[ { 4, 4, 5 } ],
  { 10, { 3 }, True },
  TestID -> "Archimedean-pentagonal-prism"
]

VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 3, 3, 3, 5 } ], PolyhedronData[ { "Antiprism", 5 }, "SkeletonGraph" ] ], True, TestID -> "Archimedean-pentagonal-antiprism" ]
VerificationTest[ IsomorphicGraphQ[ TessellationGraph[ { 4, 4, 4 } ], GraphData[ "CubicalGraph" ] ], True, TestID -> "Archimedean-444-forwards-to-cube" ]

VerificationTest[
  With[ { g = TessellationGraph[ { 3, 6, 3, 6 }, 4 ] }, { Union @ VertexDegree @ g, VertexTransitiveGraphQ @ g } ],
  { { 4 }, True },
  TestID -> "Archimedean-trihexagonal-torus"
]

VerificationTest[
  With[ { g = TessellationGraph[ { 4, 8, 8 }, 4 ] }, { Union @ VertexDegree @ g, VertexTransitiveGraphQ @ g } ],
  { { 3 }, True },
  TestID -> "Archimedean-truncated-square-torus"
]

VerificationTest[
  With[ { g = TessellationGraph[ { 3, 12, 12 }, 4 ] }, { Union @ VertexDegree @ g, VertexTransitiveGraphQ @ g } ],
  { { 3 }, True },
  TestID -> "Archimedean-truncated-hexagonal-torus"
]

VerificationTest[
  With[ { g = TessellationGraph[ { 3, 4, 6, 4 }, 4 ] }, { Union @ VertexDegree @ g, VertexTransitiveGraphQ @ g } ],
  { { 4 }, True },
  TestID -> "Archimedean-rhombitrihexagonal-torus"
]

VerificationTest[
  With[ { g = TessellationGraph[ { 4, 6, 12 }, 4 ] }, { Union @ VertexDegree @ g, VertexTransitiveGraphQ @ g } ],
  { { 3 }, True },
  TestID -> "Archimedean-truncated-trihexagonal-torus"
]

(* genus via Euler with the mixed face vector: a Euclidean uniform tiling has genus 1 *)
VerificationTest[
  With[ { g = TessellationGraph[ { 3, 6, 3, 6 }, 5 ], cfg = { 3, 6, 3, 6 } },
    With[ { v = VertexCount @ g }, 1 - v ( 1 - Length[ cfg ]/2 + Total[ 1/cfg ] )/2 ] ],
  1,
  TestID -> "Archimedean-torus-genus-1"
]

VerificationTest[ TessellationGraph[ { 3, 3, 3, 3, 6 }, 4 ], $Failed, { TessellationGraph::deferred }, TestID -> "Archimedean-euclidean-snub-deferred" ]


(* ===================== Map invariants: curvature, Euler characteristic, genus ===================== *)

(* combinatorial Gaussian curvature classifies the geometry by sign: flat / hyperbolic / spherical *)
VerificationTest[
  Sign @ { TessellationCurvature[ { 3, 6 } ], TessellationCurvature[ { 3, 7 } ], TessellationCurvature[ { 3, 5 } ] },
  { 0, -1, 1 },
  TestID -> "TessellationCurvature-flat-hyperbolic-spherical"
]

(* every Euclidean uniform (Archimedean) tiling is flat *)
VerificationTest[
  { TessellationCurvature[ { 4, 8, 8 } ], TessellationCurvature[ { 3, 4, 6, 4 } ], TessellationCurvature[ { 3, 6, 3, 6 } ] },
  { 0, 0, 0 },
  TestID -> "TessellationCurvature-archimedean-flat"
]

(* icosahedron {3,5} is a sphere: chi = 2, genus 0 *)
VerificationTest[
  With[ { g = TessellationGraph[ { 3, 5 } ] }, { TessellationEulerCharacteristic[ g, { 3, 5 } ], TessellationGenus[ g, { 3, 5 } ] } ],
  { 2, 0 },
  TestID -> "TessellationGenus-icosahedron-sphere"
]

(* Euclidean tilings (regular and Archimedean) on the torus have genus 1 *)
VerificationTest[
  { TessellationGenus[ TessellationGraph[ { 4, 4 }, { 12, 12 } ], { 4, 4 } ],
    TessellationGenus[ TessellationGraph[ { 4, 8, 8 }, 5 ], { 4, 8, 8 } ] },
  { 1, 1 },
  TestID -> "TessellationGenus-torus-genus-1"
]

(* hyperbolic quotient: chi agrees with the discrete Gauss-Bonnet sum V*kappa, genus > 1 *)
VerificationTest[
  With[ { g = TessellationGraph[ { 3, 7 }, 2 ] },
    TessellationEulerCharacteristic[ g, { 3, 7 } ] == VertexCount[ g ] TessellationCurvature[ { 3, 7 } ] && TessellationGenus[ g, { 3, 7 } ] > 1 ],
  True,
  TestID -> "TessellationGenus-hyperbolic-gauss-bonnet"
]

(* spec-free forms detect a regular configuration (uniform degree, girth face) from the graph *)
VerificationTest[
  { TessellationGenus[ TessellationGraph[ { 3, 5 } ] ],
    TessellationGenus[ TessellationGraph[ { 4, 4 }, { 12, 12 } ] ],
    TessellationGenus[ TessellationGraph[ { 3, 7 }, 2 ] ] },
  { 0, 1, 14 },
  TestID -> "TessellationGenus-spec-free-regular-detection"
]


(* ===================== Example graphs: Sierpinski & Bethe ===================== *)

(* trivalent Sierpinski graph: 3-simplex K_4 truncated n-1 times; 4*3^(n-1) vertices,
   3-regular at every generation (n=1 is K_4, n=2 the truncated tetrahedron) *)
VerificationTest[
  Table[ { VertexCount @ SierpinskiGraph[ n ], EdgeCount @ SierpinskiGraph[ n ], Union @ VertexDegree @ SierpinskiGraph[ n ] }, { n, 1, 4 } ],
  Table[ { 4 * 3^( n - 1 ), 6 * 3^( n - 1 ), { 3 } }, { n, 1, 4 } ],
  TestID -> "Sierpinski-trivalent-truncation"
]

VerificationTest[ IsomorphicGraphQ[ SierpinskiGraph[ 1 ], CompleteGraph[ 4 ] ], True, TestID -> "Sierpinski-seed-is-3-simplex" ]
VerificationTest[ IsomorphicGraphQ[ SierpinskiGraph[ 2 ], PolyhedronData[ "TruncatedTetrahedron", "SkeletonGraph" ] ], True, TestID -> "Sierpinski-level-2-is-truncated-tetrahedron" ]
VerificationTest[ { ConnectedGraphQ @ SierpinskiGraph[ 4 ], PlanarGraphQ @ SierpinskiGraph[ 4 ] }, { True, True }, TestID -> "Sierpinski-connected-planar" ]

(* BetheGraph[n, z]: n shells, coordination z; all interior vertices z-valent, only the
   depth-n boundary 1-valent; vertex count 1 + z((z-1)^n - 1)/(z-2) *)
VerificationTest[
  { KeySort @ Counts @ VertexDegree @ BetheGraph[ 3, 3 ], Max @ VertexDegree @ BetheGraph[ 4, 3 ] },
  { <| 1 -> 12, 3 -> 10 |>, 3 },
  TestID -> "Bethe-coordination-regular"
]

VerificationTest[ VertexCount @ BetheGraph[ 4, 3 ], 1 + 3 ( ( 3 - 1 )^4 - 1 )/( 3 - 2 ), TestID -> "Bethe-vertex-count-formula" ]
VerificationTest[ TreeGraphQ @ BetheGraph[ 3, 4 ], True, TestID -> "Bethe-is-tree" ]

(* BranchingSequenceTree[b]: spherically symmetric tree, b[[l+1]] children at depth l;
   shell sizes are FoldList[Times, 1, b], constant b is CompleteKaryTree *)
VerificationTest[
  Values @ KeySort @ Counts[ First /@ VertexList @ BranchingSequenceTree[ { 2, 3, 2 } ] ],
  FoldList[ Times, 1, { 2, 3, 2 } ],
  TestID -> "BranchingSequenceTree-shell-sizes"
]

VerificationTest[ IsomorphicGraphQ[ BranchingSequenceTree[ { 2, 2, 2 } ], CompleteKaryTree[ 4, 2 ] ], True, TestID -> "BranchingSequenceTree-constant-is-CompleteKaryTree" ]

(* radial symmetry: every vertex at a given depth has the same degree *)
VerificationTest[
  With[ { g = BranchingSequenceTree[ { 3, 2, 2 } ] },
    AllTrue[ GroupBy[ VertexList @ g, First, Union[ VertexDegree[ g, # ] & /@ # ] & ], Length @ # == 1 & ] ],
  True,
  TestID -> "BranchingSequenceTree-radial"
]


(* ===== TessellationNeighborhoodGraph: unwrapped {p,q} patches ===== *)

(* Euclidean patches: interior degree q, exact ball counts, p-gon faces *)
VerificationTest[
  Max @ VertexDegree @ TessellationNeighborhoodGraph[ { 4, 4 }, 3 ],
  4,
  TestID -> "TessellatedDisk-44-interior-degree-4"
]

VerificationTest[
  Max @ VertexDegree @ TessellationNeighborhoodGraph[ { 3, 6 }, 3 ],
  6,
  TestID -> "TessellatedDisk-36-interior-degree-6"
]

VerificationTest[
  Max @ VertexDegree @ TessellationNeighborhoodGraph[ { 6, 3 }, 3 ],
  3,
  TestID -> "TessellatedDisk-63-interior-degree-3"
]

(* triangular B_r ball: 1 + 3 r (r + 1) vertices *)
VerificationTest[
  VertexCount /@ ( TessellationNeighborhoodGraph[ { 3, 6 }, # ] & /@ { 2, 3, 4 } ),
  { 19, 37, 61 },
  TestID -> "TessellatedDisk-36-ball-counts"
]

(* the patch has a boundary -- unlike the torus, it is not vertex-transitive *)
VerificationTest[
  VertexTransitiveGraphQ @ TessellationNeighborhoodGraph[ { 3, 6 }, 3 ],
  False,
  TestID -> "TessellatedDisk-36-not-vertex-transitive"
]

VerificationTest[
  FindCycle[ TessellationNeighborhoodGraph[ { 4, 4 }, 3 ], { 4 }, 1 ] =!= {} &&
   FindCycle[ TessellationNeighborhoodGraph[ { 6, 3 }, 3 ], { 6 }, 1 ] =!= {},
  True,
  TestID -> "TessellatedDisk-euclidean-faces"
]

(* Hyperbolic patches: connected, planar, max degree == q (dedup regression guard) *)
VerificationTest[
  With[ { g = TessellationNeighborhoodGraph[ { 7, 3 }, 4 ] },
    { ConnectedGraphQ @ g, PlanarGraphQ @ g, Max @ VertexDegree @ g } ],
  { True, True, 3 },
  TestID -> "TessellatedDisk-73-connected-planar-degree3"
]

VerificationTest[
  With[ { g = TessellationNeighborhoodGraph[ { 3, 7 }, 3 ] },
    { ConnectedGraphQ @ g, PlanarGraphQ @ g, Max @ VertexDegree @ g } ],
  { True, True, 7 },
  TestID -> "TessellatedDisk-37-connected-planar-degree7"
]

VerificationTest[
  FindCycle[ TessellationNeighborhoodGraph[ { 7, 3 }, 4 ], { 7 }, 1 ] =!= {},
  True,
  TestID -> "TessellatedDisk-73-heptagon-faces"
]

(* vertex count strictly increases with r *)
VerificationTest[
  With[ { c = VertexCount /@ ( TessellationNeighborhoodGraph[ { 7, 3 }, # ] & /@ { 2, 3, 4 } ) },
    OrderedQ @ c && DuplicateFreeQ @ c ],
  True,
  TestID -> "TessellatedDisk-73-monotone"
]

(* Spherical patches: the combinatorics closes the tiling up into the finite Platonic graph *)
VerificationTest[
  VertexCount /@ ( TessellationNeighborhoodGraph[ #, 9 ] & /@ { { 3, 3 }, { 4, 3 }, { 3, 4 }, { 5, 3 }, { 3, 5 } } ),
  { 4, 8, 6, 20, 12 },
  TestID -> "TessellatedDisk-spherical-closure-counts"
]

VerificationTest[
  Max @ VertexDegree @ TessellationNeighborhoodGraph[ { 3, 5 }, 9 ],
  5,
  TestID -> "TessellatedDisk-icosahedron-degree-5"
]

VerificationTest[
  With[ { cap = TessellationNeighborhoodGraph[ { 3, 5 }, 1 ] },
    ConnectedGraphQ @ cap && VertexCount @ cap < 12 ],
  True,
  TestID -> "TessellatedDisk-icosahedron-cap-open"
]


(* ===== TessellationNeighborhoodGraph: unwrapped uniform / Archimedean patches ===== *)

(* the unwrapped ball of a uniform tiling equals the same-radius ball cut from its compact
   torus quotient -- the construction-independent characterisation of B_r *)
VerificationTest[
  IsomorphicGraphQ[
    TessellationNeighborhoodGraph[ { 4, 8, 8 }, 2 ],
    With[ { g = TessellationGraph[ { 4, 8, 8 }, 5 ] }, NeighborhoodGraph[ g, First @ GraphCenter @ g, 2 ] ] ],
  True,
  TestID -> "TessellatedDisk-488-matches-torus-ball"
]

(* interior valence is the configuration length: degree 3 for the 3-valent uniform tilings *)
VerificationTest[
  Max @ VertexDegree @ TessellationNeighborhoodGraph[ { 4, 8, 8 }, 4 ],
  3,
  TestID -> "TessellatedDisk-488-degree-3"
]

(* and degree 4 for the 4-valent ones *)
VerificationTest[
  Max @ VertexDegree @ TessellationNeighborhoodGraph[ { 3, 6, 3, 6 }, 4 ],
  4,
  TestID -> "TessellatedDisk-3636-degree-4"
]

(* every face size in the configuration occurs as a girth-class cycle of the patch *)
VerificationTest[
  AllTrue[ { 3, 4, 6 }, FindCycle[ TessellationNeighborhoodGraph[ { 3, 4, 6, 4 }, 4 ], { # }, 1 ] =!= {} & ],
  True,
  TestID -> "TessellatedDisk-3464-face-types"
]

(* an all-equal configuration is a regular {p, q} symbol and forwards to the {p, q} engine *)
VerificationTest[
  IsomorphicGraphQ[
    TessellationNeighborhoodGraph[ { 6, 6, 6 }, 3 ],
    TessellationNeighborhoodGraph[ { 6, 3 }, 3 ] ],
  True,
  TestID -> "TessellatedDisk-regular-config-forwards"
]

(* a spherical configuration (defect > 0) is the finite Archimedean solid: cuboctahedron *)
VerificationTest[
  With[ { g = TessellationNeighborhoodGraph[ { 3, 4, 3, 4 }, 9 ] },
    VertexCount @ g == 12 && Max @ VertexDegree @ g == 4 ],
  True,
  TestID -> "TessellatedDisk-cuboctahedron"
]

(* the Euclidean snub / elongated families are chiral and deferred *)
VerificationTest[
  TessellationNeighborhoodGraph[ { 3, 3, 3, 3, 6 }, 2 ],
  $Failed,
  { TessellationNeighborhoodGraph::deferred },
  TestID -> "TessellatedDisk-snub-deferred"
]

(* a hyperbolic uniform tiling (defect < 0) grows in the Poincare disk: interior valence is the
   configuration length, both face sizes occur, and the patch is genuinely hyperbolic (K = -1) *)
VerificationTest[
  With[ { g = TessellationNeighborhoodGraph[ { 3, 7, 3, 7 }, 3 ] },
    Max @ VertexDegree @ g == 4 && FindCycle[ g, { 3 }, 1 ] =!= {} && FindCycle[ g, { 7 }, 1 ] =!= {} ],
  True,
  TestID -> "TessellatedDisk-hyperbolic-3737"
]

(* a hyperbolic uniform tiling with three distinct face sizes, including a large polygon *)
VerificationTest[
  Max @ VertexDegree @ TessellationNeighborhoodGraph[ { 4, 6, 14 }, 3 ],
  3,
  TestID -> "TessellatedDisk-hyperbolic-4-6-14"
]

(* the curvature parameter solved from the angle defect has the sign of the defect *)
VerificationTest[
  Sign /@ { TessellationCurvature[ { 4, 8, 8 } ], TessellationCurvature[ { 3, 7, 3, 7 } ], TessellationCurvature[ { 3, 5 } ] },
  { 0, -1, 1 },
  TestID -> "TessellatedDisk-defect-signs"
]


(* ===== GeodesicIntervalGraph / GeodesicOccupation ===== *)

(* the interval graph is a directed acyclic graph whose vertices are the metric interval (= the CylinderVolumes support) *)
VerificationTest[
    With[{g = GridGraph[{3, 3}], ig = GeodesicIntervalGraph[GridGraph[{3, 3}], 1, 9]},
        {AcyclicGraphQ[ig], DirectedGraphQ[ig],
         Sort[VertexList[ig]] === Sort @ Flatten @ Position[GraphDistance[g, 1] + GraphDistance[g, 9], GraphDistance[g, 1, 9]]}
    ],
    {True, True, True},
    TestID -> "GeodesicIntervalGraph-dag-interval"
]

(* per-vertex occupation equals brute-force enumeration of every geodesic *)
VerificationTest[
    With[{g = GridGraph[{3, 3}]},
        KeySort @ GeodesicOccupation[g, 1, 9] === KeySort @ Counts @ Catenate @ FindPath[g, 1, 9, {GraphDistance[g, 1, 9]}, All]
    ],
    True,
    TestID -> "GeodesicOccupation-matches-bruteforce"
]

(* family size = Max occupation = number of geodesics; the 3x3 grid center lies on 4 of the 6 *)
VerificationTest[
    With[{occ = GeodesicOccupation[GridGraph[{3, 3}], 1, 9]}, {Max[Values[occ]], occ[5]}],
    {6, 4},
    TestID -> "GeodesicOccupation-grid-counts"
]

(* the DAG-form accessor agrees with the (g, u, v) form *)
VerificationTest[
    With[{g = GridGraph[{4, 4}]},
        GeodesicOccupation[GeodesicIntervalGraph[g, 1, 16]] === GeodesicOccupation[g, 1, 16]
    ],
    True,
    TestID -> "GeodesicOccupation-dag-form-agrees"
]

(* path: unique geodesic; cycle: antipodal points have two geodesics *)
VerificationTest[
    {Max[Values @ GeodesicOccupation[PathGraph[Range[5]], 1, 5]], Max[Values @ GeodesicOccupation[CycleGraph[6], 1, 4]]},
    {1, 2},
    TestID -> "GeodesicOccupation-path-cycle"
]

(* edge occupation matches brute-force edge counts over all geodesics *)
VerificationTest[
    With[{g = GridGraph[{3, 3}]},
        KeySort @ KeyMap[Sort @* Apply[List], GeodesicEdgeOccupation[g, 1, 9]] ===
        KeySort @ KeyMap[Sort, Counts @ Catenate[Partition[#, 2, 1] & /@ FindPath[g, 1, 9, {GraphDistance[g, 1, 9]}, All]]]
    ],
    True,
    TestID -> "GeodesicEdgeOccupation-matches-bruteforce"
]

(* every edge of a unique geodesic carries occupation 1; total flow through a
   layer cut equals the family size *)
VerificationTest[
    {Values @ GeodesicEdgeOccupation[PathGraph[Range[5]], 1, 5],
     Total @ Values @ KeySelect[GeodesicEdgeOccupation[GridGraph[{3, 3}], 1, 9], First[#] === 1 &]},
    {{1, 1, 1, 1}, Max @ Values @ GeodesicOccupation[GridGraph[{3, 3}], 1, 9]},
    TestID -> "GeodesicEdgeOccupation-flow-conservation"
]

(* disconnected endpoints: the interval graph is empty *)
VerificationTest[
    VertexCount @ GeodesicIntervalGraph[Graph[{1, 2, 3}, {1 <-> 2}], 1, 3],
    0,
    TestID -> "GeodesicIntervalGraph-disconnected-empty"
]


(* ===== Displacements ===== *)

(* displacement fixtures: coordinate grid, cycle step *)
dispGrid = VertexReplace[GridGraph[{5, 5}], Catenate @ Table[5 j + i + 1 -> {i + 1, j + 1}, {j, 0, 4}, {i, 0, 4}]]
dispX = AssociationMap[{{Min[#[[1]] + 1, 5], #[[2]]}} &, VertexList @ dispGrid]
dispY = AssociationMap[{{#[[1]], Min[#[[2]] + 1, 5]}} &, VertexList @ dispGrid]
dispCycleStep = AssociationMap[{Mod[#, 10] + 1} &, Range @ 10]

(* the sum is exactly commutative: the bisector construction is symmetric *)
VerificationTest[
    DisplacementSum[dispGrid, dispX, dispY] === DisplacementSum[dispGrid, dispY, dispX],
    True,
    TestID -> "Displacement-sum-commutative"
]

(* coordinate steps have zero metric bracket on the interior *)
VerificationTest[
    DisplacementBracket[dispGrid, dispX, dispY, {2, 2}],
    {{2, 2}},
    TestID -> "Displacement-bracket-coordinate-steps"
]

(* negative is the straight reflection on the interior *)
VerificationTest[
    DisplacementNegative[dispGrid, dispX] @ {3, 3},
    {{2, 3}},
    TestID -> "Displacement-negative-reflection"
]

(* inverse relations record every preimage, including empty fibers *)
VerificationTest[
    Lookup[DisplacementInverse @ dispX, {{1, 3}, {5, 3}}],
    {{}, {{4, 3}, {5, 3}}},
    TestID -> "Displacement-inverse-relation"
]

(* a Killing displacement has a genuine two-sided inverse *)
VerificationTest[
    With[{inverse = DisplacementInverse @ dispCycleStep},
      {DisplacementCompose[dispCycleStep, inverse], DisplacementCompose[inverse, dispCycleStep]}],
    ConstantArray[AssociationMap[{#} &, Range @ 10], 2],
    TestID -> "Displacement-inverse-killing"
]

(* scaling round-trip on the cycle: (1/3)(3 D) = D *)
VerificationTest[
    DisplacementScale[CycleGraph[10], DisplacementScale[CycleGraph[10], dispCycleStep, 3], 1/3],
    dispCycleStep,
    TestID -> "Displacement-scale-roundtrip"
]

(* magnitude of a unit step field is 1 *)
VerificationTest[
    DisplacementMagnitude[dispGrid, dispX],
    1,
    TestID -> "Displacement-magnitude-unit"
]

(* reduce recovers a field from its neighbourhood blur *)
VerificationTest[
    DisplacementReduce[dispGrid, Map[Union @ Flatten[{#, AdjacencyList[dispGrid, First @ #]}, 1] &, dispX]],
    dispX,
    TestID -> "Displacement-reduce-deblur"
]

(* predicate hierarchy: clamped grid step is single-valued but no bijection; cycle step is a Killing displacement *)
VerificationTest[
    {DisplacementSingleValuedQ @ dispX, DisplacementBijectionQ @ dispX,
     DisplacementBijectionQ @ dispCycleStep, DisplacementIsomorphismQ[CycleGraph[10], dispCycleStep]},
    {True, False, True, True},
    TestID -> "Displacement-predicates"
]

(* k-continuity: the clamped step is 1-continuous *)
VerificationTest[
    ContinuousDisplacementQ[dispGrid, dispX],
    True,
    TestID -> "Displacement-continuity"
]

(* weak, Hausdorff and strong continuity distinguish their set quantifiers *)
VerificationTest[
        With[{graph = PathGraph[Range[4]], displacement = <|1 -> {1, 4}, 2 -> {2}, 3 -> {3}, 4 -> {4}|>},
            ContinuousDisplacementQ[graph, displacement, Method -> #] & /@ {"Weak", "Hausdorff", "Strong"}],
        {True, False, False},
        TestID -> "Displacement-continuity-methods"
]

(* smallest Killing displacement of the cycle is a unit rotation *)
VerificationTest[
    With[{killing = FindKillingDisplacement[CycleGraph[10]]},
            {KillingDisplacementMagnitude[CycleGraph[10]], killing, DisplacementIsomorphismQ[CycleGraph[10], killing]}],
        {1, FindKillingDisplacement[CycleGraph[10]], True},
    TestID -> "Displacement-killing-cycle"
]

(* exact commutators of Killing displacements close and reverse by inversion *)
VerificationTest[
        With[
            {graph = CycleGraph[10], rotation = dispCycleStep,
             reflection = AssociationMap[{Mod[2 - #, 10, 1]} &, Range @ 10]},
            With[
                {xy = DisplacementCommutator[graph, rotation, reflection],
                 yx = DisplacementCommutator[graph, reflection, rotation]},
                {DisplacementIsomorphismQ[graph, xy], xy === DisplacementInverse @ yx}]],
        {True, True},
        TestID -> "Displacement-commutator-killing-closure-antisymmetry"
]

(* inverse and negative commutator loops remain separately selectable *)
VerificationTest[
        {DisplacementCommutator[dispGrid, dispX, dispY, Method -> "Inverse"] ===
             DisplacementCommutator[dispGrid, dispX, dispY],
         DisplacementCommutator[dispGrid, dispX, dispY, Method -> "Negative"] ===
             DisplacementBracket[dispGrid, dispX, dispY]},
        {True, True},
        TestID -> "Displacement-commutator-methods"
]

(* the outward radial displacement is the gradient of the distance from the centre *)
VerificationTest[
    First @ PolarDisplacements[dispGrid, {3, 3}] ===
      GradientDisplacement[dispGrid, AssociationThread[VertexList @ dispGrid, GraphDistance[dispGrid, {3, 3}]]],
    True,
    TestID -> "Displacement-radial-is-gradient"
]

(* random displacements are continuous sections of the scale-r tangent bundle *)
VerificationTest[
    SeedRandom[7]; With[{d = RandomDisplacement[dispGrid, 2]},
      {ContinuousDisplacementQ[dispGrid, d], DisplacementMagnitude[dispGrid, d] <= 2}],
    {True, True},
    TestID -> "Displacement-random-continuous"
]


(* ===================== Example substrates ===================== *)

(* a patch models an open subset of the plane: connected, and with no pendant vertex, since a vertex
   with a single neighbour represents nothing in the plane *)
VerificationTest[
    AllTrue[{"Plane", "Triangular", "Square", "Hexagonal"},
      With[{g = ExampleGraphData[#, "Large"]}, ConnectedGraphQ[g] && Min[VertexDegree[g]] >= 2] &],
    True,
    TestID -> "ExampleGraphData-patches-open"
]

(* the interior of a tiling patch carries the valence the tiling prescribes *)
VerificationTest[
    Max @ VertexDegree @ ExampleGraphData[#, "Large"] & /@ {"Triangular", "Square", "Hexagonal"},
    {6, 4, 3},
    TestID -> "ExampleGraphData-tiling-valence"
]

(* the frozen table and the live generator agree on the tilings, which carry no randomness *)
VerificationTest[
    AllTrue[{"Triangular", "Square", "Hexagonal"},
      IsomorphicGraphQ[ExampleGraphData[#, "Large"], ExampleGraphData[#, 12]] &],
    True,
    TestID -> "ExampleGraphData-frozen-matches-live"
]

(* tiers are strictly increasing in size *)
VerificationTest[
    AllTrue[ExampleGraphData[],
      OrderedQ[VertexCount /@ Lookup[ExampleGraphData[#], {"Small", "Medium", "Large"}]] &],
    True,
    TestID -> "ExampleGraphData-tiers-increase"
]

VerificationTest[
    AmbientGraphStyle[],
    {"Default", "GrayFaint", "GrayOpaque", "Gray"},
    TestID -> "AmbientGraphStyle-names"
]


(* ===================== Inflation ===================== *)

(* inflation leaves the base intact: it survives as the induced subgraph on the original vertices *)
VerificationTest[
    SeedRandom[42]; With[{base = GridGraph[{6, 6}]},
      {inf = InflateGraph[base, "ExtraVertices" -> {1, 3}, "ExtraEdges" -> 1]},
      {IsomorphicGraphQ[Subgraph[inf, VertexList @ base], base], VertexCount[inf] > VertexCount[base]}],
    {True, True},
    TestID -> "InflateGraph-base-recoverable"
]

(* every base vertex carries a fiber of the requested size, and each fiber vertex is joined to it *)
VerificationTest[
    SeedRandom[42]; With[{base = CycleGraph[8]},
      {inf = InflateGraph[base, "ExtraVertices" -> 2, "ExtraEdges" -> 0, "Density" -> 0]},
      {VertexCount[inf], AllTrue[VertexList @ base, MemberQ[AdjacencyList[inf, InflatedVertex[#, 1]], #] &]}],
    {24, True},
    TestID -> "InflateGraph-fibers-attached"
]

(* cross-fiber edges only join fibers whose base vertices lie within "Radius" of each other *)
VerificationTest[
    With[{base = GridGraph[{6, 6}]},
      AllTrue[{1, 2, 3},
        Function[r,
          SeedRandom[7];
          AllTrue[
            Cases[EdgeList @ InflateGraph[base, "ExtraVertices" -> 2, "Radius" -> r, "Density" -> 2],
              UndirectedEdge[InflatedVertex[a_, _], InflatedVertex[b_, _]] /; a =!= b :> {a, b}],
            GraphDistance[base, First @ #, Last @ #] <= r &]]]],
    True,
    TestID -> "InflateGraph-radius-respected"
]

(* fiber size is sampled inside the given range, per base vertex *)
VerificationTest[
    SeedRandom[3]; With[{base = CycleGraph[20]},
      {inf = InflateGraph[base, "ExtraVertices" -> {1, 4}]},
      {sizes = Length /@ GroupBy[Cases[VertexList @ inf, InflatedVertex[v_, _] :> v], Identity]},
      {Min @ Values @ sizes >= 1, Max @ Values @ sizes <= 4, Length @ sizes == 20}],
    {True, True, True},
    TestID -> "InflateGraph-fiber-size-range"
]

EndTestSection[]
