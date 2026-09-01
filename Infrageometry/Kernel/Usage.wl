Package["WolframInstitute`Infrageometry`"]

(* Usage messages for exported symbols. Experimental functions are marked (experimental). *)

ComplexClosure::usage = "ComplexClosure[g] returns the simplicial closure of a list of simplices g.";
IndexComplex::usage = "IndexComplex[g] relabels vertices of complex g to a consecutive ordering of integers.";

SimplexDimension::usage = "SimplexDimension[s] gives the dimension (#vertices - 1) of simplex s.";
ComplexDimension::usage = "ComplexDimension[g] gives the maximal simplex dimension of complex g (or -1 for empty).";
ComplexDimensions::usage = "ComplexDimensions[g] gives inductive vertex dimensions of complex g.";
ComplexInductiveDimension::usage = "ComplexInductiveDimension[g] gives the mean of vertex dimensions (inductive dimension).";
SimplexList::usage = "SimplexList[g, k] lists all simplices of dimension <= k (or within a range {kmin,kmax}).";
ComplexBones::usage = "ComplexBones[g] returns interior (d-2)-faces (\"bones\") of a pure d-dimensional complex.";
ComplexWalls::usage = "ComplexWalls[g] returns codimension-1 faces (\"walls\") of a pure complex (potential boundary facets).";
ComplexFacets::usage = "ComplexFacets[g] returns all maximal simplices (facets) of g.";
ComplexFrames::usage = "ComplexFrames[g] returns the ordered facets (frames) of g: every permutation of each maximal simplex.";
ComplexHypergraph::usage = "ComplexHypergraph[g] returns the maximal simplices of g as a hypergraph, peeling top-down by dimension so faces already contained in a higher simplex are dropped.";
ComplexVertexList::usage = "ComplexVertexList[g] returns the sorted list of vertices in g.";
SimplexCardinality::usage = "SimplexCardinality[g, k] gives the number of k-dimensional simplices in g.";
SimplexCardinalities::usage = "SimplexCardinalities[g] gives counts of simplices in each dimension (f-vector without the leading 1).";
SimplexStar::usage = "SimplexStar[g, s] gives the star: all simplices containing s.";
SimplexStarSphere::usage = "SimplexStarSphere[g, s] gives the star of s with s itself removed (the simplices strictly containing s).";
SimplexCore::usage = "SimplexCore[g, s] gives all simplices contained in s.";
SimplexCoreSphere::usage = "SimplexCoreSphere[s] gives the proper faces of s: its closure with s itself removed.";
SimplexUnitSphere::usage = "SimplexUnitSphere[g, s] gives the unit sphere (link) around simplex s (its star minus interior).";
SimplexMirror::usage = "SimplexMirror[g, s] returns the \"mirror\" simplex made by the first vertex absent from each coface in the star boundary (used for geodesic continuation).";
SimplexBoundary::usage = "SimplexBoundary[s] lists codimension-1 faces of simplex s.";
ComplexDual::usage = "ComplexDual[g] returns a dual cell structure (barycentric dual) of g.";
SimplexIndex::usage = "SimplexIndex[s] gives signed index weight of simplex s; SimplexIndex[s,t] gives relative index if s and t coincide as sets.";
SimplexSign::usage = "SimplexSign[s] is the permutation signature of vertex ordering of s; SimplexSign[s,t] relative signature if same underlying set.";
SimplexWeight::usage = "SimplexWeight[s] is (-1)^(Length[s]).";
ContractibleQ::usage = "ContractibleQ[g] attempts a recursive star decomposition test for contractibility of g.";
ComplexSphereQ::usage = "ComplexSphereQ[g] heuristically tests if g is a combinatorial sphere (vertex links spheres + Euler).";
ComplexManifoldQ::usage = "ComplexManifoldQ[g] tests if all vertex links are spheres or balls (pseudomanifold / manifold condition).";
SimplicialMap::usage = "SimplicialMap[g, perm] builds a simplicial map induced by a vertex permutation.";

ComplexJoin::usage = "ComplexJoin[a,b] returns the join of simplicial complexes a and b.";

ComplexEulerCharacteristic::usage = "ComplexEulerCharacteristic[g] gives the alternating sum of simplex counts of g.";
ComplexFermiCharacteristic::usage = "ComplexFermiCharacteristic[g] gives the multiplicative Fermi characteristic (product of simplex weights).";
LefschetzNumber::usage = "LefschetzNumber[g] estimates average Lefschetz number of random automorphisms; LefschetzNumber[g,map] uses a given simplicial map.";
LefschetzCurvature::usage = "LefschetzCurvature[g] gives simplexwise curvature contributions via Lefschetz fixed point indices.";

ComplexPolynomial::usage = "ComplexPolynomial[g, t] gives the f-polynomial 1 + sum f_i t^i of g.";
PoincarePolynomial::usage = "PoincarePolynomial[g, t] gives the Poincaré polynomial from Betti numbers of g.";
ComplexCurvature::usage = "ComplexCurvature[g, t] integrates the f-polynomial symbolically.";
ComplexCurvatures::usage = "ComplexCurvatures[g, t] gives curvature values for each simplex using its unit sphere.";
DehnSommervilleQ::usage = "DehnSommervilleQ[g] tests the Dehn–Sommerville relations f(t)=f(-1-t).";

GraphComplex::usage = "GraphComplex[g, k] returns the clique complex of graph g up to dimension k (or range).";
SkeletonComplex::usage = "SkeletonComplex[g] returns the 2-skeleton clique complex of graph g.";
ComplexGraph::usage = "ComplexGraph[g] returns the 1-skeleton graph of simplicial complex g.";
FaceGraph::usage = "FaceGraph[g] returns incidence graph between simplices and subfaces.";
BarycentricRefinement::usage = "BarycentricRefinement[g] returns the barycentric subdivision (complex, graph, or iterated form).";

GraphTopology::usage = "GraphTopology[g] returns star neighborhoods of each simplex of the clique complex of g.";
AlexandrovTopology::usage = "AlexandrovTopology[g] returns the specialization/Alexandrov topology induced by the face poset of g.";

IndexMatrix::usage = "IndexMatrix[g,k] returns signed incidence between (k+1)- and k-simplices; IndexMatrix[g] defaults to full complex.";
SignMatrix::usage = "SignMatrix[g,k] returns orientation sign incidence matrix between (k+1)- and k-simplices.";
FaceMatrix::usage = "FaceMatrix[i][g, x, y] gives the Boolean matrix of the i-th face map between simplex lists x and y (entry 1 when deleting vertex i of a y-simplex yields the x-simplex).";
ComplexIncidenceMatrix::usage = "ComplexIncidenceMatrix[g,k] returns the oriented incidence matrix at dimension k.";
ConnectionMatrix::usage = "ConnectionMatrix[g] returns Euler-characteristic based connection matrix among simplices.";
GreenFunctionMatrix::usage = "GreenFunctionMatrix[g] returns the inverse of ConnectionMatrix[g] in closed form via star-intersection Euler characteristics (Knill's unimodularity theorem). Distinct from GreenOperatorMatrix.";
GreenOperatorMatrix::usage = "GreenOperatorMatrix[g] returns the Moore-Penrose pseudoinverse of the Hodge Laplacian (d + d^T)^2, block-diagonal by simplex degree. Top-left block of GreenOperatorMatrix[GraphComplex[g]] is the graph-Laplacian pseudoinverse.";
HodgePropagatorMatrix::usage = "HodgePropagatorMatrix[g, k] returns the Moore-Penrose pseudoinverse of the boundary operator d_k = ComplexIncidenceMatrix[g, k], computed as (d_k^T d_k)^+ d_k^T. HodgePropagatorMatrix[g, All] returns the list over k = 0..ComplexDimension[g] - 1.";
DiracHodgeMatrix::usage = "DiracHodgeMatrix[g] returns d + d^T for the simplicial complex g.";
DiracConnectionMatrix::usage = "DiracConnectionMatrix[g] returns L + L^T for connection matrix L.";
DiracBlockMatrix::usage = "DiracBlockMatrix[g] is block diagonal with incidence blocks.";
DiracDualBlockMatrix::usage = "DiracDualBlockMatrix[g] is dual block diagonal with transposed incidence products.";
DiracColumns::usage = "DiracColumns[g] partitions DiracHodgeMatrix columns by simplex dimensions.";
DiracDualColumns::usage = "DiracDualColumns[g] returns transposed column blocks of DiracHodgeMatrix.";
HodgeBlock::usage = "HodgeBlock[g] returns (d + d^T)^2 for complex g (global, not partitioned).";
HodgeMatrix::usage = "HodgeMatrix[g] returns block-diagonal Hodge Laplacian matrices by dimension.";
HodgeLaplacianMatrix::usage = "HodgeLaplacianMatrix[g] returns the block form (d^T d + d d^T) aggregated as DiracBlockMatrix^2.";
BettiVector::usage = "BettiVector[g] returns Betti numbers (nullities) for each dimension of g.";

MatrixBlocks::usage = "MatrixBlocks[m, blocks] splits square matrix m into block submatrices according to cardinalities blocks.";
MatrixNullity::usage = "MatrixNullity[m] gives the dimension of the null space of square matrix m.";
MatrixColumns::usage = "MatrixColumns[m, blocks] extracts grouped column blocks from square matrix m.";
SuperTrace::usage = "SuperTrace[mat] returns graded trace (alternating signs) or from vector of traces.";
PseudoDeterminant::usage = "PseudoDeterminant[m] gives product of nonzero eigenvalues of m.";
SuperDeterminant::usage = "SuperDeterminant[m] gives graded determinant from pseudo-determinants of blocks.";

GraphSuspension::usage = "GraphSuspension[g] adds two new universal vertices and returns a 3D graph embedding.";
RandomGraphAutomorphism::usage = "RandomGraphAutomorphism[g, n] samples n automorphisms (All for all, Automatic for one).";

(* Graph.wl *)
GraphEdgeWeights::usage = "GraphEdgeWeights[g] gives edge weights (default 1) for graph g.";
GraphVertexWeights::usage = "GraphVertexWeights[g] gives vertex weights (default 1) for graph g.";
GraphBoundary::usage = "GraphBoundary[g, S] gives the inner vertex boundary of S in g (vertices where a g-edge escapes S). If S is a vertex list it is treated as the induced subgraph, so the boundary is the vertices of S adjacent to some vertex outside S; if S is a subgraph h, the boundary is the vertices of h having a g-neighbor they are not joined to in h (so a path/curve, lacking its induced chords, is all boundary).";
GraphInterior::usage = "GraphInterior[g, S] gives the interior of S in g (= S minus GraphBoundary[g, S]): the vertices all of whose g-edges stay inside the object. S may be a vertex list (induced-subgraph notion) or a subgraph h (its own edges, so a 1-D curve has empty interior).";
GraphExteriorBoundary::usage = "GraphExteriorBoundary[g] gives the exterior-boundary (rim) vertices of the whole graph, detected from vertex degrees; GraphExteriorBoundary[mr] gives the exact surface vertices of the MeshRegion (the vertices on a facet belonging to exactly one top cell). Option Method (\"AverageDegree\" (default, for meshes: the vertices of below-average degree) | \"MaxDegree\" (for lattices: the vertices of less than full degree)); a MeshRegion needs no method. Complements GraphBoundary, the inner boundary of a subset.";
BoundarylessGraph::usage = "BoundarylessGraph[g] deletes every edge joining two exterior-boundary vertices (GraphExteriorBoundary[g]) and then the vertices this isolates, preserving the original vertex coordinates and their dimension: the rim contour disappears while boundary vertices with an inward edge survive as whiskers, so the result models an open window onto the geometry. BoundarylessGraph[mr] does the same on the 1-skeleton of the MeshRegion, with the exact surface as the boundary. Options: Method (passed to GraphExteriorBoundary; Graph form only), \"KeepCoordinates\" (True (default) carries the coordinates over, False drops them).";
RelativeEccentricity::usage = "RelativeEccentricity[g] gives (e(v) - radius)/(diameter - radius) for each vertex in VertexList order -- 0 on GraphCenter, 1 on GraphPeriphery. Also takes a distance matrix. Identically 0 when diameter == radius or the graph is disconnected.";
RelativeEccentricitySubgraph::usage = "RelativeEccentricitySubgraph[g, {lo, hi}] gives the induced subgraph on { v : lo <= RelativeEccentricity[g][[v]] <= hi }, the substrate cut to a relative depth; a bare q means {0, q}, and the default band is everything. Vertex labels and coordinates are g's, so a construction made on the band draws on g. Relative-depth cousin of BoundarylessGraph.";
BallHull::usage = "BallHull[g, S] gives the ball hull of vertex subset S in g: the intersection of all closed metric balls containing S, equivalently { v : d(c, v) <= max_{s in S} d(c, s) for every vertex c }. This is the smallest ball-convex (Mazur) superset of S. S may be a vertex list or a subgraph.";
BallVolumes::usage = "BallVolumes[g, v] gives the List {V(0), ..., V(ecc(v))} of cumulative ball volumes V(r) = |B_r(v)| (position i is radius i - 1). Slot 2 is a single vertex, a list, or All (default); slot 3 selects radii: r_Integer returns the scalar V(r), {rmin, rmax} a rectangular window (saturating past eccentricity, so a fixed window over a vertex list Transposes for subset statistics), All (default) the full profile. Option \"Measure\" picks the counting convention (\"Counting\" (default) = |B_r| | \"Hausdorff\" = |B_r| - |dB_r| | \"Front\" = V(t) = sum_{s<=t} |S_s(v)|, the cumulative passage count of the bouncing advancing front, the slot-3 index t being a step count rather than a radius -- it propagates past eccentricity, and All defaults to 2 ecc(v) steps). The growth invariant underlying VolumeGrowthObservables; BallVolumes (default \"Counting\") == Accumulate[ShellAreas].";
ShellAreas::usage = "ShellAreas[g, v] gives the List {A(0), ..., A(ecc(v))} of geodesic-sphere areas A(r) = #{w : d(v, w) == r} (position i is radius i - 1), the radial derivative of BallVolumes (A(1) is the coordination number; Accumulate recovers BallVolumes[\"Closed\"] -- the OEIS coordination sequence). Slot 2 is a single vertex, a list, or All (default); slot 3 selects radii: r_Integer returns the scalar A(r), {rmin, rmax} a rectangular window (0 past eccentricity), All (default) the full sequence.";
CylinderVolumes::usage = "CylinderVolumes[g, sources, targets, s] gives the matrix of cylinder volumes between every source-target pair; the cylinder from p to q is the metric interval I(p, q) = { w : d(p, w) + d(w, q) == d(p, q) } (all p-q geodesics) thickened to its closed s-neighborhood (s defaults to 0), and its volume is the vertex count. A scalar source gives a flat list ordered as targets.";
TubeVolumes::usage = "TubeVolumes[g, core] gives the tube-volume profile {T(0), ..., T(sMax)} of the vertex list core, T(s) = |{ w : d(w, core) <= s }| (T(0) = |core|). TubeVolumes[g, p, q] takes core = the metric interval I(p, q). Slot 3 (respectively 4) selects radii as in BallVolumes: s_Integer the scalar T(s), {smin, smax} a rectangular window, All (default) the full profile. For a geodesic core with direction v the quotient fit reads the Ricci projection tau + Ric(v, v): DimensionCurvatureFit[..., \"Probe\" -> \"Tube\"].";
GeodesicIntervalGraph::usage = "GeodesicIntervalGraph[g, u, v] gives the metric interval I(u, v) = { w : d(u, w) + d(w, v) == d(u, v) } (all u-v geodesics) as a directed acyclic graph, edges w -> x oriented along increasing distance from u so directed u -> v paths are exactly the geodesics; built from distance fields without enumerating paths.";
GeodesicOccupation::usage = "GeodesicOccupation[dag] gives the association w -> c(w) of per-vertex geodesic occupation over a geodesic DAG, c(w) = (number of source -> w paths) * (number of w -> sink paths) by topological-order DP, with family size Max[c]; GeodesicOccupation[g, u, v] builds the u-v geodesic DAG first.";

GeodesicEdgeOccupation::usage = "GeodesicEdgeOccupation[dag] gives the association DirectedEdge[u, v] -> c(u -> v) of per-edge geodesic occupation over a geodesic DAG, c(u -> v) = (number of source -> u paths) * (number of v -> sink paths) by topological-order DP; GeodesicEdgeOccupation[g, u, v] builds the u-v geodesic DAG first.";
FormanRicciCurvature::usage = "FormanRicciCurvature[g] returns Association[edge -> kappa] with Forman's combinatorial Ricci curvature on the clique complex of g. Options: \"OnCells\" (Integer | All | {dims}; output cell dimension(s), default 1) and \"MaxCellDimension\" (Automatic (default) | Integer; clique-complex truncation, Automatic = Max[OnCells] + 1).";
OllivierRicciCurvature::usage = "OllivierRicciCurvature[g] returns Association[edge -> kappa] with the Ollivier-Ricci curvature kappa(u, v) = 1 - W_1(mu_u, mu_v) / d(u, v), where mu_x is uniform on N(x) and W_1 is the Wasserstein-1 distance under graph distance (alpha = 0).";
LogDifferenceQuotients::usage = "LogDifferenceQuotients[w] gives the log-difference quotients q(r) = (Log w(r) - Log w(r-1)) / (Log(r+1) - Log r) of a sequence w = {w(0), w(1), ...}, the discrete d Log w / d Log r; equals ResourceFunction[\"LogDifferences\"][w]. Accepts any numeric or Around sequence: feed BallVolumes[g, v] for the volume-growth dimension estimator, or LogDifferenceQuotients[MeanAround /@ Transpose[BallVolumes[g, subset, {0, R}]]] to average a vertex subset first and carry the spread into Around error bars.";
VolumeGrowthObservables::usage = "VolumeGrowthObservables[g, v, window] returns the growth observables at vertex v as one flat association bundling the raw profiles and the fitted parameters: <|\"BallVolumes\" -> {V(0), ...}, \"ShellAreas\" -> {A(0), ...}, \"BallLogDifferenceQuotients\" -> {q_V(1), ...}, \"SphereLogDifferenceQuotients\" -> {q_A(1), ...}, \"BallDimension\" -> d, \"SphereDimension\" -> n, \"BallScalarCurvature\" -> R, \"SphereScalarCurvature\" -> S, \"BallCurvatureByRadius\" -> {R(1), ...}, \"SphereCurvatureByRadius\" -> {S(1), ...}, \"SphereMeanCurvatureByRadius\" -> {H(1), ...}, \"BallWindow\" -> {rmin, rmax}, \"SphereWindow\" -> {rmin, rmax}|>. Both growth probes are fitted by DimensionCurvatureFit, Bishop-Gromov regression of the log-difference quotient q(r) on r (r + 1): the ball (volume V(r)) intercept is the dimension d with R = -3 (d + 2) slope; the sphere (area A(r) = ShellAreas, Gray's Area(S_r) = sigma_{n-1} r^(n-1) (1 - S/(6 n) r^2)) intercept is n - 1, reported as the manifold dimension n = intercept + 1 with S = -3 n slope. \"...CurvatureByRadius\" is the per-radius comparison profile (ball 6 (d + 2)/r^2 (1 - V/V_E); sphere 6 n/r^2 (1 - A/A_E)); \"SphereMeanCurvatureByRadius\" is the discrete geodesic-sphere mean curvature d Log A/dr. The two probes give independent (n, S) readouts, so their agreement is a consistency check. The window is {rmin, rmax}, All, or Automatic (default), the linear core of the (x, q) scatter; \"BallWindow\" and \"SphereWindow\" record the windows actually used (they differ under Automatic since the sphere window is capped at the rising part of A(r)). The log-difference quotient is the radius-consistent log-log slope q(r) = (Log f(r) - Log f(r-1))/(Log r - Log(r-1)) (not the index-based LogDifferenceQuotients), and the ball fit runs directly on the chosen measure with no extra shift, so \"BallVolumes\" and \"BallLogDifferenceQuotients\" are exactly the series the fit consumes. VolumeGrowthObservables[g, verts, window] returns one association per vertex and VolumeGrowthObservables[g] (= VolumeGrowthObservables[g, All]) one per VertexList[g]. Options: \"Dimension\" (Automatic (default) | Integer, pins the intercept) and \"Measure\" (\"Hausdorff\" (default, the boundary-corrected |GraphInterior[B_r]|) | \"Counting\").";
DimensionCurvatureFit::usage = "DimensionCurvatureFit[{{r, q(r)}, ...}] fits the dimension d and scalar curvature R to log-difference quotients q(r) paired with their radii by Bishop-Gromov regression on x = r (r + 1), the squared geometric-mean radius Sqrt[r(r+1)], returning <|\"Dimension\" -> d, \"ScalarCurvature\" -> R|>. DimensionCurvatureFit[{q(0), q(1), ...}] takes a bare quotient list at radii 0, 1, 2, .... Every supplied point is fitted (window by slicing the quotients first). The caller supplies the quotients, so the convention is its choice -- e.g. DimensionCurvatureFit[LogDifferenceQuotients[BallVolumes[g, v, \"Counting\"]]] for the index-based counting estimator; Around-valued q (averaging a vertex subset first) carries its spread through the closed-form fit to Around dimension and curvature. Options: \"Probe\" (\"Ball\" (default), intercept d, R = -3 (d + 2) slope | \"Sphere\", intercept d - 1, R = -3 d slope | \"Tube\", intercept d - 1, tau + Ric(v,v) = -3 (d + 1) slope | \"TubeMantle\", intercept d - 2, tau + Ric(v,v) = -3 (d - 1) slope) and \"Dimension\" (Automatic (default) | Integer, pins the intercept).";
SectionalCurvatures::usage = "SectionalCurvatures[g] estimates discrete sectional curvatures for edge neighborhoods (if implemented).";
EffectiveResistance::usage = "EffectiveResistance[g, u, v] returns the Klein-Randic resistance distance R(u, v) = (e_u - e_v)^T L^+ (e_u - e_v) for the graph Laplacian pseudoinverse L^+. EffectiveResistance[g] returns the full V x V matrix; EffectiveResistance[g, vs] the submatrix on a vertex list.";
ResistanceQ::usage = "ResistanceQ[r] tests whether a real symmetric n x n matrix r with zero diagonal is realisable as a resistance distance matrix (Klein-Randic / Schoenberg negative-type criterion: the centred Gram matrix is positive semidefinite).";
SpacetimeGraph::usage = "SpacetimeGraph[{m,n}] builds a 2D causal diamond style spacetime graph grid.";
SpacetimeTorusGraph::usage = "SpacetimeTorusGraph[{m,n}] builds a periodic (toroidal) spacetime graph.";
RotateEdge::usage = "RotateEdge[e,{m,n}] rotates a directed edge in toroidal coordinates.";
KickEdge::usage = "KickEdge[e] applies one 'kick' transition edge transformation.";
PersistEdge::usage = "PersistEdge[e] returns edge advanced forward without lateral change.";
RightMatrix::usage = "RightMatrix[g,{m,n}] returns sparse right-step transition matrix for a graph walk.";
ToroidalRightMatrix::usage = "ToroidalRightMatrix[g] returns right-walk transition matrix with wrap-around edges.";
ToroidalLeftMatrix::usage = "ToroidalLeftMatrix[g] returns left-walk transition matrix with wrap-around edges.";
DiracWalk::usage = "DiracWalk[g,p] returns coined quantum walk transition matrix (mix of kick/persist).";
VertexAmplitudes::usage = "VertexAmplitudes[g, edgeWeights,{m,n}] returns complex amplitude per vertex from adjacent weighted edges.";

(* Topology.wl *)
BallTopology::usage = "BallTopology[g, r] returns the Hasse diagram of the r-ball specialization preorder on V(g): directed edge q -> p iff the closed r-ball at p is contained in the one at q, transitive edges removed. This digraph is the topology object consumed by the Topological* operators. Option \"Dual\" (False default; True gives the ReverseGraph).";
TopologicalClosure::usage = "TopologicalClosure[topo, verts] gives the closure of vertex list verts in the specialization-preorder digraph topo: the union of the in-components (down-sets) of the vertices.";
TopologicalInterior::usage = "TopologicalInterior[topo, verts] gives the interior int(S) = V \\ cl(V\\S) of vertex list verts in the digraph topo, with carrier V = VertexList[topo].";
TopologicalBoundary::usage = "TopologicalBoundary[topo, verts] gives the (two-sided) boundary cl(S) \\ int(S) of vertex list verts in the digraph topo.";
TopologicalNeighborhood::usage = "TopologicalNeighborhood[topo, verts] gives the unique minimal open neighborhood of vertex list verts in the digraph topo: the union of the out-components (up-sets) of the vertices.";
ContinuousMapQ::usage = "ContinuousMapQ[f, topo1, topo2] tests whether the vertex map f is continuous from preorder digraph topo1 to topo2, i.e. every Hasse edge q -> p of topo1 maps to a pair reachable in the transitive closure of topo2. f: Association, list of Rule, or callable.";
TopologyGraph::usage = "TopologyGraph[g, topo] draws the graph g overlaid with the Hasse arrows of the specialization-preorder digraph topo.";

(* Mesh.wl *)
ComplexEmbedding::usage = "ComplexEmbedding[g,d] numerically embeds complex g in R^d via energy minimization (d=2 or 3).";
ComplexMesh::usage = "ComplexMesh[g,d] builds a MeshRegion from complex g (with optional explicit coordinates).";
HighlightComplex::usage = "HighlightComplex[g, h] renders the mesh of complex g with the simplices in h highlighted (each h entry a vertex list, optionally wrapped in Style).";
GraphMesh::usage = "GraphMesh[g] builds a MeshRegion from graph g using its embedding + cliques up to size 4.";
MeshComplex::usage = "MeshComplex[mr] returns the simplicial complex induced by MeshRegion mr.";
MeshIncidenceMatrix::usage = "MeshIncidenceMatrix[mr,k] gives oriented incidence matrix between (k-1)- and k-cells.";
CellOrientation::usage = "CellOrientation[cell] gives orientation sign (+/-1) of a mesh cell.";
FlipCellOrientation::usage = "FlipCellOrientation[cell] reverses or rotates orientation of a cell consistently.";
HighlightCellOrientations::usage = "HighlightCellOrientations[mr] colors top-dimensional cells by orientation sign.";
OrientMeshRegion::usage = "OrientMeshRegion[mr] attempts to orient all top-dimensional cells consistently.";
TriangulateArrayMesh::usage = "TriangulateArrayMesh[mr] refines a mesh to bounded cell measure.";
OrientableMeshRegionQ::usage = "OrientableMeshRegionQ[mr] heuristically tests orientability via connectivity of oriented adjacency.";
MoebiusMesh::usage = "MoebiusMesh[nu,nv] constructs a triangulated Möbius band strip.";
PathMesh::usage = "PathMesh[list] returns a simple path mesh through given vertices.";
UniformLengthGraph::usage = "UniformLengthGraph[region, n] returns the contact graph of an n-sphere hard-sphere packing relaxed in region (filling a solid, meshing a surface); every edge has length 2r. Option Method (\"IterativeProjection\" (default), \"ConstrainedPacking\"); \"Radius\" (Automatic spaces the spheres to tile the region's content); \"KeepCoordinates\" (False (default) drops the packing coordinates, True stores them as VertexCoordinates).";
UniformLengthEmbedding::usage = "UniformLengthEmbedding[graph] embeds graph in R^d (option \"Dimension\") so every edge is a unit segment, returning coordinates in VertexList order (cf. GraphEmbedding); the iterative counterpart of ComplexEmbedding.";

(* Chains.wl *)
SymmetricRelationGraph::usage = "SymmetricRelationGraph[f, assoc] builds an undirected graph joining keys whose values satisfy relation f.";
SpatialReconstruction::usage = "SpatialReconstruction[g, slice, n] returns spatial relation graph from causal graph slice after n steps.";
CausalGraphSimpleChains::usage = "CausalGraphSimpleChains[g] finds simple source-sink path families in causal graph g.";
CoordinatizeCausalGraph::usage = "CoordinatizeCausalGraph[g,{obs1,obs2}] assigns 2D coordinates from pairs of observer chains.";
CoordinatizedCausalGraph::usage = "CoordinatizedCausalGraph[g,{obs1,obs2}] builds a reduced causal graph with observer highlighting.";

(* Experimental.wl (experimental) *)
FaceVector::usage = "FaceVector[g] gives the f-vector (excluding empty face) of complex g.";
HVector::usage = "HVector[g] gives the h-vector derived from the f-vector of g.";
GVector::usage = "GVector[g] gives the g-vector (first differences of h).";
LinkComplex::usage = "LinkComplex[g,s] returns the link of simplex s in g.";
InducedSubcomplex::usage = "InducedSubcomplex[g,verts] returns the induced subcomplex on a vertex subset.";
VertexDeletion::usage = "VertexDeletion[g,v] removes vertex v (and incident simplices) returning the induced subcomplex.";
ComplexUnion::usage = "ComplexUnion[a,b] returns closure of union of complexes a and b.";
ComplexIntersection::usage = "ComplexIntersection[a,b] returns simplices common to a and b (no closure).";
ComplexDifference::usage = "ComplexDifference[a,b] returns closure of simplices in a not in b.";
PureComplexQ::usage = "PureComplexQ[g] tests whether all facets have identical dimension.";
RandomSimplicialComplex::usage = "RandomSimplicialComplex[n,p,max] generates Linial–Meshulam random complex up to max dimension.";
RandomFlagComplex::usage = "RandomFlagComplex[n,p,k] generates an Erdős–Rényi G(n,p) then clique complex up to dimension k.";
FacetGraph::usage = "FacetGraph[g] returns the dual adjacency graph of facets sharing a ridge.";
BettiAssociation::usage = "BettiAssociation[g] gives an association from dimension to Betti number.";
EulerBettiConsistencyQ::usage = "EulerBettiConsistencyQ[g] checks Euler characteristic equals alternating Betti sum.";
EnumerateComplexes::usage = "EnumerateComplexes[verts,d] generates all simplicial complexes on given vertex set with maximal dimension d (default Infinity). Use option \"MaxCount\"->n to cap enumeration size.";
DiscreteDirichletEnergy::usage = "DiscreteDirichletEnergy[g,f] gives 1/2 Sum_{(u,v)} (f[u]-f[v])^2 over edges of the 1-skeleton of complex or graph g; f may be an Association or list aligned to vertex order.";

(* Geodesics *)
ComplexGeodesicFlow::usage = "ComplexGeodesicFlow[g, path] performs one geodesic extension step of a simplex path within complex g via mirror continuation.";
SimplexOrbit::usage = "SimplexOrbit[g, path, n] iteratively applies ComplexGeodesicFlow up to n steps (default Infinity) or until the path self-intersects.";
ComplexGeodesics::usage = "ComplexGeodesics[g] returns a list of unique geodesics starting from all the facets.";

(* SimplicialSet.wl *)
SimplicialData::usage = "SimplicialData[faceBlocks, degeneracyBlocks] is a container for face- and degeneracy-blocks of correct dimensions representing a simplicial set structure.";
SimplicialSet::usage = "SimplicialSet[simplices] constructs the minimal simplicial set that contains the given set of simplices. Options: \"IncludeDegeneracies\"->True (default), \"FlattenMultiplicities\"->True (default).";
SimplicialSetQ::usage = "SimplicialSetQ[sd] checks if the given simplicial data form a valid simplicial set (face and degeneracy maps satisfy simplicial identities).";
DeltaComplexQ::usage = "DeltaComplexQ[sd] checks if the given simplicial data form a Delta-complex (face maps satisfy simplicial identities, all degeneracy maps are zero).";
SimplicialComplexQ::usage = "SimplicialComplexQ[sd] checks if the given simplicial data form a simplicial complex (each simplex has a unique combination of boundary elements).";

BettiTable::usage = "BettiTable[data, radii, opts] returns <| 'Radii'->rlist, 'Betti'->matrix, 'Dimensions'->{d0,...} |> constructed from BettiCurves. Options: passes through MaxDimension -> k (default Automatic=all).";
BettiCurves::usage = "BettiCurves[data, radii, opts] returns an association r -> {b0,b1,...}. Option MaxDimension->k truncates vectors (Automatic = all).";
VietorisRipsFiltration::usage = "VietorisRipsFiltration[data, radii, opts] returns association r -> Vietoris–Rips complex at scale r. Options: 'MaxDimension'->k (default Infinity), 'Sort'->True/False to control radius sorting.";
VietorisRipsComplex::usage = "VietorisRipsComplex[data, r, k] returns the Vietoris–Rips simplicial complex (clique complex of threshold graph) truncated to dimension k (Infinity for full). VietorisRipsComplex[g, r, k] does the same for a Graph g under its shortest-path metric: the clique complex of the radius-r power graph (at r = 1 this is GraphComplex[g]).";
VietorisRipsThresholdGraph::usage = "VietorisRipsThresholdGraph[data, r, opts] returns the threshold graph joining points with distance <= r. Options: 'Metric'->f (default EuclideanDistance), 'IncludeLoops'->False (future use), 'VertexCoordinates'->True to attach coordinates. VietorisRipsThresholdGraph[g, r] returns instead the r-th power of a Graph g, joining vertices whose graph distance is <= r.";

MiniballRadius::usage = "MiniballRadius[pts] returns the radius of the smallest enclosing ball of the points (BoundingRegion[pts, 'MinBall']).";
BallIntersectionComplex::usage = "BallIntersectionComplex[data, r, k] returns the order-k ball-intersection complex of closed radius-r balls: a simplex is admitted iff every k-subset of its balls has a common point. k = 2 is Vietoris-Rips (equal to VietorisRipsComplex[data, 2 r]), k = Infinity is Cech (the nerve), 2 < k < Infinity interpolates. Options: 'Metric'->EuclideanDistance|matrix|Graph|fn (Euclidean uses the exact miniball test, other metrics an intrinsic intersection oracle over the sample points), 'IntersectionTest'->fn applied to the common region (default = non-empty), 'MaxDimension'->k.";
CechComplex::usage = "CechComplex[data, r] returns the Cech complex (nerve) of closed radius-r balls = BallIntersectionComplex[data, r, Infinity].";
BallIntersectionFiltrationValue::usage = "BallIntersectionFiltrationValue[data, sigma, k] returns f_k(sigma), the birth radius of sigma in the order-k complex: the max miniball radius over its k-subsets (its own miniball when |sigma| <= k). Monotone under faces.";
BallIntersectionFiltration::usage = "BallIntersectionFiltration[data, radii, k] returns association r -> BallIntersectionComplex[data, r, k] over the sorted radii, ready for PersistenceIntervals.";
CechFiltration::usage = "CechFiltration[data, radii] = BallIntersectionFiltration[data, radii, Infinity].";
BallIntersectionBifiltration::usage = "BallIntersectionBifiltration[data, radii, orders] returns the (r, k) object as association k -> (association r -> complex); for fixed r the nesting C^(k) contains C^(k+1) as k grows, saturating to Cech at k = d + 1 for convex balls (Helly).";
PersistentHomology::usage = "PersistentHomology[filtration, opts] returns association dim -> {{birth,death},...} over GF(2). PersistentHomology[data, radii] builds internal filtration first. Option MaxDimension->k (Automatic = all).";
PersistenceIntervals::usage = "PersistenceIntervals[filtration, opts] internal helper implementing Z2 reduction; same output format as PersistentHomology.";
PersistenceDiagram::usage = "PersistenceDiagram[filtration, opts] returns list {dim,birth,death}. Also PersistenceDiagram[data, radii] builds filtration. Option MaxDimension->k.";
PopularNetwork::usage = "PopularNetwork[name, what] returns a requested artifact: 'Graph' (default), 'Description', 'Source', or 'All' (association with all fields).";
PopularNetworkNames::usage = "PopularNetworkNames[] lists available names for PopularNetwork.";
PopularHypergraph::usage = "PopularHypergraph[name, what] returns a hypergraph dataset as list-of-hyperedges ('Hypergraph', default) or derived views: 'IncidenceGraph', '2SectionGraph', 'Description', 'Source', or 'All'.";
PopularHypergraphNames::usage = "PopularHypergraphNames[] lists available hypergraph dataset names (FIM transaction itemsets etc.).";
HypergraphVertexSet::usage = "HypergraphVertexSet[edges] returns the set of distinct vertices in a list-of-hyperedges representation.";
HypergraphVertexCount::usage = "HypergraphVertexCount[edges] gives the number of distinct vertices.";
HyperedgeCount::usage = "HyperedgeCount[edges] gives the number of hyperedges.";
HyperedgeSizes::usage = "HyperedgeSizes[edges] returns a list of the sizes of each hyperedge.";
HyperedgeSizeDistribution::usage = "HyperedgeSizeDistribution[edges] tallies hyperedge sizes as {size,count}.";
HypergraphDegree::usage = "HypergraphDegree[edges] returns an association vertex -> number of incident hyperedges (treating duplicates within a hyperedge once).";
HypergraphMaximalEdges::usage = "HypergraphMaximalEdges[edges] filters to inclusion-maximal hyperedges (removing those strictly contained in another).";
HypergraphComplex::usage = "HypergraphComplex[edges] returns the downward-closed simplicial complex generated by the hyperedges.";
HypergraphLineGraph::usage = "HypergraphLineGraph[edges] returns the intersection graph of hyperedges (one vertex per hyperedge).";
Hypergraph2Section::usage = "Hypergraph2Section[edges] returns the 2-section (primal) graph connecting vertices co-occurring in a hyperedge of size >= 2.";
Weighted2SectionGraph::usage = "Weighted2SectionGraph[edges, min] returns a weighted 2-section graph with edge weights = co-occurrence counts (filtering weights < min).";
HypergraphSummary::usage = "HypergraphSummary[edges] returns an association summarizing counts, size stats, average degree, and degree distribution.";

(* QuantumCalculus.wl *)
WaveEquationSolution::usage = "WaveEquationSolution[g, u0, v0, t] gives the d'Alembert wave equation solution cos(D t) u0 + t sinc(D t) v0 on a simplicial complex g.";
DiscreteWaveStep::usage = "DiscreteWaveStep[g, u, uPrev] returns {uNext, u} for one discrete-time leapfrog wave step on complex g.";
WavePropagator::usage = "WavePropagator[g, t] returns the unitary wave propagator matrix exp(i D t) for complex g.";
WuCharacteristic::usage = "WuCharacteristic[g, m] gives the m-th Wu characteristic (generalized Euler characteristic from m-tuple interactions). Default m=2.";
IsospectralDeformation::usage = "IsospectralDeformation[g, t, n] returns the Dirac matrix after Lax-pair isospectral deformation D'=[d-d*,D] for time t in n steps.";
DiracMass::usage = "DiracMass[g, t] extracts the diagonal mass term from the isospectrally deformed Dirac operator at time t.";
ConnesDistance::usage = "ConnesDistance[g] returns the Connes spectral distance matrix between vertices of complex g.";
DiracZetaFunction::usage = "DiracZetaFunction[g, s] gives the super-spectral zeta function str(|D|^{-s}) of the Dirac operator.";
ConnectionZetaFunction::usage = "ConnectionZetaFunction[g, s] gives the spectral zeta function of the connection matrix L.";
LefschetzZetaFunction::usage = "LefschetzZetaFunction[g, perm, z] gives the Lefschetz zeta function exp(sum L(T^n)/n z^n) for automorphism perm.";
AnalyticTorsion::usage = "AnalyticTorsion[g] gives the analytic torsion exp(1/2 sum (-1)^{k+1} k log det'(H_k)) of complex g.";
IndexExpectationCurvature::usage = "IndexExpectationCurvature[g] gives the index-expectation curvature K(x)=E[1-chi(S(x) cap {f<f(x)})], satisfying Gauss-Bonnet: sum K = chi.";

RadarCoordinates::usage = "RadarCoordinates[g, basis, v] gives the distance vector (d(v, b))_{b in basis} of vertex v; RadarCoordinates[g, basis] gives the association of all vertices' radar coordinates.";
ResolvingSetQ::usage = "ResolvingSetQ[g, basis] tests whether basis is a resolving set: the radar map v |-> (d(v, b))_{b in basis} is injective over the vertices.";
FindResolvingSet::usage = "FindResolvingSet[g, n, m] returns up to n resolving sets (metric bases) of g by ascending size; m restricts the sizes (All, an integer max, {min, max}, or {exact}).";
MetricDimension::usage = "MetricDimension[g] gives the metric dimension of g: the size of a smallest resolving set.";
ResistanceCoordinates::usage = "ResistanceCoordinates[g] gives the association vertex -> spectral embedding Phi with ||Phi(u)-Phi(v)||^2 == EffectiveResistance[g,u,v]; options \"Rescaling\" (\"ResistanceMatching\" | \"None\" | \"Diffusion\"->t), \"Dimension\", \"Origin\". ResistanceCoordinates[g, v] gives the coordinates of v.";
FindBallCover::usage = "FindBallCover[g, r] returns a minimum r-ball cover of g: a smallest set of centres whose radius-r balls cover every vertex (a minimum r-dominating set). FindBallCover[g, r, targets] covers only the given vertex subset (centres still chosen from all of g). FindBallCover[g, r, targets, count] returns up to count distinct minimum covers for an integer count or UpTo[count], or every one for All; count defaults to 1 (a single cover). Option Method (\"Exhaustive\" (default) exact integer program, \"Greedy\" repeatedly takes the centre covering the most uncovered targets -- fast but not minimum in general, even on vertex-transitive graphs, \"Symmetric\" smallest union of Aut(g) orbits that covers -- exact when a minimum cover is orbit-shaped, an upper bound otherwise).";
BallCoverQ::usage = "BallCoverQ[g, r, S] tests whether the radius-r balls around the centres S cover every vertex of g. BallCoverQ[g, r, S, targets] tests coverage of the given vertex subset.";
DominationNumber::usage = "DominationNumber[g, r] gives the r-domination number of g: the size of a minimum r-ball cover. DominationNumber[g, r, targets] gives the size of a minimum r-ball cover of the given vertex subset.";
(* ===== Tessellations / maps (moved from SyntheticInfrageometry) ===== *)

SierpinskiGraph::usage = "SierpinskiGraph[n] is the trivalent Sierpinski graph: the 3-simplex K_4 with corner-cutting (truncation) iterated n-1 times. 3-regular at every generation, 4*3^(n-1) vertices; n=2 is the truncated tetrahedron. Graph options are forwarded.";
BetheGraph::usage = "BetheGraph[n, z] is the finite Bethe lattice / Cayley tree of n shells and coordination number z (argument order matching CompleteKaryTree[n, k]): the root branches z ways and every other internal node z-1, so all interior vertices are z-valent. Distinct from the rooted, irregular CompleteKaryTree. Undirected by default; DirectedEdges -> True orients edges away from the root.";
BranchingSequenceTree::usage = "BranchingSequenceTree[b] is the spherically symmetric rooted tree whose offspring count depends only on depth: a vertex at depth l has b[[l+1]] children. Length[b]+1 levels, FoldList[Times, 1, b] vertices per shell. Constant b gives CompleteKaryTree. Graph options are forwarded.";

AmbientGraphStyle::usage = "AmbientGraphStyle[name] is the Graph option list rendering a substrate as a faint backdrop, one of \"Default\", \"GrayFaint\", \"GrayOpaque\" or \"Gray\"; splice it in with Graph[g, Sequence @@ AmbientGraphStyle[name]]. AmbientGraphStyle[] lists the names.";

InfraSubstrateStyle::usage = "InfraSubstrateStyle[g] applies the substrate backdrop styling to any graph, choosing the ambient gray by vertex count (<= 250 \"Gray\", <= 800 \"GrayOpaque\", denser \"GrayFaint\") -- the same rule InfraSubstrate uses for a raw spec; InfraSubstrateStyle[g, style] uses the named AmbientGraphStyle. InfraSubstrateStyle[state] takes a list of hyperedges (a Wolfram-model final state) and styles its graph, binary edges directly and higher arities via the 2-section. Graph options are forwarded.";
InfraSubstrate::usage = "InfraSubstrate[name, size] is the named example substrate at tier \"Small\", \"Medium\" or \"Large\", or at a raw spec (a cell measure, a radius, grid dimensions, a generation count); generation is seeded per (name, size), so repeated calls return the same graph. Each substrate is one explicit definition in Kernel/InfraSubstrate.wl, with its tier table and its exceptions (an interior strip, a kept embedding) written into the definition. A Wolfram-model universe is named by its Registry of Notable Universes number, as in InfraSubstrate[\"wm6655\", 11], and any of the 947 registry entries resolves through ResourceFunction[\"WolframModelData\"]. InfraSubstrate[name, size, style] overrides the ambient style, which defaults to the AmbientGraphStyle gray of that tier since a substrate is a backdrop; pass \"Default\" for none. A substrate is bare combinatorics by default -- a stored embedding is discarded and a spring layout of its own dimension places the vertices; option \"KeepCoordinates\" -> True draws the substrate where it lives instead. InfraSubstrate[] lists the roster classified by what a substrate models (\"OpenManifold\" -- boundaryless patches, open subsets delivered with the rim contour removed; \"ClosedManifold\" -- compact tessellated surfaces; \"Exotic\" -- graphs with no manifold model; \"WolframModel\"); InfraSubstrate[All] is the flat name list; InfraSubstrate[name] is the \"Medium\" tier. Graph options are forwarded.";

TorusTessellation::usage = "TorusTessellation[{m, n}, shape] is the vertex-transitive flat-torus graph carrying the regular tessellation indicated by shape, one of \"Square\" ({4,4}), \"Triangular\" ({3,6}, the default) or \"Hexagonal\" ({6,3}).";
InflateGraph::usage = "InflateGraph[g] grows a fiber of extra vertices over every vertex of g, joins each to its base vertex, and adds random edges between fibers whose base vertices lie within \"Radius\" in g. The base is recoverable as the induced subgraph on VertexList[g]. Options \"ExtraVertices\", \"ExtraEdges\", \"Radius\" and \"Density\", each a constant or a {min, max} range sampled per base vertex.";
InflatedVertex::usage = "InflatedVertex[v, i] is the i-th fiber vertex over base vertex v in a graph produced by InflateGraph.";
TessellationGraph::usage = "TessellationGraph[{p, q}] is the smallest regular map of type {p, q} as a graph -- the Platonic solid when (p-2)(q-2) < 4, the smallest hyperbolic quotient when > 4. TessellationGraph[config] is the uniform (Archimedean) map of vertex configuration config (the cyclic face sizes around a vertex, e.g. {3, 6, 3, 6}). TessellationGraph[spec, n] sizes the result (n x n flat torus in the Euclidean case, n-th PSL(2,ell) quotient in the hyperbolic case); TessellationGraph[{p, q}, {m, n}] gives the m x n flat torus; TessellationGraph[{p, q}, G] / [{p, q}, {r, s}] carries the coset graph of a finite group G or an explicit (2,p,q)-generation. Option Method (Automatic (default; fast realiser per curvature), \"Platonic\", \"Torus\", \"PSL2\", or \"CosetEnumeration\" / {\"CosetEnumeration\", \"MaxIndex\" -> n} for the general low-index method).";

TessellationCurvature::usage = "TessellationCurvature[{p, q}] (or TessellationCurvature[config]) is the combinatorial Gaussian curvature Sum 1/f_i - (k-2)/2 at a vertex of the {p, q} regular map or the uniform map of vertex configuration config; its sign is spherical (> 0), flat (== 0), or hyperbolic (< 0) and the geometric angle defect is 2 Pi times it. TessellationCurvature[graph] detects a regular configuration (uniform degree, girth face size) from the graph.";

TessellationEulerCharacteristic::usage = "TessellationEulerCharacteristic[graph, spec] is the Euler characteristic V - E + F of the realised tessellation graph, where spec is its {p, q} symbol or vertex configuration. TessellationEulerCharacteristic[graph] assumes a regular map and detects spec from the graph (pass spec explicitly for Archimedean / mixed-face maps).";

TessellationGenus::usage = "TessellationGenus[graph, spec] is the orientable genus (2 - chi)/2 of the realised tessellation graph, where spec is its {p, q} symbol or vertex configuration. TessellationGenus[graph] assumes a regular map and detects spec from the graph (pass spec explicitly for Archimedean / mixed-face maps).";

TessellationNeighborhoodGraph::usage = "TessellationNeighborhoodGraph[{p, q}, r] is the radius-r graph-distance ball cut from the infinite regular {p, q} tessellation of its covering surface (Euclidean plane, hyperbolic plane, or closing-up sphere by the curvature (p-2)(q-2)) -- the non-compact companion to TessellationGraph, with VertexCoordinates carrying the embedding. TessellationNeighborhoodGraph[config, r] cuts the same ball from the uniform / Archimedean tiling of a longer vertex configuration (the chiral snub / elongated families are deferred). TessellationNeighborhoodGraph[{p, q}, {m, n}] is the m x n rectangular Euclidean patch.";

CosetEnumeration::usage = "CosetEnumeration[p, q, subwords, maxc] is the Todd-Coxeter index [D(p,q,2) : H] of the subgroup H generated by subwords (lists over 1 = x, 2 = x^-1, 3 = y, 4 = y^-1) of the von Dyck group <x, y | x^p = y^q = (x y)^2 = 1>, or $Failed if it exceeds maxc; on the cyclic stabilizers <y>, <x y>, <x> it gives the vertex, edge, face counts.";
LowIndexMaps::usage = "LowIndexMaps[p, q, maxIndex] enumerates every genuine {p, q} map of index <= maxIndex up to isomorphism, by low-index subgroup enumeration of the von Dyck group, as associations with \"Index\", \"Generators\", \"Skeleton\", \"Regular\" (normal-subgroup test), and \"Genus\".";
RotationMapGraph::usage = "RotationMapGraph[{x, y}] is the 1-skeleton of the orientable map of the rotation pair {x, y}: vertices are the cycles of y, edges the 2-cycles of x y joining them.";
DisplacementCompose::usage = "DisplacementCompose[d1, d2, ...] composes displacements as flows, the leftmost acting first; targets are collected over every intermediate point.";
DisplacementScale::usage = "DisplacementScale[g, d, t] scales displacement d by t: endpoints of the geodesics v -> d(v) rescaled to t times their length (t < 0 reflects through the base point).";
DisplacementNegative::usage = "DisplacementNegative[g, d] is the metric negative of d: each step is reflected through its base point, the scale -1 case of DisplacementScale.";
DisplacementInverse::usage = "DisplacementInverse[d] reverses the relation d: its value at v is the set of vertices whose d-value contains v. It is the ordinary inverse when d is bijective and may have empty fibers otherwise.";
DisplacementSum::usage = "DisplacementSum[g, d1, d2] is the bisector of the two composition orders of d1 and d2 -- by Baker-Campbell-Hausdorff the sum of the generators, with the commutator term cancelled.";
DisplacementCommutator::usage = "DisplacementCommutator[g, d1, d2] applies the commutator loop selected by Method. Method -> \"Inverse\" (default) is the exact group commutator of bijective displacements; Method -> \"Negative\" composes d1, d2 and their metric negatives. DisplacementCommutator[g, d1, d2, v] evaluates it at v.";
DisplacementBracket::usage = "DisplacementBracket[g, d1, d2] is DisplacementCommutator[g, d1, d2, Method -> \"Negative\"], the scale-dependent metric bracket candidate. DisplacementBracket[g, d1, d2, v] evaluates it at v.";
DisplacementMagnitude::usage = "DisplacementMagnitude[g, d] is the maximal step length of displacement d -- its scale.";
DisplacementReduce::usage = "DisplacementReduce[g, d] contracts each value set of d to its metric centre (minimal eccentricity within the set), iterated to a fixed point; genuine ties survive.";
DisplacementSingleValuedQ::usage = "DisplacementSingleValuedQ[d] tests whether every value of displacement d is a single vertex.";
DisplacementBijectionQ::usage = "DisplacementBijectionQ[d] tests whether displacement d is a single-valued permutation of the vertex set.";
DisplacementIsomorphismQ::usage = "DisplacementIsomorphismQ[g, d] tests whether displacement d is a graph automorphism of g -- a discrete Killing displacement.";
ContinuousDisplacementQ::usage = "ContinuousDisplacementQ[g, d, k] tests k-continuity of displacement d (default k = 1). Method -> \"Weak\" requires one close target pair across each edge; \"Hausdorff\" requires every target to have a close partner; \"Strong\" requires every cross-pair to be close.";
RandomDisplacement::usage = "RandomDisplacement[g, r] generates a random continuous displacement of magnitude at most r (default 1): a random continuous section of the scale-r tangent bundle.";
FindKillingDisplacement::usage = "FindKillingDisplacement[g] finds the nontrivial graph automorphism of least displacement magnitude, as a displacement. FindKillingDisplacement[g, All] returns all minimal ones.";
KillingDisplacementMagnitude::usage = "KillingDisplacementMagnitude[g] is the least magnitude of a nonidentity graph automorphism, or Infinity when g is asymmetric.";
PolarDisplacements::usage = "PolarDisplacements[g, c] gives the polar pair {radial, angular} at centre c: radial steps along geodesics from c (outward, or inward with \"Direction\" -> \"Inward\"), angular steps along edges of equal distance.";
GradientDisplacement::usage = "GradientDisplacement[g, f] is the steepest-ascent displacement of the vertex function f (an association): each vertex moves to the neighbours maximising the increase of f; local maxima stay put.";
TranslationDisplacement::usage = "TranslationDisplacement[g, v] translates along the graph embedding: each vertex moves to the vertices whose coordinates are nearest to its own position plus the vector v.";
DisplacementPlot::usage = "DisplacementPlot[g, d] draws displacement d as bent arcs over the graph's own embedding. DisplacementPlot[g, {d1, d2, ...}] draws a sequence, the k-th in the k-th Standard colour.";

(* Differential forms and cochains. Cochains are stored sparsely on the increasing
   representative of each clique; two conventions share that format and are not
   interchangeable -- see the header of Kernel/DifferentialForms.wl. *)

FormValue::usage = "FormValue[w, v, tuple] gives the value of the germ of form w at vertex v on a tuple of neighbours of v, alternating in the tuple.";
CochainValue::usage = "CochainValue[a, tuple] gives the value of the ALTERNATING cochain a on an arbitrary vertex tuple, by the sign of the permutation taking it to increasing order; 0 off the complex. Do not apply it to the output of OrderedCochainCup or CochainCupOne, which are ordered cochains.";
OrderedCochainValue::usage = "OrderedCochainValue[a, tuple] gives the value of the ORDERED cochain a on an increasing vertex tuple, 0 off the complex, and Missing[\"NonIncreasingTuple\", tuple] otherwise. This is the correct accessor for the output of OrderedCochainCup and CochainCupOne.";
FormDegree::usage = "FormDegree[w] gives the degree of form w, read off a stored germ.";
CochainDegree::usage = "CochainDegree[a] gives the degree of cochain a: one less than the number of vertices of a stored cell.";
ZeroForm::usage = "ZeroForm[g, f] is the vertex function f (an association or a function) as a 0-form on g.";
RestrictionMap::usage = "RestrictionMap[g, a] is the form R a obtained from the alternating cochain a by reading it with the base vertex prepended; it vanishes off cliques.";
IntegrationMap::usage = "IntegrationMap[g, w] is the alternating cochain I w obtained by averaging the germs of w over the vertices of each clique with the orientation sign. I is a left inverse of RestrictionMap and a chain map.";
Coboundary::usage = "Coboundary[g, a] is the coboundary of cochain a: the alternating sum over the faces of every clique one dimension up. It agrees on the alternating and the ordered convention.";
FormDifferential::usage = "FormDifferential[g, w] is the differential of form w: the graph gradient on 0-forms, and on 1-forms the difference of germ values corrected by the transport term from the neighbouring germs.";
NaiveDifferential::usage = "NaiveDifferential[g, w] is the differential of the 1-form w with the transport term dropped; integrating it loses the factor (k+1)/(k+2).";
FormWedge::usage = "FormWedge[w, e] is the wedge product of forms, the exterior product on each tangent fiber. It is strictly associative and graded-commutative, but its differential fails Leibniz.";
CochainCup::usage = "CochainCup[g, a, b] is the cup product of ALTERNATING cochains: the full antisymmetrisation of the Alexander-Whitney formula over the (p+q+1)! orderings of each clique. It needs no vertex order, is unital, graded-commutative and a derivation for the coboundary, and is NOT associative; its 1/(p+q+1)! normalisation is the one agreeing with the cup product on cohomology. For the associative Alexander-Whitney formula use OrderedCochainCup.";
OrderedCochainCup::usage = "OrderedCochainCup[g, a, b] is the bare Alexander-Whitney cup product of ORDERED cochains, a(v0..vp) b(vp..v_{p+q}) on each increasing clique. Associative and unital but not graded-commutative, and well defined only in the ordered convention: the Alexander-Whitney cup of two alternating cochains is not alternating, so read the result with OrderedCochainValue. It is what the Steenrod tower and the A-infinity comparison need.";
CochainCupOne::usage = "CochainCupOne[g, a, b] is the Steenrod cup-1 product of ORDERED cochains. For closed a and b it is a primitive for the graded commutator: Coboundary[g, CochainCupOne[g, a, b]] equals OrderedCochainCup[g, a, b] - (-1)^(p q) OrderedCochainCup[g, b, a]. It vanishes when a has degree 0.";
AntisymmetrizedCup::usage = "AntisymmetrizedCup[g, a, b] is an alias of CochainCup, the name the antisymmetrised product carried before it became the cup product.";
