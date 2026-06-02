Package["WolframInstitute`Infrageometry`"]

(* Usage messages for exported symbols. Experimental functions are marked (experimental). *)

ComplexClosure::usage = "ComplexClosure[g] returns the simplicial closure of a list of simplices g.";
IndexHypergraph::usage = "IndexHypergraph[h] relabels vertices of a hypergraph (list of hyperedges) h to consecutive integers.";
IndexComplex::usage = "IndexComplex[g] relabels vertices of complex g to a consecutive ordering of integers.";

SimplexDimension::usage = "SimplexDimension[s] gives the dimension (#vertices - 1) of simplex s.";
ComplexDimension::usage = "ComplexDimension[g] gives the maximal simplex dimension of complex g (or -1 for empty).";
ComplexDimensions::usage = "ComplexDimensions[g] gives inductive vertex dimensions of complex g.";
ComplexInductiveDimension::usage = "ComplexInductiveDimension[g] gives the mean of vertex dimensions (inductive dimension).";
SimplexList::usage = "SimplexList[g, k] lists all simplices of dimension <= k (or within a range {kmin,kmax}).";
ComplexBones::usage = "ComplexBones[g] returns interior (d-2)-faces (\"bones\") of a pure d-dimensional complex.";
ComplexWalls::usage = "ComplexWalls[g] returns codimension-1 faces (\"walls\") of a pure complex (potential boundary facets).";
ComplexFacets::usage = "ComplexFacets[g] returns all maximal simplices (facets) of g.";
ComplexVertexList::usage = "ComplexVertexList[g] returns the sorted list of vertices in g.";
SimplexCardinality::usage = "SimplexCardinality[g, k] gives the number of k-dimensional simplices in g.";
SimplexCardinalities::usage = "SimplexCardinalities[g] gives counts of simplices in each dimension (f-vector without the leading 1).";
SimplexStar::usage = "SimplexStar[g, s] gives the star: all simplices containing s.";
SimplexCore::usage = "SimplexCore[g, s] gives all simplices contained in s.";
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
GraphBoundary::usage = "GraphBoundary[g, S] gives the inner vertex boundary of vertex subset S in g: the vertices of S adjacent to some vertex outside S. S may be a vertex list or a subgraph.";
GraphInterior::usage = "GraphInterior[g, S] gives the interior of vertex subset S in g: the vertices of S all of whose neighbors lie in S (= S minus GraphBoundary[g, S]). S may be a vertex list or a subgraph.";
FormanRicciCurvature::usage = "FormanRicciCurvature[g] returns Association[edge -> kappa] with Forman's combinatorial Ricci curvature on the clique complex of g. Options: \"OnCells\" (Integer | All | {dims}; output cell dimension(s), default 1) and \"MaxCellDimension\" (Automatic (default) | Integer; clique-complex truncation, Automatic = Max[OnCells] + 1).";
OllivierRicciCurvature::usage = "OllivierRicciCurvature[g] returns Association[edge -> kappa] with the Ollivier-Ricci curvature kappa(u, v) = 1 - W_1(mu_u, mu_v) / d(u, v), where mu_x is uniform on N(x) and W_1 is the Wasserstein-1 distance under graph distance (alpha = 0).";
WolframRicciCurvature::usage = "WolframRicciCurvature[g] returns Association[v -> mean_r R(v, r)] with the volume-comparison Ricci scalar averaged over the per-vertex valid radius range. WolframRicciCurvature[g, r] gives a single radius; WolframRicciCurvature[g, {rmin, rmax}] gives a window. Option: \"Dimension\" (Automatic (default) | Integer).";
WolframHausdorffDimension::usage = "WolframHausdorffDimension[g] returns Association[v -> mean_r d(v, r)] with the volume-growth local dimension d(v, r) = (Log V(r+1) - Log V(r)) / (Log(r+1) - Log r), V(r) = |B_r(v)|, averaged over the per-vertex valid radius range. WolframHausdorffDimension[g, r] gives a single radius; WolframHausdorffDimension[g, {rmin, rmax}] gives a window.";
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
GraphMesh::usage = "GraphMesh[g] builds a MeshRegion from graph g using its embedding + cliques up to size 4.";
InteriorMeshGraph::usage = "InteriorMeshGraph[mr] returns the 1-skeleton of mr with every edge between two boundary (surface) vertices deleted, keeping edges incident to an interior vertex.";
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
UnitLengthGraph::usage = "UnitLengthGraph[region, n] returns the contact graph of an n-sphere hard-sphere packing relaxed in region (filling a solid, meshing a surface); every edge has length 2r and the packing is stored as VertexCoordinates. Option Method (\"IterativeProjection\" (default), \"ConstrainedPacking\"); \"Radius\" (Automatic spaces the spheres to tile the region's content).";
UnitLengthEmbedding::usage = "UnitLengthEmbedding[graph] embeds graph in R^d (option \"Dimension\") so every edge is a unit segment, returning coordinates in VertexList order (cf. GraphEmbedding); the iterative counterpart of ComplexEmbedding.";

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
FindBallCover::usage = "FindBallCover[g, r] returns a minimum r-ball cover of g: a smallest set of centres whose radius-r balls cover every vertex (a minimum r-dominating set).";
BallCoverQ::usage = "BallCoverQ[g, r, S] tests whether the radius-r balls around the centres S cover every vertex of g.";
DominationNumber::usage = "DominationNumber[g, r] gives the r-domination number of g: the size of a minimum r-ball cover.";
