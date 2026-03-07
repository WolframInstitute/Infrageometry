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
GreenFunctionMatrix::usage = "GreenFunctionMatrix[g] returns Green-type matrix using star intersections.";
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
FormanRicciCurvature::usage = "FormanRicciCurvature[g] returns association of edges to Forman–Ricci curvature values.";
FormanRicciCurvatures::usage = "FormanRicciCurvatures[g] returns extended Forman–Ricci curvature components per edge (if implemented).";
SectionalCurvatures::usage = "SectionalCurvatures[g] estimates discrete sectional curvatures for edge neighborhoods (if implemented).";
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

(* Mesh.wl *)
ComplexEmbedding::usage = "ComplexEmbedding[g,d] numerically embeds complex g in R^d via energy minimization (d=2 or 3).";
ComplexMesh::usage = "ComplexMesh[g,d] builds a MeshRegion from complex g (with optional explicit coordinates).";
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
VietorisRipsComplex::usage = "VietorisRipsComplex[data, r, k] returns the Vietoris–Rips simplicial complex (clique complex of threshold graph) truncated to dimension k (Infinity for full).";
VietorisRipsThresholdGraph::usage = "VietorisRipsThresholdGraph[data, r, opts] returns the threshold graph joining points with distance <= r. Options: 'Metric'->f (default EuclideanDistance), 'IncludeLoops'->False (future use), 'VertexCoordinates'->True to attach coordinates.";

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
