Package["WolframInstitute`Infrageometry`"]

(* Usage messages for exported symbols. Experimental functions are marked (experimental). *)

ComplexClosure::usage = "ComplexClosure[g] returns the simplicial closure of a list of simplices g.";
CanonicalComplex::usage = "CanonicalComplex[g] relabels vertices of complex g to a canonical consecutive ordering.";

SimplexDimension::usage = "SimplexDimension[s] gives the dimension (#vertices - 1) of simplex s.";
ComplexDimension::usage = "ComplexDimension[g] gives the maximal simplex dimension of complex g (or -1 for empty).";
ComplexDimensions::usage = "ComplexDimensions[g] gives inductive vertex dimensions of complex g.";
ComplexInductiveDimension::usage = "ComplexInductiveDimension[g] gives the mean of vertex dimensions (inductive dimension).";
SimplexList::usage = "SimplexList[g, k] lists all simplices of dimension <= k (or within a range {kmin,kmax}).";
ComplexFacets::usage = "ComplexFacets[g] returns all maximal simplices (facets) of g.";
ComplexVertexList::usage = "ComplexVertexList[g] returns the sorted list of vertices in g.";
SimplexCardinality::usage = "SimplexCardinality[g, k] gives the number of k-dimensional simplices in g.";
SimplexCardinalities::usage = "SimplexCardinalities[g] gives counts of simplices in each dimension (f-vector without the leading 1).";
SimplexStar::usage = "SimplexStar[g, s] gives the star: all simplices containing s.";
SimplexCore::usage = "SimplexCore[g, s] gives all simplices contained in s.";
ComplexUnitSphere::usage = "ComplexUnitSphere[g, s] gives the unit sphere (link) around simplex s (its star minus interior).";
SimplexBoundary::usage = "SimplexBoundary[s] lists codimension-1 faces of simplex s.";
SimplexIndex::usage = "SimplexIndex[s] gives signed index weight of simplex s; SimplexIndex[s,t] gives relative index if s and t coincide as sets.";
SimplexSign::usage = "SimplexSign[s] is the permutation signature of vertex ordering of s; SimplexSign[s,t] relative signature if same underlying set.";
SimplexWeight::usage = "SimplexWeight[s] is (-1)^(Length[s]).";
ContractibleQ::usage = "ContractibleQ[g] attempts a recursive star decomposition test for contractibility of g.";
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

BettiTable::usage = "BettiTable[data, radii] returns an association with radii, rectangular Betti number matrix and dimensions labels derived from BettiCurves.";
BettiCurves::usage = "BettiCurves[data, radii] returns an association radius -> Betti vector for the Vietoris–Rips filtration.";
VietorisRipsFiltration::usage = "VietorisRipsFiltration[data, radii] builds an association radius -> Vietoris–Rips complex (up to MaxDimension).";
VietorisRipsComplex::usage = "VietorisRipsComplex[data, r, k] returns the k-truncated VR complex at scale r.";
VietorisRipsThresholdGraph::usage = "VietorisRipsThresholdGraph[data, r] returns the threshold graph connecting points within distance r (metric configurable).";

PersistentHomology::usage = "PersistentHomology[filtration] returns an association dim -> { {birth, death}, ... } of persistence intervals over Z2 (death=Infinity for essential).";
PersistenceIntervals::usage = "PersistenceIntervals is an internal helper returning the same structure as PersistentHomology.";
PersistenceDiagram::usage = "PersistenceDiagram[filtration] returns a list of {dim,birth,death} triples for intervals.";
PopularNetwork::usage = "PopularNetwork[name, what] returns a requested artifact: 'Graph' (default), 'Description', 'Source', or 'All' (association with all fields).";
PopularNetworkNames::usage = "PopularNetworkNames[] lists available names for PopularNetwork.";
