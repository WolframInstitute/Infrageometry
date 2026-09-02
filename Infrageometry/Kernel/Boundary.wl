Package["WolframInstitute`Infrageometry`"]

PackageExport[GraphBoundary]
PackageExport[GraphInterior]
PackageExport[GraphExteriorBoundary]
PackageExport[BoundarylessGraph]
PackageExport[GraphEccentricities]
PackageExport[EccentricitySubgraph]
PackageExport[RelativeEccentricity]
PackageExport[RelativeEccentricitySubgraph]
PackageScope[meshSurfaceVertices]


(* ===================== Inner vertex boundary / interior ===================== *)

(* GraphBoundary[g, S]: inner vertex boundary. Two input forms, one notion (a
   vertex is boundary iff some g-edge at it escapes the given object):
     - vertex list S: the object is the INDUCED subgraph, so "escaping g-edge"
       means a neighbor outside S -- {v in S : v has a neighbor in V\S};
     - subgraph h (its own edges, e.g. a path/curve): {v in V(h) : v has a
       g-neighbor it is not joined to in h}. For an induced h this agrees with
       the list form; for a sparser h a curve is all boundary (every vertex has
       g-neighbors off the curve), so a 1-D object has empty interior. *)
GraphBoundary[g_Graph, h_Graph] :=
	With[{hAdj = AssociationThread[VertexList[h], AdjacencyList[h]]},
		Select[VertexList[h], ! SubsetQ[Lookup[hAdj, #, {}], AdjacencyList[g, #]] &]
	]

GraphBoundary[g_Graph, subset_List] :=
	With[{outside = Complement[VertexList[g], subset]},
		Select[subset, IntersectingQ[AdjacencyList[g, #], outside] &]
	]

(* GraphInterior[g, S]: S \ GraphBoundary[g, S], same two input forms. *)
GraphInterior[g_Graph, h_Graph] :=
	Complement[VertexList[h], GraphBoundary[g, h]]

GraphInterior[g_Graph, subset_List] :=
	Complement[subset, GraphBoundary[g, subset]]


(* ===================== Exterior boundary ===================== *)

(* GraphExteriorBoundary[x]: the rim of the whole object -- exact for a MeshRegion
   (the vertices lying on a boundary facet), heuristic for a bare Graph (the
   degree-deficient vertices).  Method -> "AverageDegree" (default, for meshes:
   below-average degree), "MaxDegree" (for lattices: less than full degree).
   Complements GraphBoundary, the inner boundary of a SUBSET: as its own subset
   the whole graph has empty inner boundary, so the rim must be detected from
   the object (mesh facets) or guessed from degrees. *)

Options[GraphExteriorBoundary] = {Method -> "AverageDegree"};

GraphExteriorBoundary[g_Graph, OptionsPattern[]] :=
	With[
		{deg = AssociationThread[VertexList[g], VertexDegree[g]]},
		{threshold = Switch[OptionValue[Method] /. Automatic -> "AverageDegree",
			"AverageDegree", Mean[N @ Values @ deg],
			"MaxDegree", Max @ Values @ deg]},
		Select[VertexList[g], deg[#] < threshold &]
	]

GraphExteriorBoundary[mr_MeshRegion, OptionsPattern[]] := meshSurfaceVertices[mr]


(* surface vertices lie on a boundary face: a (d-1)-subset of a top simplex occurring in exactly one top cell *)
meshSurfaceVertices[mr_MeshRegion] := With[
	{d = RegionDimension[mr]},
	If[d <= 1,
		{},
		Union @@ Keys @ Select[Counts[Sort /@ Catenate[Subsets[First[#], {d}] & /@ MeshCells[mr, d]]], # == 1 &]
	]
]


(* ===================== Boundaryless graph ===================== *)

(* BoundarylessGraph[x]: delete every edge joining two exterior-boundary
   vertices -- the rim contour -- then drop only the vertices this isolates.
   No other vertex is removed: a boundary vertex with an inward edge survives
   as a whisker, so the result models an OPEN window onto the geometry, with
   no visible boundary contour.  One rule for both forms; only the boundary
   detector differs (exact surface for a MeshRegion, degree heuristic for a
   Graph). *)

Options[BoundarylessGraph] = Join[{Method -> "AverageDegree", "KeepCoordinates" -> True}, Options[Graph]];

BoundarylessGraph[g_Graph, opts : OptionsPattern[]] :=
	With[
		{coords = AssociationThread[VertexList[g], GraphEmbedding[g]]},
		{h = rimEdgeTrim[g, GraphExteriorBoundary[g, Method -> OptionValue[Method]]]},
		If[Length @ First @ coords === 3, Graph3D, Graph][h,
			Sequence @@ FilterRules[{opts}, Options[Graph]],
			Sequence @@ If[TrueQ @ OptionValue["KeepCoordinates"],
				{VertexCoordinates -> Lookup[coords, VertexList[h]]}, {}]]
	]

BoundarylessGraph[mr_MeshRegion, opts : OptionsPattern[]] :=
	With[
		{coords = MeshCoordinates[mr], edges = UndirectedEdge @@@ (First /@ MeshCells[mr, 1])},
		{h = rimEdgeTrim[Graph[Range @ Length @ coords, edges], GraphExteriorBoundary[mr]]},
		Graph[h,
			Sequence @@ FilterRules[{opts}, Options[Graph]],
			Sequence @@ If[TrueQ @ OptionValue["KeepCoordinates"],
				{VertexCoordinates -> coords[[VertexList[h]]]}, {}]]
	]

rimEdgeTrim[g_, boundary_] :=
	With[
		{bQ = Association @ Thread[boundary -> True]},
		{h = EdgeDelete[g, Select[EdgeList[g], TrueQ[bQ[#[[1]]]] && TrueQ[bQ[#[[2]]]] &]]},
		VertexDelete[h, Pick[VertexList[h], VertexDegree[h], 0]]
	]


(* ===================== Relative eccentricity ===================== *)

(* RelativeEccentricity[x]: t(v) = (e(v) - radius) / (diameter - radius), the scale-free
   depth coordinate of a finite metric space -- 0 exactly on the center, 1 exactly on the
   periphery. Takes a graph or a distance matrix, and returns one number per point in
   VertexList / row order. Degenerate when diameter == radius (a vertex-transitive graph)
   or the space is disconnected: there every point is at once center and periphery, and t
   is identically 0. *)

(* GraphEccentricities[x]: e(v) = max_w d(v, w), one per point in VertexList / row order --
   the list form VertexEccentricity lacks (it takes one vertex at a time). Absolute, in
   hops: the honest quantity, bounded by GraphRadius and GraphDiameter. *)

GraphEccentricities[g_Graph] := GraphEccentricities @ GraphDistanceMatrix[g]

GraphEccentricities[distMatrix_List] := Max /@ distMatrix


RelativeEccentricity[x : (_Graph | _List)] :=
	With[
		{ecc = GraphEccentricities[x]},
		{r = Min[ecc], d = Max[ecc]},
		If[! NumericQ[d] || d == r, ConstantArray[0, Length[ecc]], (ecc - r) / (d - r)]
	]


(* RelativeEccentricitySubgraph[g, band]: the induced subgraph on {v : lo <= t(v) <= hi},
   the substrate cut to a relative depth. A bare q is the band {0, q}. Subgraph keeps the
   vertex labels and their coordinates, so a construction carried out on the band is
   simultaneously an object of g and draws on it unchanged. The rim-stripping cousin
   BoundarylessGraph cuts absolutely and once; this cuts relatively and by a dial. A band
   that disconnects is returned disconnected. *)

RelativeEccentricitySubgraph[g_Graph, band : (_?NumericQ | {_?NumericQ, _?NumericQ}) : 1] :=
	With[
		{range = Replace[band, q_?NumericQ :> {0, q}]},
		Subgraph[g, Pick[VertexList[g], range[[1]] <= # <= range[[2]] & /@ RelativeEccentricity[g]]]
	]


(* EccentricitySubgraph[g, band]: the same cut on the raw eccentricity, in hops -- the
   induced subgraph on {v : lo <= e(v) <= hi}, a bare k meaning e(v) <= k. Prefer this to
   the relative form when you are reading one graph: e is what the observer counts, and
   between GraphRadius[g] and GraphDiameter[g] it has only D - r + 1 values anyway, so the
   rescaled dial is that same short ladder wearing a fraction. The relative form earns its
   place only when one number has to mean the same depth on substrates of different size. *)

EccentricitySubgraph[g_Graph, band : (_?NumericQ | {_?NumericQ, _?NumericQ}) : Infinity] :=
	With[
		{range = Replace[band, k_?NumericQ :> {0, k}]},
		Subgraph[g, Pick[VertexList[g], range[[1]] <= # <= range[[2]] & /@ GraphEccentricities[g]]]
	]
