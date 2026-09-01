Package["WolframInstitute`Infrageometry`"]

PackageExport[GraphBoundary]
PackageExport[GraphInterior]
PackageExport[GraphExteriorBoundary]
PackageExport[BoundarylessGraph]
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

(* BoundarylessGraph[x]: delete the exterior boundary and return the largest
   connected component of the subgraph induced on the interior, carrying the
   original coordinates over -- the graph made to look boundaryless to the
   observer.  One deletion rule for both forms; only the boundary detector
   differs (exact surface for a MeshRegion, degree heuristic for a Graph). *)

Options[BoundarylessGraph] = Join[{Method -> "AverageDegree", "KeepCoordinates" -> True}, Options[Graph]];

BoundarylessGraph[g_Graph, opts : OptionsPattern[]] :=
	With[
		{coords = AssociationThread[VertexList[g], GraphEmbedding[g]]},
		{h = interiorComponent[g, VertexList[g], GraphExteriorBoundary[g, Method -> OptionValue[Method]]]},
		If[Length @ First @ coords === 3, Graph3D, Graph][h,
			Sequence @@ FilterRules[{opts}, Options[Graph]],
			Sequence @@ If[TrueQ @ OptionValue["KeepCoordinates"],
				{VertexCoordinates -> Lookup[coords, VertexList[h]]}, {}]]
	]

BoundarylessGraph[mr_MeshRegion, opts : OptionsPattern[]] :=
	With[
		{coords = MeshCoordinates[mr], edges = UndirectedEdge @@@ (First /@ MeshCells[mr, 1])},
		{h = interiorComponent[Graph[Range @ Length @ coords, edges], Range @ Length @ coords, GraphExteriorBoundary[mr]]},
		Graph[h,
			Sequence @@ FilterRules[{opts}, Options[Graph]],
			Sequence @@ If[TrueQ @ OptionValue["KeepCoordinates"],
				{VertexCoordinates -> coords[[VertexList[h]]]}, {}]]
	]

interiorComponent[g_, vertices_, boundary_] :=
	With[{h = Subgraph[g, Complement[vertices, boundary]]},
		If[VertexCount[h] == 0, h, First @ MaximalBy[ConnectedGraphComponents[h], VertexCount]]
	]
