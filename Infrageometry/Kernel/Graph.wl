Package["WolframInstitute`Infrageometry`"]


PackageExport[SymmetricRelationGraph]
PackageExport[GraphEdgeWeights]
PackageExport[GraphVertexWeights]
PackageExport[GraphBoundary]
PackageExport[GraphInterior]
PackageExport[InteriorGraph]
PackageScope[withGraphOptions]
PackageExport[BallHull]
PackageExport[BallVolumes]
PackageExport[ShellAreas]
PackageExport[CylinderVolumes]
PackageExport[TubeVolumes]
PackageScope[cylinderVolume]
PackageExport[GeodesicIntervalGraph]
PackageExport[GeodesicOccupation]
PackageExport[GeodesicEdgeOccupation]

PackageExport[FormanRicciCurvature]
PackageExport[OllivierRicciCurvature]
PackageExport[LogDifferenceQuotients]
PackageExport[VolumeGrowthObservables]
PackageExport[DimensionCurvatureFit]
PackageExport[SectionalCurvatures]
PackageScope[wasserstein1]
PackageScope[windowSaturate]

PackageExport[EffectiveResistance]
PackageExport[ResistanceQ]

PackageExport[SpacetimeGraph]
PackageExport[SpacetimeTorusGraph]
PackageExport[RotateEdge]
PackageExport[KickEdge]
PackageExport[PersistEdge]
PackageExport[RightMatrix]
PackageExport[ToroidalRightMatrix]
PackageExport[ToroidalLeftMatrix]
PackageExport[DiracWalk]
PackageExport[VertexAmplitudes]
PackageExport[GraphSuspension]
PackageExport[RandomGraphAutomorphism]



SymmetricRelationGraph[f_, v_Association, opts___] :=
	Graph[Keys[v], UndirectedEdge @@@ Pick[Subsets[Keys[v], {2}], f @@@ Subsets[Values[v], {2}]], opts]


GraphEdgeWeights[g_Graph] := Replace[
	Replace[
		AnnotationValue[g, EdgeWeight],
		Automatic -> ConstantArray[1, EdgeCount[g]]
	],
	Automatic -> 1,
	1
]

GraphVertexWeights[g_Graph] := Replace[
	Replace[
		AnnotationValue[g, VertexWeight],
		Automatic -> ConstantArray[1, VertexCount[g]]
	],
	Automatic -> 1,
	1
]

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

(* InteriorGraph[g]: strip the degree-boundary, keeping the largest connected
   component and carrying the original vertex coordinates over.  The geometric
   "remove the boundary layer" companion to the combinatorial GraphInterior.
   Method -> "AverageDegree" (default, for meshes): delete every edge both of
   whose endpoints have below-average degree (the thin rim).  Method ->
   "MaxDegree" (for lattices -- grids, hexagonal / triangular tilings, cubes):
   the induced subgraph on the vertices of full degree, since a lattice's
   boundary is exactly its degree-deficient rim. *)
Options[InteriorGraph] = Join[{Method -> "AverageDegree"}, Options[Graph]];

InteriorGraph[g_Graph, opts : OptionsPattern[]] :=
	With[
		{deg = AssociationThread[VertexList[g], VertexDegree[g]],
		 coords = AssociationThread[VertexList[g], GraphEmbedding[g]]},
		{h = First @ MaximalBy[
			Switch[OptionValue[Method] /. Automatic -> "AverageDegree",
				"AverageDegree",
					With[{avg = Mean[N @ Values @ deg]},
						ConnectedGraphComponents @ EdgeDelete[g,
							Select[EdgeList[g], deg[#[[1]]] < avg && deg[#[[2]]] < avg &]]],
				"MaxDegree",
					ConnectedGraphComponents @ Subgraph[g,
						Select[VertexList[g], deg[#] == Max[Values @ deg] &]]],
			VertexCount]},
		If[Length @ First @ coords === 3, Graph3D, Graph][h,
			Sequence @@ FilterRules[{opts}, Options[Graph]],
			VertexCoordinates -> Lookup[coords, VertexList[h]]]
	]

(* apply forwarded Graph options to a constructed graph, passing a non-graph result
   (e.g. $Failed from a deferred constructor) straight through *)
withGraphOptions[g_, opts___] := If[GraphQ[g], Graph[g, Sequence @@ FilterRules[{opts}, Options[Graph]]], g]

(* ===================== Ball hull ===================== *)

(* BallHull[g, S]: intersection of all closed metric balls containing S, the
   smallest ball-convex (Mazur) superset of S.  For each center c the smallest
   enclosing radius is r_c = max_{s in S} d(c, s); v lies in the hull iff
   d(c, v) <= r_c for every c.  Read straight off the distance matrix. *)

BallHull[g_Graph, subgraph_Graph] :=
	BallHull[g, VertexList[subgraph]]

BallHull[g_Graph, S_List] :=
	With[{dist = GraphDistanceMatrix[g], idx = VertexIndex[g, #] & /@ S},
		With[{radii = Max /@ dist[[All, idx]]},
			Pick[VertexList[g], AllTrue[NonNegative] /@ Transpose[radii - dist], True]
		]
	]

(* ===================== Forman-Ricci curvature ===================== *)

(* Forman's combinatorial Ricci curvature on the clique complex of a graph.
   For a p-cell alpha (a (p+1)-clique encoded as a sorted vertex list):

       F(alpha) = w(alpha) [   Sum_{beta > alpha} w(alpha)/w(beta)
                             + Sum_{gamma < alpha} w(gamma)/w(alpha)
                             - Sum_{aHat || alpha}
                                 | Sum_{beta > alpha, beta > aHat} Sqrt[w(alpha) w(aHat)] / w(beta)
                                 - Sum_{gamma < alpha, gamma < aHat} Sqrt[w(alpha) w(aHat)] / w(gamma) | ]

   where beta > alpha denotes a (p+1)-coface, gamma < alpha a (p-1)-subface,
   and aHat || alpha (parallel) means: aHat is a distinct p-cell sharing
   either a coface or a subface with alpha but not both (Forman's strong
   parallelism).  Higher-cell weights default to 1.

   Options:
     "OnCells" -> p_Integer | All | {p1, p2, ...}      output cell dimension(s)
     "MaxCellDimension" -> Automatic | k_Integer       clique-complex truncation;
                                                       Automatic = Max[OnCells] + 1
                                                       (or full clique complex when All).

   Output keys:
     p = 0  ->  vertices
     p = 1  ->  UndirectedEdge[u, v] (in EdgeList[g] order, for backward
                compatibility with edge-keyed consumers)
     p >= 2 ->  sorted vertex lists {v1, ..., v_{p+1}}
   Single-integer "OnCells"     ->  flat Association[cell -> kappa].
   List or All "OnCells"        ->  Association[p -> Association[cell -> kappa]]. *)

Options[FormanRicciCurvature] = {"OnCells" -> 1, "MaxCellDimension" -> Automatic};

FormanRicciCurvature[g_Graph, OptionsPattern[]] := Module[{
	onCells, dMax, complex, byDim, vw, ew, weightAt, formanAt,
	outputDims, dimResult, upMap
},
	onCells = OptionValue["OnCells"];
	dMax = Replace[OptionValue["MaxCellDimension"],
		Automatic :> If[onCells === All,
			ComplexDimension @ GraphComplex[g],
			Max[Flatten[{onCells}]] + 1
		]
	];
	outputDims = Switch[onCells,
		All,      Range[0, dMax],
		_List,    onCells,
		_Integer, {onCells}
	];
	complex = GraphComplex[g, dMax + 1];
	byDim = AssociationMap[SimplexList[complex, {#}] &, Range[0, dMax]];

	(* upMap[q][gamma] = list of (q+1)-cells containing gamma; one pass over (q+1)-cells.
	   Replaces O(|cells_p|^2) Subset-scans inside formanAt. *)
	upMap = AssociationMap[
		q |-> Module[{acc = <||>},
			Do[
				(acc[#] = If[KeyExistsQ[acc, #], Append[acc[#], beta], {beta}]) & /@
					Subsets[beta, {q + 1}],
				{beta, byDim[q + 1]}
			];
			acc
		],
		Range[0, dMax - 1]
	];

	vw = AssociationThread[VertexList[g] -> GraphVertexWeights[g]];
	ew = AssociationThread[Sort /@ List @@@ EdgeList[g] -> GraphEdgeWeights[g]];

	weightAt[cell_List] := Which[
		Length[cell] == 1, vw[First[cell]],
		Length[cell] == 2, ew[Sort @ cell],
		True,              1
	];

	formanAt[alpha_List] := With[{p = Length[alpha] - 1, wA = weightAt[alpha]},
		With[{
			cofaces = If[p + 1 <= dMax, Lookup[upMap[p], Key @ alpha, {}], {}],
			subfaces = If[p >= 1, SimplexBoundary[alpha], {}]
		},
			With[{
				cofShared = If[cofaces === {}, {},
					DeleteCases[Union @@ Map[SimplexBoundary, cofaces], alpha]
				],
				subfShared = If[subfaces === {}, {},
					DeleteCases[
						Union @@ Map[gamma |-> Lookup[upMap[p - 1], Key @ gamma, {}], subfaces],
						alpha
					]
				]
			},
				With[{
					parallels = Complement[
						Union[cofShared, subfShared],
						Intersection[cofShared, subfShared]
					]
				},
					wA (
						Total[(beta |-> wA / weightAt[beta]) /@ cofaces]
						+ Total[(gamma |-> weightAt[gamma] / wA) /@ subfaces]
						- Total[
							(aHat |-> With[{sqrtW = Sqrt[wA * weightAt[aHat]]},
								Abs[
									Total[(beta |-> sqrtW / weightAt[beta]) /@
										Select[cofaces, SubsetQ[#, aHat] &]]
									- Total[(gamma |-> sqrtW / weightAt[gamma]) /@
										Intersection[subfaces, SimplexBoundary[aHat]]]
								]
							]) /@ parallels
						]
					)
				]
			]
		]
	];

	dimResult[p_] := Switch[p,
		0, AssociationThread[Catenate @ byDim[0] -> formanAt /@ byDim[0]],
		1, With[{kappaBySorted = AssociationThread[byDim[1] -> formanAt /@ byDim[1]]},
			AssociationMap[edge |-> kappaBySorted[Sort[List @@ edge]], EdgeList[g]]
		],
		_, AssociationThread[byDim[p] -> formanAt /@ byDim[p]]
	];

	If[IntegerQ[onCells],
		dimResult[onCells],
		AssociationMap[dimResult, outputDims]
	]
]


(* ===================== Ollivier-Ricci curvature ===================== *)

(* kappa(u, v) = 1 - W_1(mu_u, mu_v) / d(u, v),
   mu_x = uniform on the open neighborhood N(x); idleness alpha = 0;
   W_1 is the Wasserstein-1 (Earth-Mover) distance under graph distance,
   solved as a transport LP via LinearOptimization. *)

OllivierRicciCurvature[g_Graph] := Module[{vs, idx, adj, dist},
	vs   = VertexList[g];
	idx  = AssociationThread[vs, Range @ Length @ vs];
	adj  = AssociationMap[AdjacencyList[g, #] &, vs];
	dist = GraphDistanceMatrix[g];
	AssociationMap[
		e |-> With[{nu = adj[e[[1]]], nv = adj[e[[2]]]},
			1 - wasserstein1[
				ConstantArray[1.0 / Length[nu], Length[nu]],
				ConstantArray[1.0 / Length[nv], Length[nv]],
				dist[[idx /@ nu, idx /@ nv]]
			] / dist[[idx[e[[1]]], idx[e[[2]]]]]
		],
		EdgeList[g]
	]
]


(* Wasserstein-1 distance between probability vectors mu, nu on finite
   point sets given the m-by-n cost matrix.  Solved as a transport LP. *)

wasserstein1[mu_List, nu_List, costs_List] := Module[{m = Length[mu], n = Length[nu], vars},
	vars = Array[t, {m, n}];
	LinearOptimization[
		Total[Flatten[costs * vars]],
		Join[
			Table[Total[vars[[i, All]]] == mu[[i]], {i, m}],
			Table[Total[vars[[All, j]]] == nu[[j]], {j, n}],
			Thread[Flatten[vars] >= 0]
		],
		Flatten[vars],
		"PrimalMinimumValue"
	]
]


(* ===================== Ball volumes ===================== *)

(* a radial invariant restricted to a radius range: All -> the full (ragged) sequence
   {f(0), ..., f(ecc)}; r_Integer -> the scalar f(r); {rmin, rmax} -> a RECTANGULAR
   window, saturating past the sequence end with pad (so a fixed window over a vertex
   list Transposes cleanly for subset statistics).  pad defaults to the last value
   (ball volumes saturate at the component size); ShellAreas passes pad = 0. *)
windowSaturate[c_, range_] := windowSaturate[c, range, Last[c]]

windowSaturate[c_, range_, pad_] := Switch[range,
	All,                  c,
	_Integer,             If[0 <= range < Length[c], c[[range + 1]], pad],
	{_Integer, _Integer}, Table[If[0 <= r < Length[c], c[[r + 1]], pad], {r, range[[1]], range[[2]]}]
]

(* bouncing advancing front from a source set src: the foliation {S_0, ..., S_steps}
   of a momentum-carrying wavefront.  Each vertex of S_i steps one shell OUTWARD from
   the previous front S_{i-1} (to neighbours v with d(S_{i-1}, v) == d(S_{i-1}, u) + 1)
   and reflects inward where there is none, so the front turns around at a boundary or
   focus and propagates past the eccentricity -- unlike the metric sphere, which dies
   there.  State is the pair (S_{i-1}, S_i): the discrete wave-equation form, momentum
   carried as the trailing front.  (SyntheticInfrageometry`FindAdvancingInfraFront is
   the InfraSet-wrapped view of this same recurrence.) *)
advancingFront[g_Graph, src_List, steps_Integer] :=
	With[{adj = AssociationMap[AdjacencyList[g, #] &, VertexList[g]], dm = GraphDistanceMatrix[g], vl = VertexList[g]},
		{vidx = AssociationThread[vl, Range[Length[vl]]]},
		NestList[
			pair |-> With[{prev = pair[[1]], cur = pair[[2]]},
				{dp = AssociationThread[vl, Min /@ Transpose[dm[[Lookup[vidx, prev]]]]]},
				{cur, DeleteDuplicates @ Catenate[
					(u |-> With[{out = Select[adj[u], dp[#] == dp[u] + 1 &], in = Select[adj[u], dp[#] == dp[u] - 1 &]},
						Which[out =!= {}, out, in =!= {}, in, True, {u}]]) /@ cur]}],
			{src, src}, steps][[All, 2]]
	]

(* horizon for the "Front" ball-volume measure: a step count, not a radius.  All has no
   natural end (the front never stops), so default to 2 ecc(v) -- one out-and-back. *)
frontHorizon[g_, v_, range_] := Switch[range,
	All,                  2 Max[DeleteCases[GraphDistance[g, v], Infinity]],
	_Integer,             range,
	{_Integer, _Integer}, Last[range]
]

(* V(t) = sum_{s<=t} |S_s(v)| -- the cumulative passage count of the advancing front,
   the "ball volume" the front sweeps through time t (grows past eccentricity). *)
frontVolumeProfile[g_, v_, range_] := Accumulate[Length /@ advancingFront[g, {v}, frontHorizon[g, v, range]]]

(* V(r) = |B_r(v)|, the cumulative vertex count within radius r, as the List
   {V(0), ..., V(ecc(v))} (position i is radius i - 1).  Object slot 2 (single vertex,
   list, or All), radius slot 3 (default All -> full profile; r_Integer -> the scalar
   V(r); {rmin, rmax} -> the rectangular window).  Option "Measure" picks the counting
   convention: "Counting" = |B_r| (default); "Hausdorff" = |B_r| - |dB_r| =
   |GraphInterior[B_r]| (boundary-corrected, == the counting measure in the bulk).
   BallVolumes (default "Counting") == Accumulate[ShellAreas].  The growth invariant
   feeding VolumeGrowthObservables. *)

Options[BallVolumes] = {"Measure" -> "Counting"};

BallVolumes[g_Graph, opts : OptionsPattern[]] := BallVolumes[g, All, All, opts]

BallVolumes[g_Graph, pts : (All | _List | Except[_Rule | _RuleDelayed]), opts : OptionsPattern[]] :=
	BallVolumes[g, pts, All, opts]

(* the all/list form reads every vertex's distances off one GraphDistanceMatrix:
   one optimized all-pairs call is ~200x faster than V separate GraphDistance BFS
   calls (each carries a fixed graph-to-internal-rep overhead paid V times) *)
BallVolumes[g_Graph,
	vertices : (_List | All),
	range : (_Integer | {_Integer, _Integer} | All),
	OptionsPattern[]
] /; vertices === All || ! MemberQ[VertexList[g], vertices] :=
	With[
		{conv = OptionValue["Measure"], dm = GraphDistanceMatrix[g], vs = VertexList[g]},
		{idx = PositionIndex[vs], targets = If[vertices === All, vs, vertices]},
		(windowSaturate[#, range] &) /@ Switch[conv,
			"Counting",  (Accumulate @ Values @ KeySort @ Counts @ DeleteCases[dm[[idx[#][[1]]]], Infinity] &) /@ targets,
			"Hausdorff", (With[{row = dm[[idx[#][[1]]]]}, Table[Length @ GraphInterior[g, Pick[vs, Thread[row <= r]]], {r, 0, Max @ DeleteCases[row, Infinity]}]] &) /@ targets,
			"Front",     (frontVolumeProfile[g, #, range] &) /@ targets
		]
	]

BallVolumes[g_Graph,
	vertex : Except[All | _Rule | _RuleDelayed],
	range : (_Integer | {_Integer, _Integer} | All),
	OptionsPattern[]
] := windowSaturate[
	Switch[OptionValue["Measure"],
		"Counting",  Accumulate @ Values @ KeySort @ Counts @ DeleteCases[GraphDistance[g, vertex], Infinity],
		"Hausdorff", With[{vs = VertexList[g], row = GraphDistance[g, vertex]}, Table[Length @ GraphInterior[g, Pick[vs, Thread[row <= r]]], {r, 0, Max @ DeleteCases[row, Infinity]}]],
		"Front",     frontVolumeProfile[g, vertex, range]
	],
	range
]


(* ===================== Shell areas ===================== *)

(* A(r) = |dB_r(v)| = #{ w : d(v, w) == r } as the List {A(0), ..., A(ecc(v))}: the
   discrete geodesic-sphere areas, the radial derivative of BallVolumes (default "Counting"
   == Accumulate[ShellAreas]; the crystallography / OEIS coordination sequence, A(1) =
   the coordination number).  Same object/range convention as BallVolumes; a finite
   {rmin, rmax} window is rectangular, padding past eccentricity with 0 (empty sphere). *)

ShellAreas[g_Graph] := ShellAreas[g, All, All]

ShellAreas[g_Graph, pts : (All | _List | Except[_Rule | _RuleDelayed])] := ShellAreas[g, pts, All]

(* all/list form: one GraphDistanceMatrix; single vertex stays on one GraphDistance *)
ShellAreas[g_Graph,
	vertices : (_List | All),
	range : (_Integer | {_Integer, _Integer} | All)
] /; vertices === All || ! MemberQ[VertexList[g], vertices] :=
	With[{dm = GraphDistanceMatrix[g], vs = VertexList[g]},
		{idx = PositionIndex[vs]},
		(windowSaturate[Values @ KeySort @ Counts @ DeleteCases[dm[[idx[#][[1]]]], Infinity], range, 0] &) /@ If[vertices === All, vs, vertices]
	]

ShellAreas[g_Graph,
	vertex : Except[All | _Rule | _RuleDelayed],
	range : (_Integer | {_Integer, _Integer} | All)
] := windowSaturate[Values @ KeySort @ Counts @ DeleteCases[GraphDistance[g, vertex], Infinity], range, 0]


(* ===================== Cylinder volumes ===================== *)

(* CylinderVolumes[g, sources, targets, s] gives the matrix of cylinder volumes
   between every source-target pair: the cylinder from p to q is the metric
   interval I(p, q) = { w : d(p, w) + d(w, q) == d(p, q) } (the union of all
   p-q geodesics) thickened to its closed s-neighborhood, and its volume is the
   vertex count.  s defaults to 0 (the bare interval).  A scalar source gives a
   flat list ordered as targets (e.g. center -> shell -> anisotropy profile). *)

CylinderVolumes[g_Graph, source : Except[_List | _Rule | _RuleDelayed], targets_List, s_Integer : 0] :=
	First @ CylinderVolumes[g, {source}, targets, s]

CylinderVolumes[g_Graph, sources_List, targets_List, s_Integer : 0] :=
	With[{dm = GraphDistanceMatrix[g], pos = PositionIndex @ VertexList[g]},
		Outer[cylinderVolume[dm, pos[#1][[1]], pos[#2][[1]], s] &, sources, targets, 1]
	]

cylinderVolume[dm_, pi_, qi_, s_] :=
	With[{dpq = dm[[pi, qi]]},
		If[dpq === Infinity, 0,
			With[{interval = Flatten @ Position[dm[[pi]] + dm[[qi]], dpq]},
				If[s == 0, Length[interval], Count[Min /@ Transpose[dm[[interval]]], x_ /; x <= s]]
			]
		]
	]


(* ===================== Tube volumes ===================== *)

(* T(s) = |{ w : d(w, S) <= s }|, the tube-volume profile of a vertex set S, as the List
   {T(0), ..., T(sMax)} with T(0) = |S|, saturating at the component of S.  The core S is
   any vertex list -- a geodesic segment, a cycle (capless tube), a submanifold sample;
   TubeVolumes[g, p, q] takes S = the metric interval I(p, q) (the CylinderVolumes
   support, so TubeVolumes[g, p, q, s] == CylinderVolumes[g, p, {q}, s]).  Radius slot
   mirrors BallVolumes: All (default) | s_Integer | {smin, smax} rectangular.  For a
   geodesic core with direction v, Gray's tube expansion Vol T_s = omega_{n-1} s^(n-1) L
   (1 - (tau + Ric(v,v))/(6(n+1)) s^2 + O(s^4)) makes the quotient fit read the Ricci
   projection: DimensionCurvatureFit[..., "Probe" -> "Tube"].  One BFS from a virtual
   vertex joined to S -- no distance matrix. *)

TubeVolumes[g_Graph, core_List, range : (_Integer | {_Integer, _Integer} | All) : All] :=
	With[{aux = Unique["tubeSource"]},
		{aug = EdgeAdd[VertexAdd[g, {aux}], UndirectedEdge[aux, #] & /@ DeleteDuplicates[core]]},
		windowSaturate[
			Accumulate @ Values @ KeySort @ Counts @ DeleteCases[
				Values @ KeyDrop[AssociationThread[VertexList[aug], GraphDistance[aug, aux]], {aux}] - 1, Infinity],
			range]
	]

TubeVolumes[g_Graph, p : Except[_List], q : Except[_List], range : (_Integer | {_Integer, _Integer} | All) : All] :=
	With[{vs = VertexList[g]},
		{dp = AssociationThread[vs, GraphDistance[g, p]], dq = AssociationThread[vs, GraphDistance[g, q]]},
		TubeVolumes[g, Select[vs, dp[#] + dq[#] == dp[q] &], range]
	]


(* ===================== Geodesic interval graph ===================== *)

(* GeodesicIntervalGraph[g, u, v]: the metric interval I(u, v) as a directed
   acyclic graph -- vertices { w : d(u, w) + d(w, v) == d(u, v) } (= the
   CylinderVolumes support, the union of all u-v geodesics), edges w -> x for
   adjacent w, x in the interval with d(u, x) == d(u, w) + 1.  Directed paths
   u -> v are exactly the u-v geodesics.  Built from two distance fields, never
   enumerating paths, so it is polynomial even when the geodesic count is not. *)

GeodesicIntervalGraph[g_Graph, u_, v_] :=
	Module[{du = AssociationThread[VertexList[g], GraphDistance[g, u]],
			dv = AssociationThread[VertexList[g], GraphDistance[g, v]], duv, interval, inSet},
		duv = du[v];
		If[duv === Infinity, Graph[{}, {}],
			interval = Select[VertexList[g], du[#] + dv[#] == duv &];
			inSet = AssociationThread[interval, True];
			Graph[interval,
				Catenate @ Map[
					w |-> DirectedEdge[w, #] & /@ Select[AdjacencyList[g, w], TrueQ[inSet[#]] && du[#] == du[w] + 1 &],
					interval
				]
			]
		]
	]


(* ===================== Geodesic occupation ===================== *)

(* GeodesicOccupation[dag]: the per-vertex geodesic occupation c(w) =
   sigma_in(w) * sigma_out(w) over a geodesic DAG, where sigma_in(w) is the
   number of source -> w directed paths and sigma_out(w) the number of w -> sink
   paths (the Brandes shortest-path-count decomposition).  c(w) is the number of
   maximal directed paths through w; the family size M = Max c (attained at the
   endpoints).  Two topological-order DP sweeps, never enumerating the paths.
   GeodesicOccupation[g, u, v] builds the u-v geodesic DAG first. *)

(* Key[] wraps every vertex lookup: a list-valued vertex label (a {i, j} grid
   or tessellation label) would otherwise be read by Lookup / assoc[...] as a
   LIST OF KEYS, silently returning Missing instead of the count. *)
GeodesicOccupation[dag_Graph] :=
	Module[{order = TopologicalSort[dag],
			inNbr = GroupBy[EdgeList[dag], Last -> First],
			outNbr = GroupBy[EdgeList[dag], First -> Last], sigmaIn, sigmaOut},
		sigmaIn = Fold[
			{acc, w} |-> Append[acc, w -> With[{p = Lookup[inNbr, Key[w], {}]}, If[p === {}, 1, Total[Lookup[acc, Key /@ p]]]]],
			<||>, order];
		sigmaOut = Fold[
			{acc, w} |-> Append[acc, w -> With[{q = Lookup[outNbr, Key[w], {}]}, If[q === {}, 1, Total[Lookup[acc, Key /@ q]]]]],
			<||>, Reverse[order]];
		AssociationMap[Lookup[sigmaIn, Key[#]] Lookup[sigmaOut, Key[#]] &, VertexList[dag]]
	]

GeodesicOccupation[g_Graph, u_, v_] := GeodesicOccupation[GeodesicIntervalGraph[g, u, v]]


(* GeodesicEdgeOccupation[dag]: the per-edge geodesic occupation
   c(u -> v) = sigma_in(u) * sigma_out(v) over a geodesic DAG -- the number of
   maximal directed paths through the edge, keyed by the DAG's DirectedEdges;
   the edge companion of GeodesicOccupation, same two topological-order DP
   sweeps, never enumerating the paths.  GeodesicEdgeOccupation[g, u, v]
   builds the u-v geodesic DAG first. *)

GeodesicEdgeOccupation[dag_Graph] :=
	Module[{order = TopologicalSort[dag],
			inNbr = GroupBy[EdgeList[dag], Last -> First],
			outNbr = GroupBy[EdgeList[dag], First -> Last], sigmaIn, sigmaOut},
		sigmaIn = Fold[
			{acc, w} |-> Append[acc, w -> With[{p = Lookup[inNbr, Key[w], {}]}, If[p === {}, 1, Total[Lookup[acc, Key /@ p]]]]],
			<||>, order];
		sigmaOut = Fold[
			{acc, w} |-> Append[acc, w -> With[{q = Lookup[outNbr, Key[w], {}]}, If[q === {}, 1, Total[Lookup[acc, Key /@ q]]]]],
			<||>, Reverse[order]];
		Association[(# -> Lookup[sigmaIn, Key[First[#]]] Lookup[sigmaOut, Key[Last[#]]]) & /@ EdgeList[dag]]
	]

GeodesicEdgeOccupation[g_Graph, u_, v_] := GeodesicEdgeOccupation[GeodesicIntervalGraph[g, u, v]]


(* ===================== Log-difference quotients ===================== *)

(* q(r) = (Log w(r) - Log w(r-1)) / (Log(r+1) - Log r): the discrete d Log w / d Log r
   of any sequence w = {w(0), w(1), ...} against the log index, the log-log slope at
   each step.  Log[Ratios[Range[n]], Ratios[w]] == ResourceFunction["LogDifferences"][w].
   For a ball-volume sequence this is the volume-growth dimension estimator SW's
   dimension chapters plot.  Accepts any numeric -- or Around -- sequence: feed it
   BallVolumes[g, v], or aggregate first and let the spread propagate, e.g.
   LogDifferenceQuotients[MeanAround /@ Transpose[BallVolumes[g, subset, {0, R}]]]. *)
LogDifferenceQuotients[w_List] := Log[Ratios[Range[Length[w]]], Ratios[N[w]]]


(* ===================== Volume-growth observables ===================== *)

(* The growth observables at a vertex -- raw profiles and the fitted dimension / scalar
   curvature -- from the Bishop-Gromov regression of the log-difference quotient q(r) on
   x = r (r+1) (the squared geometric-mean radius), for BOTH growth probes:
     Ball volume V(r):  q -> d - R/(3(d+2)) x   (intercept d, R = -3(d+2) slope);
     Sphere area A(r) = ShellAreas (Gray: Area(S_r) = sigma_{n-1} r^(n-1)(1 - S/(6 n) r^2)):
                        q -> (n-1) - S/(3 n) x  (intercept n-1, manifold n = intercept+1, S = -3 n slope).
   Returns the flat association
     <|"BallVolumes", "ShellAreas", "BallLogDifferenceQuotients", "SphereLogDifferenceQuotients",
       "BallDimension", "SphereDimension", "BallScalarCurvature", "SphereScalarCurvature",
       "BallCurvatureByRadius", "SphereCurvatureByRadius", "SphereMeanCurvatureByRadius",
       "BallWindow", "SphereWindow"|>,
   where "BallVolumes"/"ShellAreas" are the raw growth profiles, "...LogDifferenceQuotients"
   their log-log slope sequences, the "...Dimension"/"...ScalarCurvature" are the single regressed parameters,
   "...CurvatureByRadius" the per-radius comparison profile (ball R(v,r) = 6(d+2)/r^2
   (1 - V(r)/V_E(d,r)); sphere S(v,r) = 6 n/r^2 (1 - A(r)/A_E(n,r))), and
   "SphereMeanCurvatureByRadius" the discrete geodesic-sphere mean curvature d Log A/dr
   (Raychaudhuri expansion theta).  The two probes give independent (n, S) readouts --
   their agreement is the consistency check.  "BallWindow"/"SphereWindow" are the radius
   windows the two fits used (they differ under Automatic: the sphere window is capped at
   the rising part of A(r) since it is non-monotonic on a finite graph).
   Window slot 3: {rmin, rmax}, All, or Automatic (default), the linear core of the (x, q)
   scatter -- the longest radius window whose least-squares residual stays within twice the
   noise floor.  "Dimension" -> d_Integer pins the intercept; "Measure" ("Counting"
   (default) | "Hausdorff") picks the ball-volume convention.  The ball dimension /
   curvature fit internally shifts the profile by one radius (B_{r-1}) so flat lattices
   read exact integer dimensions; "BallVolumes" still exposes the raw profile.  Vertex
   slot 2 (single, list, or All). *)

Options[VolumeGrowthObservables] = {"Measure" -> "Hausdorff", "Dimension" -> Automatic};

VolumeGrowthObservables[g_Graph, opts : OptionsPattern[]] :=
	VolumeGrowthObservables[g, All, Automatic, opts]

VolumeGrowthObservables[g_Graph,
	vertices : (_List | All),
	window : ({_Integer, _Integer} | All | Automatic) : Automatic,
	OptionsPattern[]
] /; vertices === All || ! MemberQ[VertexList[g], vertices] :=
	With[{dim = OptionValue["Dimension"]},
		MapThread[growthParams[#1, #2, window, dim] &,
			{BallVolumes[g, vertices, All, "Measure" -> OptionValue["Measure"]], ShellAreas[g, vertices, All]}]
	]

VolumeGrowthObservables[g_Graph,
	vertex : Except[All | _Rule | _RuleDelayed],
	window : ({_Integer, _Integer} | All | Automatic) : Automatic,
	OptionsPattern[]
] := growthParams[
	BallVolumes[g, vertex, All, "Measure" -> OptionValue["Measure"]], ShellAreas[g, vertex, All], window, OptionValue["Dimension"]]

(* DimensionCurvatureFit[{{r, q(r)}, ...}]: fit dimension d and scalar curvature R to log-difference
   quotients q(r) (each the discrete d Log f / d Log r at radius r) by Bishop-Gromov regression on
   x = r (r+1) -- the squared geometric-mean radius Sqrt[r(r+1)] at which a finite difference quotient
   lives.  DimensionCurvatureFit[{q(0), q(1), ...}] takes a bare quotient list at radii 0, 1, 2, ....
   probe table (dimension = intercept + shift, curvature factor = intercept + offset):
     "Ball"       (V ~ r^d,      shift 0, offset 2):  R = -3(d+2) slope;
     "Sphere"     (A ~ r^(d-1),  shift 1, offset 1):  R = -3 d slope;
     "Tube"       (T ~ s^(d-1),  shift 1, offset 2):  tau + Ric(v,v) = -3(d+1) slope (Gray);
     "TubeMantle" (dT ~ s^(d-2), shift 2, offset 1):  tau + Ric(v,v) = -3(d-1) slope.
   For the tube probes "ScalarCurvature" holds the tube reading tau + Ric(v,v), the Ricci
   projection plus the scalar; subtract a same-window ball tau to isolate Ric(v,v).
   Fits every supplied point (the caller windows by slicing the quotients); the fit uses
   closed-form normal equations (not LeastSquares) so Around-valued quotients carry their
   spread to Around dimension and curvature.  The caller supplies the quotients, so the
   convention is its choice: LogDifferenceQuotients (index-based) of a Counting profile,
   radialQuotients of a Hausdorff profile, ... *)

Options[DimensionCurvatureFit] = {"Probe" -> "Ball", "Dimension" -> Automatic};

DimensionCurvatureFit[q : {Except[_List] ..}, opts : OptionsPattern[]] :=
	DimensionCurvatureFit[Transpose[{Range[0, Length[q] - 1], q}], opts]

DimensionCurvatureFit[data : {{_, _} ..}, OptionsPattern[]] := With[
	{probe = OptionValue["Probe"], dimOpt = OptionValue["Dimension"]},
	{shift = Switch[probe, "Ball", 0, "Sphere" | "Tube", 1, "TubeMantle", 2],
	 offset = Switch[probe, "Ball" | "Tube", 2, "Sphere" | "TubeMantle", 1],
	 x = N[data[[All, 1]] (data[[All, 1]] + 1)], q = data[[All, 2]]},
	{dimScalar = If[dimOpt === Automatic,
		With[{m = Length[x], sx = Total[x], sxx = Total[x^2], sq = Total[q], sxq = Total[x q]},
			{den = m sxx - sx^2},
			{c2 = (m sxq - sx sq) / den, c1 = (sxx sq - sx sxq) / den},
			{c1 + shift, -3 (c1 + offset) c2}
		],
		With[{sx = Total[x], sxx = Total[x^2], sxq = Total[x q]},
			{slope = (sxq - (dimOpt - shift) sx) / sxx},
			{N[dimOpt], -3 (dimOpt - shift + offset) slope}
		]
	]},
	<|"Dimension" -> dimScalar[[1]], "ScalarCurvature" -> dimScalar[[2]]|>
]


(* q(r) = (Log f(r) - Log f(r-1)) / (Log r - Log(r-1)) sampled on the radius interval
   [r, r+1]: the radius-consistent log-log slope (matches dimensionCurvature's qAll, the
   sequence the fit regresses -- distinct from the index-based LogDifferenceQuotients) *)
radialQuotients[f_] :=
	Table[(Log[N @ f[[r + 2]]] - Log[N @ f[[r + 1]]]) / (Log[r + 1.] - Log[r]), {r, 1, Length[f] - 2}]

(* select a radius window over the quotients q (indexed by radius 1, 2, ...) -- All, an explicit
   {rmin, rmax}, or Automatic (the linear core: the longest contiguous run whose least-squares
   residual stays within twice the noise floor, exhaustive interval search on the central values
   with O(1)-per-interval prefix sums) -- then DimensionCurvatureFit the surviving {r, q(r)} pairs;
   reports the window used.  This is the windowing layer VolumeGrowthObservables wraps around the
   pure DimensionCurvatureFit. *)
windowedFit[q_, window_, probe_, dimOpt_] := With[
	{r = Range[Length[q]], x = N[Range[Length[q]] (Range[Length[q]] + 1)]},
	{sel = Which[
		window === All, r,
		ListQ[window], Select[r, window[[1]] <= # <= window[[2]] &],
		Length[q] < 5, r,
		True, Range @@ With[
			{qc = Replace[q, Around[m_, _] :> m, {1}], k = Length[q], k0 = 5},
			{sx = Prepend[Accumulate[x], 0.], sxx = Prepend[Accumulate[x^2], 0.],
			 sq = Prepend[Accumulate[qc], 0.], sqq = Prepend[Accumulate[qc^2], 0.],
			 sxq = Prepend[Accumulate[x qc], 0.]},
			{rse = {i, j} |-> With[
				{m = N[j - i + 1],
				 ax = sx[[j + 1]] - sx[[i]], axx = sxx[[j + 1]] - sxx[[i]],
				 aq = sq[[j + 1]] - sq[[i]], aqq = sqq[[j + 1]] - sqq[[i]],
				 axq = sxq[[j + 1]] - sxq[[i]]},
				{b = (m axq - ax aq) / (m axx - ax^2)},
				Sqrt[Max[aqq - (aq - b ax) aq / m - b axq, 0.] / (m - 2)]
			]},
			{tol = Max[2 Quantile[Table[rse[i, i + k0 - 1], {i, 1, k - k0 + 1}], 1/4], 1.*^-10]},
			SelectFirst[
				Catenate @ Table[{i, i + len - 1}, {len, k, k0, -1}, {i, 1, k - len + 1}],
				p |-> rse[p[[1]], p[[2]]] <= tol,
				{1, k}
			]
		]
	]},
	Append[
		DimensionCurvatureFit[Transpose[{r[[sel]], q[[sel]]}], "Probe" -> probe, "Dimension" -> dimOpt],
		"Window" -> MinMax[r[[sel]]]
	]
]

(* both probes' fitted parameters + per-radius profiles on one vertex's (ball volume,
   sphere area) pair: take the radius-consistent quotients of each profile and hand them to
   windowedFit.  The sphere fit uses the rising part of A(r) for an Automatic window
   since A(r) is non-monotonic on a finite graph (it peaks near the eccentricity), but the
   reported sphere profiles stay on the full A(r).  No boundary shift: the ball fit runs on the
   supplied volume (default Hausdorff, the boundary-corrected |GraphInterior[B_r]| = B_{r-1} on a
   regular lattice), so "BallVolumes"/"BallLogDifferenceQuotients" are exactly what was fitted *)
growthParams[w_, a_, window_, dimOpt_] := With[
	{qBall = radialQuotients[w], rising = If[window === Automatic, Take[a, First @ Ordering[a, -1]], a]},
	{ballFit = windowedFit[qBall, window, "Ball", dimOpt], qSph = radialQuotients[rising]},
	{sphFit = windowedFit[qSph, window, "Sphere", dimOpt]},
	{bd = ballFit["Dimension"], sd = sphFit["Dimension"]},
	<|
		"BallVolumes" -> w,
		"ShellAreas" -> a,
		"BallLogDifferenceQuotients" -> qBall,
		"SphereLogDifferenceQuotients" -> radialQuotients[a],
		"BallDimension" -> bd,
		"SphereDimension" -> sd,
		"BallScalarCurvature" -> ballFit["ScalarCurvature"],
		"SphereScalarCurvature" -> sphFit["ScalarCurvature"],
		"BallCurvatureByRadius" ->
			Table[N[6 (bd + 2) / r^2 (1 - w[[r + 1]] Gamma[bd / 2 + 1] / (Pi^(bd / 2) r^bd))], {r, 1, Length[w] - 1}],
		"SphereCurvatureByRadius" ->
			Table[N[6 sd / r^2 (1 - a[[r + 1]] Gamma[sd / 2 + 1] / (sd Pi^(sd / 2) r^(sd - 1)))], {r, 1, Length[a] - 1}],
		"SphereMeanCurvatureByRadius" -> Differences[Log[N[a]]],
		"BallWindow" -> ballFit["Window"],
		"SphereWindow" -> sphFit["Window"]
	|>
]


SectionalCurvatures[g_Graph] := With[{c = GraphComplex[g], deg = VertexDegree[g], vs = PositionIndex[VertexList[g]]},
    Total[Total[1 / Extract[deg, Lookup[vs, #]]] - 1 / 2 & /@ SimplexList[SimplexStar[c, {#}], {2}]] / 3 & /@ Keys[vs]
]


(* ===================== Effective resistance ===================== *)

(* Klein-Randic resistance distance R(u, v) = (e_u - e_v)^T L^+ (e_u - e_v),
   where L^+ is the Moore-Penrose pseudoinverse of the graph Laplacian
   (= the 0-block of GreenOperatorMatrix[GraphComplex[g]]).  Three forms:
   pair, full V x V matrix, submatrix on a vertex list. *)

EffectiveResistance[g_Graph] := Module[{lp, n, d},
    lp = PseudoInverse[N @ Normal @ KirchhoffMatrix[g]];
    n = VertexCount[g];
    d = Diagonal[lp];
    Table[d[[i]] + d[[j]] - 2 lp[[i, j]], {i, n}, {j, n}]
]

EffectiveResistance[g_Graph, u_, v_] /; MemberQ[VertexList[g], u] && MemberQ[VertexList[g], v] :=
    With[{lp = PseudoInverse[N @ Normal @ KirchhoffMatrix[g]],
          idx = AssociationThread[VertexList[g], Range @ VertexCount[g]]},
        With[{i = idx[u], j = idx[v]},
            lp[[i, i]] + lp[[j, j]] - 2 lp[[i, j]]
        ]
    ]

EffectiveResistance[g_Graph, vs_List] /; SubsetQ[VertexList[g], vs] :=
    With[{full = EffectiveResistance[g],
          ix = AssociationThread[VertexList[g], Range @ VertexCount[g]] /@ vs},
        full[[ix, ix]]
    ]


(* Klein-Randic / Schoenberg negative-type predicate: a real symmetric
   n x n matrix R with zero diagonal is realisable as a resistance
   distance matrix iff the centred Gram matrix
       B[i, j] = (R[1, j] + R[i, 1] - R[i, j]) / 2,  i, j >= 2
   is positive semidefinite. *)

ResistanceQ[r_ ? MatrixQ] :=
    SquareMatrixQ[r] &&
    (Transpose[r] === r || N @ Transpose[r] == N @ r) &&
    AllTrue[Diagonal[r], # == 0 &] &&
    With[{n = Length[r]},
        n <= 1 || AllTrue[
            Eigenvalues[N @ Table[(r[[1, j]] + r[[i, 1]] - r[[i, j]]) / 2, {i, 2, n}, {j, 2, n}]],
            # >= -10^-9 &
        ]
    ]

ResistanceQ[_] := False



SpacetimeGraph[{m_Integer, n_Integer}, opts___] := Block[{
	vs = Join[
		2 Tuples[{Range[m], Range[n]}] - 1,
		2 Tuples[{Range[m - 1], Range[n - 1]}]
	]
},
	RelationGraph[
		#2[[2]] - #1[[2]] == 1 && Abs[#2[[1]] - #1[[1]]] == 1 &,
		vs,
		opts,
		VertexCoordinates -> vs,
		DirectedEdges -> True
	]
]

SpacetimeTorusGraph[{m_Integer, n_Integer}, opts___] := Block[{
	vs = Join[
		2 Tuples[{Range[m], Range[n]}] - 1,
		2 Tuples[{Range[m], Range[n]}]
	]
},
	RelationGraph[
		Mod[#2[[2]] - #1[[2]], 2 n, 1] == 1 && MatchQ[Abs[#2[[1]] - #1[[1]]], 1 | 2 m - 1] &,
		vs,
		opts,
		VertexCoordinates -> vs,
		DirectedEdges -> True
	]
]

RotateEdge[DirectedEdge[i_, j_], {m_Integer : 1, n_Integer : 1}] :=
	DirectedEdge[i,
		MapThread[Mod[##, 1] &,
			{i + Cross[{If[Abs[j[[1]] - i[[1]]] > 1, -1, 1],
			If[Abs[i[[2]] - j[[2]]] > 1, -1, 1]} * Sign[j - i]],
			2 {m, n}}
		]
	]

KickEdge[DirectedEdge[i_, j_]] := If[i[[1]] < j[[1]],
	DirectedEdge[j, j + Cross[j - i], 1],
	DirectedEdge[j, j - Cross[j - i], -1]
]

PersistEdge[DirectedEdge[i_, j_]] := DirectedEdge[j, j + j - i]

RightMatrix[g_Graph, {m_Integer : 1, n_Integer : 1}] := With[{index = First /@ PositionIndex[EdgeList[g]]},
	SparseArray[
		MapIndexed[
			Enclose[Prepend[#2, Confirm @ Lookup[index, DirectedEdge[#, MapThread[Mod[##, 1] &, {# + {1, 1}, 2 {m, n}}]]]] -> 1, Nothing &] &,
			VertexList[g]
		],
		{Length[index], VertexCount[g]}
	]
]

ToroidalRightMatrix[g_Graph] := Block[{
    index = First /@ PositionIndex[EdgeList[g]], edges, weights
},
    edges = 
        Lookup[index, {DirectedEdge[#, # + {1, 1}], DirectedEdge[#, # + {-5, 1}]}, Nothing] & /@ 
            VertexList[g] // Flatten;
    weights = SparseArray[
        Table[{#, i} -> 1, {i, VertexCount[g]}] & /@ edges // Flatten,
        {Length[index], VertexCount[g]}
    ];
    weights Ramp[- Transpose@IncidenceMatrix[g]]
]

ToroidalLeftMatrix[g_Graph] := Block[{
    index = First /@ PositionIndex[EdgeList[g]], edges, weights
},
    edges = 
        Lookup[index, {DirectedEdge[#, # + {-1, 1}],DirectedEdge[#, # + {5, 1}]}, Nothing] & /@
            VertexList[g] // Flatten;
    weights = SparseArray[
        Table[{#, i} -> 1, {i, VertexCount[g]}] & /@ edges // Flatten,
        {Length[index], VertexCount[g]}
    ];
    weights Ramp[- Transpose@IncidenceMatrix[g]]
]

DiracWalk[g_Graph, p_ : 1 / 2] := With[{index = First /@ PositionIndex[EdgeList[g]]},
	SparseArray[
		Map[
			Enclose[
				With[{
					j1 = #[[3]] -> Confirm @ Lookup[index, #[[;; 2]]] & @ KickEdge[#],
					j2 = Confirm @ Lookup[index, PersistEdge[#]],
					i = Lookup[index, #]},
					Splice[{
						{j1[[2]], i} -> j1[[1]] * Sqrt[p],
						{j2, i} -> Sqrt[1 - p]
					}]
				],
				Nothing &
			] &,
			Keys[index]
		],
		{1, 1} * Length[index]
	]
]

VertexAmplitudes[g_Graph, edgeWeights_, {m_Integer : 1, n_Integer : 1}] := Map[
	# -> Chop[
		{1, I} . Lookup[
			edgeWeights,
			NestList[
				RotateEdge[#, {m, n}] &,
				DirectedEdge[#, MapThread[Mod[##, 1] &, {# + {1, 1}, 2 {m, n}}]],
				1
			],
			0
		]
	] &,
	VertexList[g]
]


GraphSuspension[g_ ? GraphQ] := With[{v1 = Unique[\[FormalV]], v2 = Unique[\[FormalV]]},
	Graph3D[EdgeAdd[g, Catenate[{UndirectedEdge[v1, #], UndirectedEdge[#, v2]} & /@ VertexList[g]]]]
]


RandomGraphAutomorphism[g_ ? GraphQ, n : _Integer | Automatic | All : Automatic] :=
	With[{gr = GraphAutomorphismGroup[g]}, {order = GroupOrder[gr]},
		GroupElements[gr, RandomSample[;; order, UpTo[Replace[n, {Automatic -> 1, All -> order}]]]] //
			If[n === Automatic, First, Identity]
	]

RandomGraphAutomorphism[g : {___List}, args___] := RandomGraphAutomorphism[ComplexGraph[g], args]

