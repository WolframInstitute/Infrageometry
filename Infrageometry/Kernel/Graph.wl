Package["WolframInstitute`Infrageometry`"]


PackageExport[SymmetricRelationGraph]
PackageExport[GraphEdgeWeights]
PackageExport[GraphVertexWeights]
PackageExport[GraphBoundary]
PackageExport[GraphInterior]
PackageExport[BallHull]
PackageExport[BallVolumeProfile]
PackageExport[CoordinationSequence]
PackageExport[CylinderVolumes]
PackageScope[cylinderVolume]

PackageExport[FormanRicciCurvature]
PackageExport[OllivierRicciCurvature]
PackageExport[WolframRicciCurvature]
PackageExport[WolframHausdorffDimension]
PackageExport[SectionalCurvatures]
PackageScope[wasserstein1]

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

(* GraphBoundary[g, S]: inner vertex boundary {v in S : v has a neighbor outside S}. *)
GraphBoundary[g_Graph, subgraph_Graph] :=
	GraphBoundary[g, VertexList[subgraph]]

GraphBoundary[g_Graph, subset_List] :=
	With[{outside = Complement[VertexList[g], subset]},
		Select[subset, IntersectingQ[AdjacencyList[g, #], outside] &]
	]

(* GraphInterior[g, S]: {v in S : all neighbors of v lie in S} = S \ GraphBoundary[g, S]. *)
GraphInterior[g_Graph, subgraph_Graph] :=
	GraphInterior[g, VertexList[subgraph]]

GraphInterior[g_Graph, subset_List] :=
	Complement[subset, GraphBoundary[g, subset]]

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


(* ===================== Ball-volume growth profile ===================== *)

(* V(r) = |B_r(v)| as the List {V(0), V(1), ..., V(ecc(v))} (position i is radius
   i - 1): the cumulative vertex count within each radius.  Distances from v in a
   connected component are the contiguous run 0..ecc, so the radius labels are
   positional and a List, not an Association, is the honest type.  The growth
   profile feeding WolframHausdorffDimension and WolframRicciCurvature.  Same
   calling convention as WolframHausdorffDimension: vertex slot 2 (single vertex,
   list, or All), radius slot 3 (default All -> full profile; r_Integer -> the
   scalar V(r), saturating at the reachable-vertex count past eccentricity;
   {rmin, rmax} -> the sub-profile over that radius window). *)

BallVolumeProfile[g_Graph] := BallVolumeProfile[g, All, All]

(* the all/list form reads every vertex's distances off one GraphDistanceMatrix:
   one optimized all-pairs call is ~200x faster than V separate GraphDistance BFS
   calls (each carries a fixed graph-to-internal-rep overhead paid V times) *)
BallVolumeProfile[g_Graph,
	vertices : (_List | All),
	range : (_Integer | {_Integer, _Integer} | All) : All
] /; vertices === All || ! MemberQ[VertexList[g], vertices] :=
	With[{dm = GraphDistanceMatrix[g], idx = PositionIndex @ VertexList[g]},
		With[{p = Accumulate @ Values @ KeySort @ Counts @ DeleteCases[dm[[idx[#][[1]]]], Infinity]},
			Switch[range,
				All,                  p,
				_Integer,             If[0 <= range < Length[p], p[[range + 1]], Last[p]],
				{_Integer, _Integer}, p[[Max[1, range[[1]] + 1] ;; Min[Length[p], range[[2]] + 1]]]
			]
		] & /@ If[vertices === All, VertexList[g], vertices]
	]

(* full profile {V(0), ..., V(ecc)} = running totals of the sphere sizes; r_Integer
   saturates at the reachable-vertex count past ecc, {a, b} clips to that window *)
BallVolumeProfile[g_Graph,
	vertex : Except[All | _Rule | _RuleDelayed],
	range : (_Integer | {_Integer, _Integer} | All) : All
] := With[{p = Accumulate @ Values @ KeySort @ Counts @ DeleteCases[GraphDistance[g, vertex], Infinity]},
	Switch[range,
		All,                  p,
		_Integer,             If[0 <= range < Length[p], p[[range + 1]], Last[p]],
		{_Integer, _Integer}, p[[Max[1, range[[1]] + 1] ;; Min[Length[p], range[[2]] + 1]]]
	]
]


(* ===================== Coordination sequence ===================== *)

(* S(r) = |S_r(v)| = #{ w : d(v, w) == r } as the List {S(0), S(1), ..., S(ecc(v))}
   (position i is radius i - 1): the sphere sizes, i.e. the discrete derivative of
   BallVolumeProfile (BallVolumeProfile = Accumulate of CoordinationSequence; the
   crystallography / OEIS coordination sequence, S(1) = the coordination number).
   Same calling convention as WolframHausdorffDimension / BallVolumeProfile:
   radius slot 3 default All -> full sequence; r_Integer -> the scalar S(r)
   (0 past eccentricity); {rmin, rmax} -> the sub-sequence over that radius window. *)

CoordinationSequence[g_Graph] := CoordinationSequence[g, All, All]

(* all/list form: one GraphDistanceMatrix; single vertex stays on one GraphDistance *)
CoordinationSequence[g_Graph,
	vertices : (_List | All),
	range : (_Integer | {_Integer, _Integer} | All) : All
] /; vertices === All || ! MemberQ[VertexList[g], vertices] :=
	With[{dm = GraphDistanceMatrix[g], idx = PositionIndex @ VertexList[g]},
		With[{s = Values @ KeySort @ Counts @ DeleteCases[dm[[idx[#][[1]]]], Infinity]},
			Switch[range,
				All,                  s,
				_Integer,             If[0 <= range < Length[s], s[[range + 1]], 0],
				{_Integer, _Integer}, s[[Max[1, range[[1]] + 1] ;; Min[Length[s], range[[2]] + 1]]]
			]
		] & /@ If[vertices === All, VertexList[g], vertices]
	]

CoordinationSequence[g_Graph,
	vertex : Except[All | _Rule | _RuleDelayed],
	range : (_Integer | {_Integer, _Integer} | All) : All
] := With[{s = Values @ KeySort @ Counts @ DeleteCases[GraphDistance[g, vertex], Infinity]},
	Switch[range,
		All,                  s,
		_Integer,             If[0 <= range < Length[s], s[[range + 1]], 0],
		{_Integer, _Integer}, s[[Max[1, range[[1]] + 1] ;; Min[Length[s], range[[2]] + 1]]]
	]
]


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


(* ===================== Wolfram-Ricci scalar curvature ===================== *)

(* Volume-comparison Ricci scalar at vertex v and integer radius r:
       R(v, r) = 6 (d + 2) / r^2 (1 - V(r) / V_E(d, r)),
       V_E(d, r) = pi^(d/2) r^d / Gamma[d/2 + 1],
   with V(r) = |B_r(v)|.  Local dimension d is supplied via "Dimension" -> d
   or read off the volume-growth local exponent when "Dimension" -> Automatic
   (default); in that case option "Differencing" selects the finite-difference
   scheme (shared with WolframHausdorffDimension), and the per-vertex valid radius
   range is that scheme's window intersected with r >= 1 (the 1/r^2 in R).

   The radius slot is a pure selector (matches BallVolumeProfile /
   CoordinationSequence): r_Integer -> the scalar R(v, r), a span or All ->
   the list of R(v, r) over that radius window.  Aggregate yourself, e.g.
   Mean @ WolframRicciCurvature[g, v, All], for a single representative scalar.

   Vertex slot 2, radius slot 3 (default All):
   WolframRicciCurvature[g, v, r_Integer]     -> R(v, r), scalar
   WolframRicciCurvature[g, v, {rmin, rmax}]  -> { R(v, r) } over [rmin, rmax], list
   WolframRicciCurvature[g, v] / [g, v, All]  -> { R(v, r) } over r = 1..ecc(v) - 1 (Automatic dim; ecc(v) with explicit "Dimension"), list
   WolframRicciCurvature[g, {v1, ...}, range] -> list, one entry per vertex
   WolframRicciCurvature[g, All, range]       -> list over VertexList[g]
   WolframRicciCurvature[g]                    -> list over VertexList[g], each a full profile.
   An integer r outside a vertex's valid range -> Indeterminate; an empty span -> {}. *)

Options[WolframRicciCurvature] = {"Dimension" -> Automatic, "Differencing" -> "Forward"};

WolframRicciCurvature[g_Graph, opts : OptionsPattern[]] :=
	WolframRicciCurvature[g, All, All, opts]

(* all/list form maps over BallVolumeProfile[g, vertices, All] (one GraphDistanceMatrix) *)
WolframRicciCurvature[g_Graph,
	vertices : (_List | All),
	range : (_Integer | {_Integer, _Integer} | All) : All,
	OptionsPattern[]
] /; vertices === All || ! MemberQ[VertexList[g], vertices] :=
	With[{dim = OptionValue["Dimension"], diff = OptionValue["Differencing"]},
		(With[{vols = #},
			{spec = If[dim === Automatic, hausdorffScheme[diff, vols], {dim &, 1, Length[vols] - 1}]},
			{dimFn = spec[[1]], rlo = Max[1, spec[[2]]], rhi = spec[[3]]},
			{ricci = r |-> With[{dr = dimFn[r], vr = vols[[r + 1]]}, N[6 (dr + 2) / r^2 (1 - vr Gamma[dr / 2 + 1] / (Pi^(dr / 2) r^dr))]]},
			Switch[range,
				All,                  ricci /@ Range[rlo, rhi],
				_Integer,             If[rlo <= range <= rhi, ricci[range], Indeterminate],
				{_Integer, _Integer}, ricci /@ Range[Max[rlo, range[[1]]], Min[rhi, range[[2]]]]
			]
		] &) /@ BallVolumeProfile[g, vertices, All]
	]

(* Except[All | _Rule | _RuleDelayed] (not also _List) so a single list-valued
   vertex lands here while option rules still defer to the OptionsPattern form.
   In Automatic-dim mode the local exponent dr and valid radius window come from the
   chosen "Differencing" scheme (the same hausdorffScheme as WolframHausdorffDimension);
   the Ricci 1/r^2 also forbids r = 0, hence the Max[1, .] clamp.  Explicit dim
   keeps the full [1, ecc] window. *)
WolframRicciCurvature[g_Graph,
	vertex : Except[All | _Rule | _RuleDelayed],
	range : (_Integer | {_Integer, _Integer} | All) : All,
	OptionsPattern[]
] := With[{dim = OptionValue["Dimension"], diff = OptionValue["Differencing"], vols = BallVolumeProfile[g, vertex]},
	{spec = If[dim === Automatic, hausdorffScheme[diff, vols], {dim &, 1, Length[vols] - 1}]},
	{dimFn = spec[[1]], rlo = Max[1, spec[[2]]], rhi = spec[[3]]},
	{ricci = r |-> With[{dr = dimFn[r], vr = vols[[r + 1]]}, N[6 (dr + 2) / r^2 (1 - vr Gamma[dr / 2 + 1] / (Pi^(dr / 2) r^dr))]]},
	Switch[range,
		All,                  ricci /@ Range[rlo, rhi],
		_Integer,             If[rlo <= range <= rhi, ricci[range], Indeterminate],
		{_Integer, _Integer}, ricci /@ Range[Max[rlo, range[[1]]], Min[rhi, range[[2]]]]
	]
]


(* Volume-growth local dimension at vertex v and radius r: the local scaling
   exponent (elasticity) d(v, r) = d log V / d log r of the ball volume
   V(r) = |B_r(v)|, estimated by a finite-difference scheme on the profile.
   Shared with WolframRicciCurvature[..., "Dimension" -> Automatic]; same
   selector calling convention.

   Option "Differencing" picks the stencil for the slope of y = log V against
   u = log r.  Each is a linear combination of the y-values at neighbouring
   radii (Sum c_i y(r_i)), the weights c_i fixed by the method of undetermined
   coefficients on the nodes u_i = log r_i:

     "Forward"  (default)  secant on [r, r+1]:   (y(r+1) - y(r)) / (u(r+1) - u(r))
     "Backward"            secant on [r-1, r]:   (y(r) - y(r-1)) / (u(r) - u(r-1))
     "Central"             secant on [r-1, r+1]: (y(r+1) - y(r-1)) / (u(r+1) - u(r-1))
     "Trapezoid"           mean of the Forward and Backward secant slopes
                           (the slope of the piecewise-linear interpolant at r)
     {"Stencil", offsets}  general k-point scheme: Fornberg / undetermined-
                           coefficient weights for dy/du at u(r) on the nodes
                           {u(r+o) : o in offsets}.  {0,1} reproduces Forward and
                           {-1,0} Backward exactly (a 2-point stencil is the unique
                           secant); a 3+-point stencil is the genuine higher-order
                           derivative (exact on degree k-1, error O(h^{k-1})) and,
                           on this non-uniform log grid, differs from the 2-point
                           "Central" secant -- it reduces to it only on a uniform grid.

   All share the limit d as r -> infinity; on a unit lattice each still carries
   an O(1/r) finite-radius bias (Central / Trapezoid with a smaller constant
   than the one-sided pair) -- see the research note for the bias analysis and
   the abscissa-shift that removes it.

   Missing values: a scheme is undefined at radii where its stencil leaves the
   profile 0..ecc or hits log 0; these are returned as Indeterminate (never a
   silent one-sided stand-in), so each scheme carries a valid window [rlo, rhi]:
     "Forward"            [1, ecc-1]                  "Backward"  [2, ecc]
     "Central"/"Trapezoid" [2, ecc-1]
     {"Stencil", offsets} [max(1, 1 - min offsets), ecc - max offsets]

   Vertex slot 2, radius slot 3 (default All), then the option:
   WolframHausdorffDimension[g, v, r_Integer]     -> d(v, r) or Indeterminate
   WolframHausdorffDimension[g, v, {rmin, rmax}]  -> { d(v, r) } clamped to the window
   WolframHausdorffDimension[g, v] / [g, v, All]  -> { d(v, r) } over the window
   WolframHausdorffDimension[g, {v1, ...}, range] -> one entry per vertex
   WolframHausdorffDimension[g, All, range]       -> list over VertexList[g]
   WolframHausdorffDimension[g]                    -> list over VertexList[g]. *)

WolframHausdorffDimension::baddiff = "Unknown \"Differencing\" scheme `1`; use \"Forward\", \"Backward\", \"Central\", \"Trapezoid\", or {\"Stencil\", offsets}.";

Options[WolframHausdorffDimension] = {"Differencing" -> "Forward"};

WolframHausdorffDimension[g_Graph, opts : OptionsPattern[]] := WolframHausdorffDimension[g, All, All, opts]

(* all/list form maps over BallVolumeProfile[g, vertices, All] (one GraphDistanceMatrix) *)
WolframHausdorffDimension[g_Graph,
	vertices : (_List | All),
	range : (_Integer | {_Integer, _Integer} | All) : All,
	OptionsPattern[]
] /; vertices === All || ! MemberQ[VertexList[g], vertices] :=
	With[{diff = OptionValue["Differencing"]},
		(With[{spec = hausdorffScheme[diff, #]},
			Switch[range,
				All,                  spec[[1]] /@ Range[spec[[2]], spec[[3]]],
				_Integer,             If[spec[[2]] <= range <= spec[[3]], spec[[1]][range], Indeterminate],
				{_Integer, _Integer}, spec[[1]] /@ Range[Max[spec[[2]], range[[1]]], Min[spec[[3]], range[[2]]]]
			]
		] &) /@ BallVolumeProfile[g, vertices, All]
	]

(* Except[All | _Rule | _RuleDelayed] so a single list-valued vertex lands here
   while a trailing option rule defers to the options form *)
WolframHausdorffDimension[g_Graph,
	vertex : Except[All | _Rule | _RuleDelayed],
	range : (_Integer | {_Integer, _Integer} | All) : All,
	OptionsPattern[]
] := With[{spec = hausdorffScheme[OptionValue["Differencing"], BallVolumeProfile[g, vertex]]},
	Switch[range,
		All,                  spec[[1]] /@ Range[spec[[2]], spec[[3]]],
		_Integer,             If[spec[[2]] <= range <= spec[[3]], spec[[1]][range], Indeterminate],
		{_Integer, _Integer}, spec[[1]] /@ Range[Max[spec[[2]], range[[1]]], Min[spec[[3]], range[[2]]]]
	]
]

(* {localExponentFn, rlo, rhi}: the scheme's per-radius estimator and its valid
   radius window on the profile vols (vols[[r+1]] = V(r), ecc = Length - 1) *)
hausdorffScheme[diff_, vols_] := With[{lv = Log[N[vols]], ecc = Length[vols] - 1},
	Switch[diff,
		"Forward",   {r |-> (lv[[r + 2]] - lv[[r + 1]]) / (Log[r + 1] - Log[r]),     1, ecc - 1},
		"Backward",  {r |-> (lv[[r + 1]] - lv[[r]])     / (Log[r] - Log[r - 1]),     2, ecc},
		"Central",   {r |-> (lv[[r + 2]] - lv[[r]])     / (Log[r + 1] - Log[r - 1]), 2, ecc - 1},
		"Trapezoid", {r |-> ((lv[[r + 2]] - lv[[r + 1]]) / (Log[r + 1] - Log[r]) + (lv[[r + 1]] - lv[[r]]) / (Log[r] - Log[r - 1])) / 2, 2, ecc - 1},
		{"Stencil", {__Integer}}, stencilScheme[lv, ecc, diff[[2]]],
		_, Message[WolframHausdorffDimension::baddiff, diff];
		   {r |-> (lv[[r + 2]] - lv[[r + 1]]) / (Log[r + 1] - Log[r]), 1, ecc - 1}
	]
]

(* general k-point discrete derivative: weights from the method of undetermined
   coefficients (Fornberg) for dy/du = d log V / d log r at the node u = log r,
   on the nodes log(r + o), o in offsets.  Recovers Forward {0,1}, Backward
   {-1,0}, Central {-1,0,1}; the window keeps every r + o >= 1 and r + o <= ecc *)
stencilScheme[lv_, ecc_, offsets_] := {
	r |-> fdWeights[Log[r + offsets], Log[r], 1] . lv[[r + offsets + 1]],
	Max[1, 1 - Min[offsets]], ecc - Max[offsets]
}

(* weights c_i with Sum_i c_i (x_i - x0)^j = m! delta_{j,m}, j = 0..k-1: the unique
   k-point stencil exact on polynomials of degree < k, approximating the m-th
   derivative at x0 (order k - m).  Vandermonde solve; Fornberg's stable recursion
   for large k, unneeded at the k <= 5 used here. *)
fdWeights[nodes_, x0_, m_] := LinearSolve[
	Table[If[j == 0, 1, (nodes[[i]] - x0)^j], {j, 0, Length[nodes] - 1}, {i, Length[nodes]}],
	m! UnitVector[Length[nodes], m + 1]
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

