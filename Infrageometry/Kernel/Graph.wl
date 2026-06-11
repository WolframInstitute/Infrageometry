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
PackageExport[WolframVolumeLogDifferenceQuotients]
PackageExport[WolframDimensionCurvatureFit]
PackageExport[WolframRicciCurvature]
PackageExport[WolframHausdorffDimension]
PackageExport[SectionalCurvatures]
PackageScope[wasserstein1]
PackageScope[volumeSequences]

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


(* ===================== Volume-growth log-difference quotients ===================== *)

(* q(r) = (log v(r+1) - log v(r)) / (log(r+1) - log r): the 2-point log-difference
   quotient of the volume sequence v against the log-radius -- the discrete
   d log V / d log r, best-approximating the elasticity at the geometric-mean
   radius r* = Sqrt[r (r+1)] (the divided-difference midpoint in log r).  With the
   default "PeeledBall" volume v(r) = V(r-1) this is SW's LogDifferences quotient,
   the clean non-shifted form.  Default returns the flat sequence {q(1), ..., q(ecc-1)};
   "Abscissa" -> True returns {r*, q} pairs (the ListPlot scatter feeding the fit).
   Vertex slot 2 (single vertex, list, or All). *)

Options[WolframVolumeLogDifferenceQuotients] = {"Volume" -> "PeeledBall", "Abscissa" -> False};

WolframVolumeLogDifferenceQuotients[g_Graph, opts : OptionsPattern[]] :=
	WolframVolumeLogDifferenceQuotients[g, All, opts]

WolframVolumeLogDifferenceQuotients[g_Graph,
	vertices : (_List | All),
	OptionsPattern[]
] /; vertices === All || ! MemberQ[VertexList[g], vertices] :=
	With[{abscissa = TrueQ[OptionValue["Abscissa"]]},
		(With[{lw = Log[N[#]], n = Length[#]},
			With[{q = Table[(lw[[r + 2]] - lw[[r + 1]]) / (Log[r + 1.] - Log[r]), {r, 1, n - 2}]},
				If[abscissa, Table[{Sqrt[r (r + 1)], q[[r]]}, {r, Length[q]}], q]
			]
		] &) /@ volumeSequences[g, vertices, OptionValue["Volume"]]
	]

WolframVolumeLogDifferenceQuotients[g_Graph,
	vertex : Except[All | _Rule | _RuleDelayed],
	OptionsPattern[]
] := With[{w = volumeSequences[g, vertex, OptionValue["Volume"]], abscissa = TrueQ[OptionValue["Abscissa"]]},
	With[{lw = Log[N[w]], n = Length[w]},
		With[{q = Table[(lw[[r + 2]] - lw[[r + 1]]) / (Log[r + 1.] - Log[r]), {r, 1, n - 2}]},
			If[abscissa, Table[{Sqrt[r (r + 1)], q[[r]]}, {r, Length[q]}], q]
		]
	]
]


(* ===================== Bishop-Gromov dimension + curvature fit ===================== *)

(* Bishop-Gromov fit at a vertex: regress the log-difference quotient q(r) on
   x = r (r+1), the squared geometric-mean radius.  The expansion
   E V(rho) = d - R/(3(d+2)) rho^2 gives intercept = dimension d and
   slope = -R/(3(d+2)), so scalar curvature R = -3 (d + 2) slope.  Returns
   <|"Dimension" -> d, "ScalarCurvature" -> R, "Window" -> {rmin, rmax}|> with
   the radius window the fit actually used.
   "Dimension" -> d_Integer pins the intercept and fits only the slope.  Window
   slot 3: {rmin, rmax} radii fed to the regression, All, or Automatic (default),
   the linear core of the (x, q) scatter -- the longest radius window whose
   least-squares residual error stays within twice the noise floor (the lower
   quartile of all 5-point-window errors), ties to the smallest rmin since the
   expansion is asymptotic at rho -> 0.  Vertex slot 2 (single, list, or All). *)

Options[WolframDimensionCurvatureFit] = {"Volume" -> "PeeledBall", "Dimension" -> Automatic};

WolframDimensionCurvatureFit[g_Graph, opts : OptionsPattern[]] :=
	WolframDimensionCurvatureFit[g, All, Automatic, opts]

WolframDimensionCurvatureFit[g_Graph,
	vertices : (_List | All),
	window : ({_Integer, _Integer} | All | Automatic) : Automatic,
	OptionsPattern[]
] /; vertices === All || ! MemberQ[VertexList[g], vertices] :=
	With[{dim = OptionValue["Dimension"]},
		(dimensionCurvature[#, window, dim] &) /@ volumeSequences[g, vertices, OptionValue["Volume"]]
	]

WolframDimensionCurvatureFit[g_Graph,
	vertex : Except[All | _Rule | _RuleDelayed],
	window : ({_Integer, _Integer} | All | Automatic) : Automatic,
	OptionsPattern[]
] := dimensionCurvature[volumeSequences[g, vertex, OptionValue["Volume"]], window, OptionValue["Dimension"]]

(* one fit on a volume sequence: q(r) on x = r (r+1) over the radius window;
   Automatic detects the linear core by exhaustive interval search (detection
   always uses the free 2-parameter line, independent of a pinned dimension);
   prefix sums give each interval's residual error in O(1) *)
dimensionCurvature[w_, window_, dimOpt_] := With[
	{lw = Log[N[w]], n = Length[w]},
	{qAll = Table[(lw[[r + 2]] - lw[[r + 1]]) / (Log[r + 1.] - Log[r]), {r, 1, n - 2}]},
	{rs = Which[
		window === All, Range[1, n - 2],
		ListQ[window], Select[Range[1, n - 2], window[[1]] <= # <= window[[2]] &],
		n - 2 < 5, Range[1, n - 2],
		True, With[
			{k = n - 2, k0 = 5, xAll = N[Range[n - 2] (Range[n - 2] + 1)]},
			{sx = Prepend[Accumulate[xAll], 0.], sxx = Prepend[Accumulate[xAll^2], 0.],
			 sq = Prepend[Accumulate[qAll], 0.], sqq = Prepend[Accumulate[qAll^2], 0.],
			 sxq = Prepend[Accumulate[xAll qAll], 0.]},
			{rse = {i, j} |-> With[
				{m = N[j - i + 1],
				 ax = sx[[j + 1]] - sx[[i]], axx = sxx[[j + 1]] - sxx[[i]],
				 aq = sq[[j + 1]] - sq[[i]], aqq = sqq[[j + 1]] - sqq[[i]],
				 axq = sxq[[j + 1]] - sxq[[i]]},
				{b = (m axq - ax aq) / (m axx - ax^2)},
				Sqrt[Max[aqq - (aq - b ax) aq / m - b axq, 0.] / (m - 2)]
			]},
			{tol = Max[2 Quantile[Table[rse[i, i + k0 - 1], {i, 1, k - k0 + 1}], 1/4], 1.*^-10]},
			Range @@ SelectFirst[
				Catenate @ Table[{i, i + len - 1}, {len, k, k0, -1}, {i, 1, k - len + 1}],
				p |-> rse[p[[1]], p[[2]]] <= tol,
				{1, k}
			]
		]
	]},
	{x = N[rs (rs + 1)], q = qAll[[rs]]},
	If[dimOpt === Automatic,
		With[{c = LeastSquares[Transpose[{ConstantArray[1., Length[x]], x}], q]},
			<|"Dimension" -> c[[1]], "ScalarCurvature" -> -3 (c[[1]] + 2) c[[2]], "Window" -> MinMax[rs]|>
		],
		With[{m = First @ LeastSquares[Transpose[{x}], q - dimOpt]},
			<|"Dimension" -> N[dimOpt], "ScalarCurvature" -> -3 (dimOpt + 2) m, "Window" -> MinMax[rs]|>
		]
	]
]


(* ===================== Wolfram-Hausdorff dimension ===================== *)

(* Volume-growth (Hausdorff) dimension: the fitted intercept d of the Bishop-Gromov
   regression, i.e. the "Dimension" projection of WolframDimensionCurvatureFit.
   Window slot 3 ({rmin, rmax}, All, or Automatic (default), the linear core),
   vertex slot 2 (single, list, or All). *)

Options[WolframHausdorffDimension] = {"Volume" -> "PeeledBall"};

WolframHausdorffDimension[g_Graph, opts : OptionsPattern[]] :=
	WolframHausdorffDimension[g, All, Automatic, opts]

WolframHausdorffDimension[g_Graph,
	vertices : (_List | All),
	window : ({_Integer, _Integer} | All | Automatic) : Automatic,
	OptionsPattern[]
] /; vertices === All || ! MemberQ[VertexList[g], vertices] :=
	#["Dimension"] & /@ WolframDimensionCurvatureFit[g, vertices, window, "Volume" -> OptionValue["Volume"]]

WolframHausdorffDimension[g_Graph,
	vertex : Except[All | _Rule | _RuleDelayed],
	window : ({_Integer, _Integer} | All | Automatic) : Automatic,
	OptionsPattern[]
] := WolframDimensionCurvatureFit[g, vertex, window, "Volume" -> OptionValue["Volume"]]["Dimension"]


(* ===================== Wolfram-Ricci scalar curvature ===================== *)

(* Volume-comparison Ricci scalar at vertex v: R(v, r) = 6 (d + 2)/r^2 (1 - v(r)/V_E(d, r)),
   V_E(d, r) = pi^(d/2) r^d / Gamma[d/2 + 1], one value per radius r = 1..ecc.
   Dimension d is supplied via "Dimension" -> d, or fitted once via
   WolframDimensionCurvatureFit when "Dimension" -> Automatic (default).  Volume
   v(r) is the chosen "Volume" convention (default "PeeledBall").  Vertex slot 2
   (single, list, or All). *)

Options[WolframRicciCurvature] = {"Dimension" -> Automatic, "Volume" -> "PeeledBall"};

WolframRicciCurvature[g_Graph, opts : OptionsPattern[]] :=
	WolframRicciCurvature[g, All, opts]

WolframRicciCurvature[g_Graph,
	vertices : (_List | All),
	OptionsPattern[]
] /; vertices === All || ! MemberQ[VertexList[g], vertices] :=
	With[{vol = OptionValue["Volume"], dimOpt = OptionValue["Dimension"]},
		With[{
			seqs = volumeSequences[g, vertices, vol],
			dims = If[dimOpt === Automatic,
				#["Dimension"] & /@ WolframDimensionCurvatureFit[g, vertices, Automatic, "Volume" -> vol],
				ConstantArray[dimOpt, Length @ If[vertices === All, VertexList[g], vertices]]
			]
		},
			MapThread[
				Function[{w, d},
					Table[N[6 (d + 2) / r^2 (1 - w[[r + 1]] Gamma[d / 2 + 1] / (Pi^(d / 2) r^d))], {r, 1, Length[w] - 1}]
				],
				{seqs, dims}
			]
		]
	]

WolframRicciCurvature[g_Graph,
	vertex : Except[All | _Rule | _RuleDelayed],
	OptionsPattern[]
] := With[{vol = OptionValue["Volume"], dimOpt = OptionValue["Dimension"]},
	With[{
		w = volumeSequences[g, vertex, vol],
		d = If[dimOpt === Automatic, WolframDimensionCurvatureFit[g, vertex, Automatic, "Volume" -> vol]["Dimension"], dimOpt]
	},
		Table[N[6 (d + 2) / r^2 (1 - w[[r + 1]] Gamma[d / 2 + 1] / (Pi^(d / 2) r^d))], {r, 1, Length[w] - 1}]
	]
]


(* ===================== Volume conventions ===================== *)

(* The ball-volume sequence v(0), v(1), ..., v(ecc) under one of three conventions:
   "Count" = |B_r| (raw cumulative count); "PeeledBall" = |B_{r-1}| (outer shell
   peeled -- SW's implicit convention, reproducing the LogDifferences shift);
   "Interior" = |B_r| - |dB_r| = |GraphInterior[B_r]| (boundary-corrected; equals
   "PeeledBall" in the bulk, differs near the eccentricity).  All/list form reads
   one GraphDistanceMatrix; single-vertex form one GraphDistance. *)

volumeSequences[g_Graph, vertices : (_List | All), vol_] /;
	vertices === All || ! MemberQ[VertexList[g], vertices] := Switch[vol,
	"Count",      BallVolumeProfile[g, vertices, All],
	"PeeledBall", (Join[{First[#]}, Most[#]] &) /@ BallVolumeProfile[g, vertices, All],
	"Interior",   With[{vs = VertexList[g], dm = GraphDistanceMatrix[g], idx = PositionIndex @ VertexList[g]},
		(With[{row = dm[[idx[#][[1]]]]},
			Table[Length @ GraphInterior[g, Pick[vs, Thread[row <= r]]], {r, 0, Max @ DeleteCases[row, Infinity]}]
		] &) /@ If[vertices === All, vs, vertices]
	]
]

volumeSequences[g_Graph, vertex_, vol_] := Switch[vol,
	"Count",      BallVolumeProfile[g, vertex],
	"PeeledBall", With[{c = BallVolumeProfile[g, vertex]}, Join[{First[c]}, Most[c]]],
	"Interior",   With[{vs = VertexList[g], row = GraphDistance[g, vertex]},
		Table[Length @ GraphInterior[g, Pick[vs, Thread[row <= r]]], {r, 0, Max @ DeleteCases[row, Infinity]}]
	]
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

