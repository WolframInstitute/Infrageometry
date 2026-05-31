Package["WolframInstitute`Infrageometry`"]


PackageExport[ComplexEmbedding]
PackageExport[ComplexMesh]
PackageExport[HighlightComplex]
PackageExport[GraphMesh]
PackageExport[InteriorMeshGraph]
PackageExport[MeshComplex]

PackageExport[MeshIncidenceMatrix]

PackageExport[CellOrientation]
PackageExport[FlipCellOrientation]
PackageExport[HighlightCellOrientations]
PackageExport[OrientMeshRegion]
PackageExport[TriangulateArrayMesh]
PackageExport[OrientableMeshRegionQ]

PackageExport[MoebiusMesh]
PackageExport[PathMesh]

PackageExport[UnitLengthGraph]
PackageExport[UnitLengthEmbedding]

PackageScope[hardSphereFixup]
PackageScope[fibonacciSeed]
PackageScope[unitLengthSeed]
PackageScope[packRetraction]
PackageScope[packMeasure]
PackageScope[contactGraph]
PackageScope[contactEdgeLengths]
PackageScope[iterativeProjectionPack]
PackageScope[constrainedPack]
PackageScope[unitLengthRepulsion]


Options[ComplexEmbedding] = {"RepulsiveForcePower" -> 1.*^-2, "Scale" -> 1, "Epsilon" -> 1.*^-3}

ComplexEmbedding[g : {___List}, d : 2 | 3 : 3, OptionsPattern[]] := Block[{
	vs = ComplexVertexList[g], l, x = \[FormalX],
    edges,
    rep = OptionValue["RepulsiveForcePower"], eps = OptionValue["Epsilon"], scale = OptionValue["Scale"],
    vars, energy, repulsion, constraints, func, sol
},
    If[vs === {}, Return[{}]];
	l = Length[vs];
	vars = Catenate[Table[x[v, k], {v, vs}, {k, d}]];
    edges = ComplexClosure[SimplexList[g], {2}];
	energy = Total @ MapApply[
		{a, b} |-> With[{r = Table[x[a, k] - x[b, k], {k, d}] },
			(Sqrt[r . r] - 1) ^ 2
		],
		edges
	];
	repulsion = rep * Total @ MapApply[
		{as, bs} |-> With[{r = Mean @ Table[x[a, k], {a, as}, {k, d}] - Mean @ Table[x[b, k], {b, bs}, {k, d}]},
			(Abs[Length[as] - Length[bs]] + 1) / (r . r + eps)
		],
		Subsets[g, {2}]
	];
	constraints = Append[
		If[edges === {}, {}, Table[Sum[x[v, k], {v, vs}] == 0, {k, d}]],
		Sum[Sum[x[v, k] ^ 2, {k, d}], {v, vs}] == scale * l
	];
	With[{cf = Compile[Evaluate[{#, _Real} & /@ vars], Evaluate[energy + repulsion], RuntimeOptions -> "Speed"]},
		func[{xs___ ? NumericQ}] := cf[xs]
	];
	sol = NArgMin[
		{func[vars], constraints},
		Element[vars, Reals]
	];
	Partition[sol, d]
]


Options[ComplexMesh] = Join[Options[ComplexEmbedding], Options[MeshRegion]]

ComplexMesh[g : {___List}, arg : {Repeated[_ ? NumericQ, {2, 3}] ..} | 2 | 3 : 3, opts : OptionsPattern[]] := Enclose @ Block[{
    d = Replace[arg, _List :> Max[Length /@ arg]],
    coords,
    simplices, faces
},
    coords = ComplexEmbedding[g, d, FilterRules[{opts}, Options[ComplexEmbedding]]];
    ConfirmAssert[Length[coords] == SimplexCardinality[g, 0]];
    If[coords === {}, Return[Region[EmptyRegion[d]]]];
    simplices = SimplexList[g, Replace[arg, _List :> Max[Length /@ arg]]];
    faces = ComplexClosure[Cases[simplices, {_, _, _, _}], {3}];
	MeshRegion[
        coords,
		Map[
            Switch[
                Length[#],
                1,
                    Style[Point[#], Directive[StandardYellow, PointSize[Large]]],
                2,
                    Style[Line[#], Directive[StandardGray, Thick]],
                3,
                    If[MemberQ[faces, #], Triangle[#], Style[Triangle[#], StandardBlue]],
                4,
                    Style[Tetrahedron[#], StandardGreen]
            ] &,
            simplices
        ],
        FilterRules[{opts}, Options[MeshRegion]],
		MeshCellLabel -> {0 -> "Index"}
	]
]


SimplexCell[x_List] := If[Length[x] == 1, Point, Simplex][x]

Options[HighlightComplex] = Options[HighlightMesh]

HighlightComplex[g : {___List}, h : {(_List | Style[_List, ___]) ...}, opts : OptionsPattern[]] := HighlightMesh[
    ComplexMesh[ComplexClosure[g, 4]],
    Replace[h, {Style[x_, dir___] :> Style[SimplexCell[x], dir], x_ :> SimplexCell[x]}, 1],
    FilterRules[{opts}, Options[ComplexMesh]]
]


GraphMesh[g_ ? GraphQ] := MeshRegion[
	GraphEmbedding[g],
	Join[Line /@ List @@@ EdgeList[g], Triangle /@ FindClique[g, {3}, All], Tetrahedron /@ FindClique[g, {4}, All]]
]


(* surface vertices lie on a boundary face (a (d-1)-subset of a top simplex occurring in exactly one top cell); keep every edge with at least one interior endpoint *)
InteriorMeshGraph[mr_MeshRegion] := With[
	{coords = MeshCoordinates[mr], d = RegionDimension[mr], edges = UndirectedEdge @@@ (First /@ MeshCells[mr, 1])},
	With[{surface = If[d <= 1,
		{},
		Union @@ Keys @ Select[Counts[Sort /@ Catenate[Subsets[First[#], {d}] & /@ MeshCells[mr, d]]], # == 1 &]
	]},
		EdgeDelete[
			Graph[Range[Length[coords]], edges, VertexCoordinates -> coords],
			Select[edges, SubsetQ[surface, List @@ #] &]
		]
	]
]


MeshComplex[mr_ ? RegionQ] := IndexComplex[Replace[Catenate[Select[MeshCells[mr], ListQ]], {Point[x_] :> {x}, _[x_] :> x}, 1]]


MeshIncidenceMatrix[mr_MeshRegion, n : _Integer ? Positive : 1] :=
	SparseArray @ Outer[
		Switch[n,
			1,
			Switch[#2, {_, #1}, 1, {#1, _}, -1, _, 0],
			_,
			With[{parts = Partition[#2, n, 1, 1]},
				Which[MemberQ[parts, #1], 1, MemberQ[parts, Reverse[#1]], -1, True, 0]
			]
		] &,
		First /@ MeshCells[mr, n - 1],
		First /@ MeshCells[mr, n],
		1
	]


CellOrientation[_[cell : {__Integer}]] := Signature[cell]

CellOrientation[_] := 1


FlipCellOrientation[head_[cell : {__Integer}]] := head[If[EvenQ[Length[cell]], RotateRight, Reverse][cell]]

FlipCellOrientation[cell_] := cell


HighlightCellOrientations[mesh_MeshRegion] := With[{d = RegionDimension[mesh]},
	HighlightMesh[
		mesh,
		MapThread[
			Style[#1, If[#2 > 0, StandardRed, StandardBlue]] &,
			{
				MeshCellIndex[mesh, d],
				CellOrientation /@ MeshCells[mesh, d]
			}
		],
		If[d > 2, MeshCellStyle -> {3 -> Opacity[1 / 2]}, {}]
	]
]

OrientMeshRegion[mesh_MeshRegion] := Enclose @ Block[{
	d = RegionDimension[mesh],
	cells, g,
	side1, signs
},
	cells = MeshCells[mesh, d];
	g = MeshConnectivityGraph[mesh, d];
	side1 = FindMaximumCut[g][[2, 1, All, 2]];
	signs = ConstantArray[-1, Length[cells]];
	signs[[side1]] = 1;
  
	MeshRegion[
		MeshCoordinates[mesh],
		MapThread[
			{cell, flip} |-> If[flip, FlipCellOrientation[cell], cell],
			{cells, MapThread[Unequal, {CellOrientation /@ cells, signs}]}
		],
		Method -> {"CheckOrientation" -> False}
	]
]

TriangulateArrayMesh[mesh_MeshRegion] := TriangulateMesh[mesh, MaxCellMeasure -> 1]

OrientableMeshRegionQ[mesh_MeshRegion] := Block[{
	d = RegionDimension[mesh], orientations
},
	orientations = AssociationThread[MeshCellIndex[mesh, d], CellOrientation /@ MeshCells[mesh, d]];
	AllTrue[
		Map[
			Lookup[orientations, Key[#]] & ,
			EdgeList @ MeshConnectivityGraph[mesh, d],
			{2}
		],
		MatchQ[x_ \[UndirectedEdge] y_ /; x != y]
	]
]


PathMesh[vs_List] := MeshRegion[
	GraphEmbedding[PathGraph[vs]], Line[vs],
	MeshCellStyle -> {0 -> PointSize[Large]}
]


Options[MoebiusMesh] = {"Radius" -> 1., "HalfWidth" -> .3};

MoebiusMesh[nu_Integer ? Positive, nv_Integer ? Positive, opts : OptionsPattern[]] := Block[{
    r = OptionValue["Radius"],
    w = OptionValue["HalfWidth"],
    u, v,
    idx, pts, tris
},
    (* parameter grids *)
    u = Subdivide[0, 2 Pi, nu];          (* length direction *)
    v = Subdivide[- w, w, nv];            (* half-width *)

    (* index of vertex (i,j) within the flattened list *)
    idx[i_, j_] := i (nv + 1) + j + 1;

    (* vertex coordinates *)
    pts = Catenate @ Table[
            With[{uu = u[[i + 1]], vv = v[[j + 1]]},
                {(r + vv Cos[uu / 2]) Cos[uu], (r + vv Cos[uu / 2]) Sin[uu], vv Sin[uu / 2]}
            ],
            {i, 0, nu},
            {j, 0, nv}
    ];

    (* triangles; the last strip (i=nu-1) is glued to i=0 with               *)
    (* the cross-section reversed (j -> nv-j) to realise the Möbius twist    *)
    tris = Flatten[
        Table[
            If[ i < nu,
                (* ordinary band *)
                {
                    {idx[i, j], idx[i + 1, j], idx[i + 1, j + 1]},
                    {idx[i, j], idx[i + 1, j + 1], idx[i, j + 1]}
                },
                (* wrap *)
                {                                                 
                    {idx[i, j], idx[0, nv - j], idx[0, nv - (j + 1)]},
                    {idx[i, j], idx[0, nv - (j + 1)], idx[i, j + 1]}
                }
            ],
            {i, 0, nu - 1},
            {j, 0, nv - 1}
        ],
        2
    ];

    MeshRegion[pts, Triangle /@ tris]
]


(* ===================== Unit-length discretization & embedding ===================== *)

(* The contact graph of a relaxed hard-sphere packing has all edges at exactly 2r:
   two touching spheres of radius r have centers at distance 2r by geometry, not by
   force balance.  UnitLengthGraph packs a region as given -- filling a solid, meshing a
   surface -- and returns that contact graph; UnitLengthEmbedding is the inverse, realising
   an abstract graph in R^d with every edge a unit segment (the iterative sibling of
   the declarative ComplexEmbedding). *)


(* one pass: every pair of centers closer than 2r is pushed apart to distance exactly 2r *)
hardSphereFixup[points_ ? MatrixQ, radius_] := Module[{disp = 0. points, neighbors},
	neighbors = Nearest[points -> "Index"][points, {Infinity, 2. radius}];
	Do[
		With[{v = points[[i]] - points[[j]], dist = EuclideanDistance[points[[i]], points[[j]]]},
			If[10.^-12 < dist < 2 radius,
				With[{delta = 0.5 (2 radius - dist) v / dist},
					disp[[i]] += delta; disp[[j]] -= delta
				]
			]
		],
		{i, Length[points]}, {j, Select[neighbors[[i]], # > i &]}
	];
	points + disp
]


(* near-uniform points on the unit 2-sphere: golden-angle azimuth, equal-area height *)
fibonacciSeed[n_Integer] := Table[
	With[{phi = N[Pi (3 - Sqrt[5])] i, z = 1. - 2. (i + 0.5) / n},
		With[{r = Sqrt[1. - z^2]}, {r Cos[phi], r Sin[phi], z}]
	],
	{i, 0, n - 1}
]


(* deterministic Fibonacci seed for a Sphere or ellipsoid shell; random sample inside/on any other region *)
unitLengthSeed[Sphere[c : {_, _, _} : {0, 0, 0}, r_ : 1], n_] := (c + r # & ) /@ fibonacciSeed[n]
unitLengthSeed[RegionBoundary[Ellipsoid[c : {_, _, _}, s : {_, _, _}]], n_] := (c + s # & ) /@ fibonacciSeed[n]
unitLengthSeed[region_, n_] := RandomPoint[region, n]


(* retraction of a point list back into/onto the region: project onto a Sphere, radially confine
   to a Ball/Ellipsoid, nearest-point otherwise (RegionNearest fills solids, projects surfaces) *)
packRetraction[Sphere[c : {_, _, _} : {0, 0, 0}, r_ : 1]] := pts |-> (c + r Normalize[# - c] & ) /@ pts
packRetraction[Ball[c : {_, _, _} : {0, 0, 0}, r_ : 1]] := pts |-> (If[EuclideanDistance[c, #] <= r, #, c + r Normalize[# - c]] & ) /@ pts
packRetraction[Ellipsoid[c : {_, _, _}, s : {_, _, _}]] := pts |-> (With[{q = (# - c) / s}, If[q . q <= 1, #, c + (# - c) / Sqrt[q . q]]] & ) /@ pts
packRetraction[RegionBoundary[Ellipsoid[c : {_, _, _}, s : {_, _, _}]]] := pts |-> (c + (# - c) / Sqrt[Total[((# - c) / s)^2]] & ) /@ pts
packRetraction[region_] := pts |-> RegionNearest[region, pts]


(* content (length / area / volume in the region's own dimension) used to calibrate the packing
   radius; analytic for the common heads, RegionMeasure otherwise *)
packMeasure[Sphere[_ : {0, 0, 0}, r_ : 1]] := 4. Pi r^2
packMeasure[Ball[_ : {0, 0, 0}, r_ : 1]] := 4. Pi r^3 / 3
packMeasure[Ellipsoid[_, {a_, b_, c_}]] := 4. Pi a b c / 3
packMeasure[RegionBoundary[Ellipsoid[_, {a_, b_, c_}]]] := With[{p = 1.6075}, 4. Pi ((a^p b^p + a^p c^p + b^p c^p) / 3)^(1 / p)]
packMeasure[region_] := RegionMeasure[region]


(* iterative projection: alternate hard-sphere fixup with a retraction onto the region *)
iterativeProjectionPack[seed_, radius_, retract_, step_, maxIter_, tol_] := Module[{points = N[seed], previous, iter = 0},
	While[iter < maxIter,
		previous = points;
		points = hardSphereFixup[points, radius];
		points = points + step (retract[points] - points);
		If[Max[Norm /@ (points - previous)] < tol, Break[]];
		iter++
	];
	points
]


(* declarative packing: minimize total squared region distance subject to |x_i - x_j| >= 2r;
   KKT activity makes the contact constraints tight, so contacts land at exactly 2r.  RegionDistance
   is zero inside a solid (so the points fill it) and is NumericQ-guarded so NMinimize only ever
   evaluates it on concrete points *)
constrainedPack[seed_, radius_, region_] := Module[{
	dim = Length[First[seed]], vars, dist
},
	dist[p : {__ ? NumericQ}] := RegionDistance[region, p];
	vars = Table[Unique["ulx"], Length[seed], dim];
	Partition[
		Flatten[vars] /. Last @ NMinimize[
			{
				Total[dist[#]^2 & /@ vars],
				And @@ Flatten @ Table[
					(vars[[i]] - vars[[j]]) . (vars[[i]] - vars[[j]]) >= (2 radius)^2,
					{i, Length[vars]}, {j, i + 1, Length[vars]}
				]
			},
			Flatten[vars]
		],
		dim
	]
]


(* edges between center pairs whose distance lies in the relative contact band 2r (1 +/- tol);
   tol is a fraction of 2r so the first neighbor shell is caught at any radius scale.  Carries
   the packing as coordinates *)
contactGraph[points_, radius_, tol_] := With[{nf = Nearest[points -> "Index"], band = 2 radius tol},
	Graph[
		Range[Length[points]],
		Flatten @ Table[
			UndirectedEdge[i, #] & /@ Select[
				nf[points[[i]], {Infinity, 2 radius + band}],
				# > i && Abs[EuclideanDistance[points[[i]], points[[#]]] - 2 radius] <= band &
			],
			{i, Length[points]}
		],
		VertexCoordinates -> points
	]
]


(* lengths of the contact edges -- an exactness probe for the relaxation *)
contactEdgeLengths[points_, radius_, tol_] :=
	EuclideanDistance[points[[#[[1]]]], points[[#[[2]]]]] & /@ (List @@@ EdgeList[contactGraph[points, radius, tol]])


Options[UnitLengthGraph] = {
	Method -> "IterativeProjection",
	"Radius" -> Automatic,
	"MaxIterations" -> 200,
	"Tolerance" -> 10.^-6,
	"ProjectionStep" -> 1.,
	"Overpack" -> 1.,
	"ContactTolerance" -> 0.25
};

(* unit-length graph of region: contact graph of a relaxed hard-sphere packing of n spheres in
   region, every edge length 2r; the packing is stored in VertexCoordinates.  The automatic radius
   spaces the spheres to tile the region's content C in its own dimension d (2r = C/n on a curve,
   Sqrt[2 C / (n Sqrt[3])] hexagonally on a surface, 1.12 (C/n)^(1/3) in a solid), so the packing
   jams into contacts; "Overpack" > 1 tightens the contact shell *)
UnitLengthGraph[region_ ? RegionQ, n_Integer, opts : OptionsPattern[]] := Module[{
	radius = OptionValue["Radius"],
	dim = RegionDimension[region],
	points
},
	radius = If[radius === Automatic,
		0.5 OptionValue["Overpack"] Switch[dim,
			1, packMeasure[region] / n,
			2, Sqrt[2 packMeasure[region] / (n Sqrt[3.])],
			_, 1.12 (packMeasure[region] / n)^(1 / 3)
		],
		N[radius]
	];
	points = Switch[OptionValue[Method],
		"ConstrainedPacking",
			constrainedPack[unitLengthSeed[region, n], radius, region],
		_,
			iterativeProjectionPack[
				unitLengthSeed[region, n], radius, packRetraction[region],
				OptionValue["ProjectionStep"], OptionValue["MaxIterations"], OptionValue["Tolerance"]
			]
	];
	contactGraph[points, radius, OptionValue["ContactTolerance"]]
]


Options[UnitLengthEmbedding] = {
	"Dimension" -> 3,
	"MaxIterations" -> 500,
	"Tolerance" -> 10.^-7,
	"NonEdgeRepulsion" -> 0.,
	"MaxStepPerVertex" -> 0.15,
	"InitialEmbedding" -> Automatic
};

(* embedding f : V -> R^d realising every edge as a unit segment, by edge-spring relaxation
   from a spring-electrical start; returns coordinates in VertexList order (cf. GraphEmbedding).
   The declarative counterpart is ComplexEmbedding *)
UnitLengthEmbedding[graph_ ? GraphQ, opts : OptionsPattern[]] := Module[{
	dim = OptionValue["Dimension"],
	maxIter = OptionValue["MaxIterations"],
	tol = OptionValue["Tolerance"],
	repulse = OptionValue["NonEdgeRepulsion"],
	maxStep = OptionValue["MaxStepPerVertex"],
	init = OptionValue["InitialEmbedding"],
	vlist = VertexList[graph], n, vIdx, edges, points, previous, d, iter = 0, norms
},
	n = Length[vlist];
	vIdx = AssociationThread[vlist -> Range[n]];
	edges = {vIdx[#[[1]]], vIdx[#[[2]]]} & /@ EdgeList[graph];
	points = N @ If[init === Automatic, GraphEmbedding[graph, "SpringElectricalEmbedding", dim], init];
	(* a flat 3D spring layout has no third-axis spread: perturb to break the degeneracy *)
	If[dim == 3 && Max[Abs[points[[All, 3]]]] < 10.^-6,
		points += RandomReal[{-0.01, 0.01}, Dimensions[points]]
	];
	(* rescale so the mean edge starts near length 1 *)
	With[{m = Mean[EuclideanDistance[points[[#[[1]]]], points[[#[[2]]]]] & /@ edges]},
		If[m > 10.^-12, points /= m]
	];
	While[iter < maxIter,
		previous = points;
		d = 0. points;
		Do[
			With[{e1 = edges[[k, 1]], e2 = edges[[k, 2]], v = points[[edges[[k, 1]]]] - points[[edges[[k, 2]]]]},
				With[{dist = Norm[v]},
					If[dist > 10.^-12,
						With[{delta = 0.5 (1. - dist) v / dist}, d[[e1]] += delta; d[[e2]] -= delta]
					]
				]
			],
			{k, Length[edges]}
		];
		If[repulse > 0., d += unitLengthRepulsion[points, edges, repulse]];
		(* clip per-vertex moves so long initial edges do not make the relaxation diverge *)
		norms = Norm /@ d;
		d = MapThread[If[#2 > maxStep, #1 (maxStep / #2), #1] &, {d, norms}];
		points += d;
		If[Max[Norm /@ (points - previous)] < tol, Break[]];
		iter++
	];
	points
]


(* soft repulsion between nearby non-adjacent pairs, preventing degenerate collapse *)
unitLengthRepulsion[points_, edges_, strength_] := Module[{
	n = Length[points], acc = 0. points,
	es = AssociationThread[Sort /@ edges -> True],
	neighbors = Nearest[points -> "Index"][points, {Infinity, 0.9}]
},
	Do[
		Do[
			If[j > i && ! KeyExistsQ[es, Sort[{i, j}]],
				With[{v = points[[i]] - points[[j]], dist = EuclideanDistance[points[[i]], points[[j]]]},
					If[10.^-12 < dist < 0.9,
						With[{delta = strength 0.5 (0.9 - dist) v / dist}, acc[[i]] += delta; acc[[j]] -= delta]
					]
				]
			],
			{j, neighbors[[i]]}
		],
		{i, n}
	];
	acc
]

