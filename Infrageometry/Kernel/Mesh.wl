Package["WolframInstitute`Infrageometry`"]


PackageExport[ComplexEmbedding]
PackageExport[ComplexMesh]
PackageExport[GraphMesh]
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


Options[ComplexEmbedding] = {"RepulsiveForcePower" -> 1.*^-2, "Scale" -> 1, "Epsilon" -> 1.*^-3}

ComplexEmbedding[g : {___List}, d : 2 | 3 : 3, OptionsPattern[]] := Block[{
	vs = ComplexVertexList[g], l, x = \[FormalX],
    rep = OptionValue["RepulsiveForcePower"], eps = OptionValue["Epsilon"], scale = OptionValue["Scale"],
    vars, energy, repulsion, constraints, func, sol
},
	l = Length[vs];
	vars = Catenate[Table[x[v, k], {v, vs}, {k, d}]];
	energy = Total @ MapApply[
		{a, b} |-> With[{r = Table[x[a, k] - x[b, k], {k, d}] },
			(Sqrt[r . r] - 1) ^ 2
		],
		ComplexClosure[SimplexList[g], {2}]
	];
	repulsion = rep * Total @ MapApply[
		{as, bs} |-> With[{r = Mean @ Table[x[a, k], {a, as}, {k, d}] - Mean @ Table[x[b, k], {b, bs}, {k, d}]},
			(Abs[Length[as] - Length[bs]] + 1) / (r . r + eps)
		],
		Subsets[g, {2}]
	];
	constraints = Append[
		Table[Sum[x[v, k], {v, vs}] == 0, {k, d}],
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
    coords = Replace[arg, d_Integer :> ComplexEmbedding[g, d, FilterRules[{opts}, Options[ComplexEmbedding]]]],
    simplices, faces
},
    ConfirmAssert[Length[coords] == SimplexCardinality[g, 0]];
    simplices = SimplexList[g, {1, Replace[arg, _List :> Max[Length /@ arg]]}];
    faces = ComplexClosure[Cases[simplices, {_, _, _, _}], {3}];
	MeshRegion[
        coords,
		Map[
            Switch[
                Length[#],
                2,
                    Style[Line[#], Directive[StandardGray, Thick]],
                3,
                    If[MemberQ[faces, #], Triangle[#], Style[Triangle[#], StandardBlue]],
                4,
                    Style[Tetrahedron[#], StandardRed]
            ] &,
            simplices
        ],
        FilterRules[{opts}, Options[MeshRegion]],
		MeshCellStyle -> {0 -> Directive[StandardGreen, PointSize[Large]]},
		MeshCellLabel -> {0 -> "Index"}
	]
]


GraphMesh[g_ ? GraphQ] := MeshRegion[
	GraphEmbedding[g],
	Join[Line /@ List @@@ EdgeList[g], Triangle /@ FindClique[g, {3}, All], Tetrahedron /@ FindClique[g, {4}, All]]
]


MeshComplex[mr_MeshRegion] := CanonicalComplex[Replace[Catenate[MeshCells[mr]], {Point[x_] :> {x}, _[x_] :> x}, 1]]


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

