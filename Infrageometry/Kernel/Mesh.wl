Package["WolframInstitute`Infrageometry`"]


PackageExport[ComplexEmbedding]
PackageExport[ComplexMesh]


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
