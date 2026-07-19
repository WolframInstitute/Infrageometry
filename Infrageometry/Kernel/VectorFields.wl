Package["WolframInstitute`Infrageometry`"]


PackageExport[VectorFieldCommutator]
PackageExport[VectorFieldSum]
PackageExport[VectorFieldDifference]
PackageExport[VectorFieldInverse]
PackageExport[GradientVectorField]
PackageExport[RotationVectorField]
PackageExport[VectorFieldPlot]



Scan[
    Apply[{f, steps} |-> (
        f[g_ ? GraphQ, x_Association, y_Association, p_, n : _Integer | _UpTo | All | Automatic : Automatic] /; VertexQ[g, p] :=
            selectEndpoints[g, p, transportEndpoints[steps[g, x, y], p], n];
        f[g_ ? GraphQ, x_Association, y_Association, n : _Integer | _UpTo | All | Automatic : Automatic] :=
            With[{fixedSteps = steps[g, x, y]},
                AssociationMap[p |-> selectEndpoints[g, p, transportEndpoints[fixedSteps, p], n], VertexList[g]]
            ]
    )],
    {
        {VectorFieldCommutator, commutatorSteps},
        {VectorFieldSum, sumSteps},
        {VectorFieldDifference, differenceSteps}
    }
]


VectorFieldInverse[g_ ? GraphQ, x_Association] :=
    Merge[Catenate[(p |-> Thread[vectorFieldValues[g, x, p] -> p]) /@ Keys[x]], Sort]


GradientVectorField[g_ ? GraphQ, v_] /; VertexQ[g, v] :=
    With[{dist = AssociationThread[VertexList[g], GraphDistance[g, v]]},
        AssociationMap[
            p |-> If[p === v || ! IntegerQ[dist[p]], p, First @ MinimalBy[AdjacencyList[g, p], dist]],
            VertexList[g]
        ]
    ]

RotationVectorField[g_ ? GraphQ, c_] /; VertexQ[g, c] :=
    With[{pos = AssociationThread[VertexList[g], GraphEmbedding[g]]},
        AssociationMap[
            p |-> With[{u = {pos[c][[2]] - pos[p][[2]], pos[p][[1]] - pos[c][[1]]}},
                If[Norm[u] < 1.*^-6, p,
                    With[{q = First @ MaximalBy[AdjacencyList[g, p], w |-> Normalize[pos[w] - pos[p]] . Normalize[u]]},
                        If[Normalize[pos[q] - pos[p]] . Normalize[u] < 0.5, p, q]
                    ]
                ]
            ],
            VertexList[g]
        ]
    ]


Options[VectorFieldPlot] = Options[Graph]

VectorFieldPlot[g_ ? GraphQ, x_Association, opts : OptionsPattern[]] := VectorFieldPlot[g, {x -> Red}, opts]

VectorFieldPlot[g_ ? GraphQ, fields : {__Rule}, opts : OptionsPattern[]] :=
    With[{pos = AssociationThread[VertexList[g], GraphEmbedding[g]]},
        Show[
            Graph[g, opts, VertexSize -> Small, VertexStyle -> GrayLevel[0.5], EdgeStyle -> GrayLevel[0.87]],
            Graphics[{Arrowheads[0.025],
                (field |-> {field[[2]],
                    Catenate[
                        (p |-> (v |-> If[v === p, {PointSize[0.012], Point[pos[p]]}, Arrow[{pos[p], pos[v]}]]) /@ vectorFieldValues[g, field[[1]], p]) /@
                            Keys[field[[1]]]
                    ]}) /@ fields}]
        ]
    ]


commutatorSteps[g_, x_, y_] := {forwardStep[g, x], forwardStep[g, y], inverseStep[g, x], inverseStep[g, y]}

sumSteps[g_, x_, y_] := {forwardStep[g, y], forwardStep[g, x]}

differenceSteps[g_, x_, y_] := {inverseStep[g, y], forwardStep[g, x]}

transportEndpoints[steps_List, p_] := Fold[{points, step} |-> Catenate[step /@ points], {p}, steps]

forwardStep[g_, x_] := q |-> vectorFieldValues[g, x, q]

inverseStep[g_, x_] := With[{inv = VectorFieldInverse[g, x]}, q |-> Lookup[inv, Key[q], {}]]

selectEndpoints[g_, p_, points_, n_] :=
    With[{sorted = SortBy[DeleteDuplicates[points], AssociationThread[VertexList[g], GraphDistance[g, p]]]},
        Replace[n, {
            Automatic :> First[sorted, Missing["NotFound"]],
            All :> sorted,
            UpTo[k_Integer] | k_Integer :> Take[sorted, UpTo[k]]
        }]
    ]

vectorFieldValues[g_, x_Association, p_] :=
    Replace[Lookup[x, Key[p]], {_Missing -> {}, v_List /; ! VertexQ[g, v] :> v, v_ :> {v}}]
