Package["WolframInstitute`Infrageometry`"]


PackageExport[VectorFieldCommutator]
PackageExport[VectorFieldSum]
PackageExport[VectorFieldDifference]
PackageExport[VectorFieldInverse]



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
