Package["WolframInstitute`Infrageometry`"]


PackageExport[VectorFieldCommutator]
PackageExport[VectorFieldInverse]



VectorFieldCommutator[g_ ? GraphQ, x_Association, y_Association, p_, n : _Integer | _UpTo | All | Automatic : Automatic] /; VertexQ[g, p] :=
    selectEndpoints[g, p, commutatorEndpoints[g, x, y, VectorFieldInverse[g, x], VectorFieldInverse[g, y], p], n]

VectorFieldCommutator[g_ ? GraphQ, x_Association, y_Association, n : _Integer | _UpTo | All | Automatic : Automatic] :=
    With[{xInv = VectorFieldInverse[g, x], yInv = VectorFieldInverse[g, y]},
        AssociationMap[p |-> selectEndpoints[g, p, commutatorEndpoints[g, x, y, xInv, yInv, p], n], VertexList[g]]
    ]


VectorFieldInverse[g_ ? GraphQ, x_Association] :=
    Merge[Catenate[(p |-> Thread[vectorFieldValues[g, x, p] -> p]) /@ Keys[x]], Sort]


commutatorEndpoints[g_, x_, y_, xInv_, yInv_, p_] :=
    Fold[
        {points, step} |-> Catenate[step /@ points],
        {p},
        {
            q |-> vectorFieldValues[g, x, q],
            q |-> vectorFieldValues[g, y, q],
            q |-> Lookup[xInv, Key[q], {}],
            q |-> Lookup[yInv, Key[q], {}]
        }
    ]

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
