Package["WolframInstitute`Infrageometry`"]

(* Differential forms on the tangent fibers of a graph vs. cochains on its clique
   complex, and the maps R (restriction) and I (integration) between them.
   Forms:    <|v -> <|{w1<...<wk} -> c|>|>   -- alternating germ on tuples of neighbours of v (sparse).
   Cochains: <|{v0<...<vk} -> c|>            -- alternating function on (k+1)-cliques (sparse).
   Antisymmetry is carried by Signature at lookup time; only sorted representatives are stored. *)

PackageExport[FormValue]
PackageExport[CochainValue]
PackageExport[FormDegree]
PackageExport[CochainDegree]
PackageExport[ZeroForm]
PackageExport[RestrictionMap]
PackageExport[IntegrationMap]
PackageExport[Coboundary]
PackageExport[FormDifferential]
PackageExport[NaiveDifferential]
PackageExport[FormWedge]
PackageExport[CochainCup]
PackageExport[AntisymmetrizedCup]

(* ===================== Evaluation ===================== *)

(* value of the germ omega_v on an arbitrary tuple of neighbours, alternating *)
FormValue[omega_, v_, tuple_List] := Signature[tuple] * Lookup[Lookup[omega, Key[v], <||>], Key[Sort[tuple]], 0]

(* value of a cochain on an arbitrary vertex tuple, alternating, 0 off the complex *)
CochainValue[alpha_, tuple_List] := Signature[tuple] * Lookup[alpha, Key[Sort[tuple]], 0]

(* k = form / cochain degree, read off a nonempty stored key *)
FormDegree[omega_] := Length @ First @ Keys @ First @ Select[Values[omega], # =!= <||> &]
CochainDegree[alpha_] := Length[First[Keys[alpha]]] - 1

(* the zero form of any degree stores no nonzero germ; sparsity makes its degree ambiguous *)
zeroFormQ[omega_] := AllTrue[Values[omega], # === <||> &]

(* ===================== Restriction and integration ===================== *)

(* canonical 0-form from a scalar function or association on the vertices *)
ZeroForm[g_, f_] := AssociationMap[v |-> <|{} -> f[v]|>, VertexList[g]]

(* R : cochain -> form, (R alpha)_v(w1..wk) = alpha(v, w1..wk); zero off cliques *)
RestrictionMap[g_, alpha_] := GroupBy[
    Flatten @ Table[
        With[{face = DeleteCases[clique, v]}, v -> (face -> CochainValue[alpha, Prepend[face, v]])],
        {clique, Keys[alpha]}, {v, clique}
    ],
    First -> Last,
    DeleteCases[0] @* Association
]

(* I : form -> cochain, (I omega)(v0..vk) = 1/(k+1) sum_i (-1)^i omega_{v_i}(v0..^vi..vk) *)
IntegrationMap[g_, omega_] := If[zeroFormQ[omega], <||>, With[{k = FormDegree[omega]},
    DeleteCases[0] @ Association @ Map[
        clique |-> clique -> Sum[(-1)^(i - 1) FormValue[omega, clique[[i]], Delete[clique, i]], {i, k + 1}] / (k + 1),
        cliqueSimplices[g, k]
    ]
]]

(* ===================== Differentials ===================== *)

(* coboundary delta on cochains, (delta alpha)(v0..v_{k+1}) = sum_i (-1)^i alpha(v0..^vi..v_{k+1}) *)
Coboundary[g_, alpha_] := If[alpha === <||>, <||>, With[{k = CochainDegree[alpha]},
    DeleteCases[0] @ Association @ Map[
        clique |-> clique -> Sum[(-1)^(i - 1) Lookup[alpha, Key[Delete[clique, i]], 0], {i, k + 2}],
        cliqueSimplices[g, k + 1]
    ]
]]

(* exterior derivative d on forms: gradient on 0-forms, corrected d on 1-forms *)
FormDifferential[g_, omega_] := Which[zeroFormQ[omega], <||>, FormDegree[omega] == 0, gradientForm[g, omega], True, oneFormDifferential[g, omega]]

(* (d f)_v(w) = f(w) - f(v) *)
gradientForm[g_, omega_] := AssociationMap[
    v |-> DeleteCases[0] @ Association @ Map[w |-> {w} -> FormValue[omega, w, {}] - FormValue[omega, v, {}], AdjacencyList[g, v]],
    VertexList[g]
]

(* (d omega)_v(w1,w2) = omega_v(w1) - omega_v(w2) + 1/2 [omega_{w1}(w2) - omega_{w2}(w1)] *)
oneFormDifferential[g_, omega_] := AssociationMap[
    v |-> DeleteCases[0] @ Association @ Map[
        pair |-> pair -> FormValue[omega, v, {pair[[1]]}] - FormValue[omega, v, {pair[[2]]}] +
            (FormValue[omega, pair[[1]], {pair[[2]]}] - FormValue[omega, pair[[2]], {pair[[1]]}]) / 2,
        Subsets[Sort @ AdjacencyList[g, v], {2}]
    ],
    VertexList[g]
]

(* naive differential on 1-forms, (d_naive omega)_v(w1,w2) = omega_v(w1) - omega_v(w2) (opposite face dropped) *)
NaiveDifferential[g_, omega_] := If[zeroFormQ[omega], <||>, AssociationMap[
    v |-> DeleteCases[0] @ Association @ Map[
        pair |-> pair -> FormValue[omega, v, {pair[[1]]}] - FormValue[omega, v, {pair[[2]]}],
        Subsets[Sort @ AdjacencyList[g, v], {2}]
    ],
    VertexList[g]
]]

(* ===================== Products ===================== *)

(* wedge product of forms, the exterior product on each Lambda(T_v G)^* (a shuffle sum) *)
FormWedge[omega_, eta_] := If[zeroFormQ[omega] || zeroFormQ[eta], <||>,
    AssociationMap[v |-> germWedge[Lookup[omega, Key[v], <||>], Lookup[eta, Key[v], <||>]], Intersection[Keys[omega], Keys[eta]]]
]

germWedge[a_, b_] := DeleteCases[0] @ Merge[
    Flatten @ Table[
        If[DisjointQ[s1, s2], Union[s1, s2] -> Signature[Join[s1, s2]] Lookup[a, Key[s1], 0] Lookup[b, Key[s2], 0], Nothing],
        {s1, Keys[a]}, {s2, Keys[b]}
    ],
    Total
]

(* Alexander-Whitney cup product, (alpha ^ beta)(v0..v_{p+q}) = alpha(v0..vp) beta(vp..v_{p+q}) *)
CochainCup[g_, alpha_, beta_] := If[alpha === <||> || beta === <||>, <||>, With[{p = CochainDegree[alpha], q = CochainDegree[beta]},
    DeleteCases[0] @ Association @ Map[
        clique |-> clique -> Lookup[alpha, Key[Take[clique, p + 1]], 0] Lookup[beta, Key[Take[clique, -(q + 1)]], 0],
        cliqueSimplices[g, p + q]
    ]
]]

(* graded-commutative cup: the full antisymmetrisation of the Alexander-Whitney product *)
AntisymmetrizedCup[g_, alpha_, beta_] := If[alpha === <||> || beta === <||>, <||>, With[{p = CochainDegree[alpha], q = CochainDegree[beta]},
    DeleteCases[0] @ Association @ Map[
        clique |-> clique -> Sum[
            Signature[perm] CochainValue[alpha, Take[perm, p + 1]] CochainValue[beta, Take[perm, -(q + 1)]],
            {perm, Permutations[clique]}
        ] / (p + q + 1)!,
        cliqueSimplices[g, p + q]
    ]
]]

(* k-simplices of the clique complex as sorted vertex tuples *)
cliqueSimplices[g_, k_] := Union[Sort /@ GraphComplex[g, {k + 1}]]
