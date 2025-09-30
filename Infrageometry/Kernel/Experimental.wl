Package["WolframInstitute`Infrageometry`"]

(* Experimental / provisional API. Subject to change. *)

PackageExport[FaceVector]
PackageExport[HVector]
PackageExport[GVector]
PackageExport[LinkComplex]
PackageExport[InducedSubcomplex]
PackageExport[VertexDeletion]
PackageExport[ComplexUnion]
PackageExport[ComplexIntersection]
PackageExport[ComplexDifference]
PackageExport[PureComplexQ]
PackageExport[RandomSimplicialComplex]
PackageExport[RandomFlagComplex]
PackageExport[FacetGraph]
PackageExport[BettiAssociation]
PackageExport[EulerBettiConsistencyQ]
PackageExport[EnumerateComplexes]


(*** Convenience Vectors ***)

FaceVector[g : {___List}] := SimplexCardinalities[g]

HVector[g : {___List}] := Module[{f = Prepend[FaceVector[g], 1], d, h},
    d = Length[f] - 1;
    h = Table[
        Sum[(-1)^(k - i) Binomial[d - i, k - i] f[[i + 1]], {i, 0, k}],
        {k, 0, d}
    ];
    h
]

GVector[g : {___List}] := Module[{h = HVector[g]}, Table[h[[i + 1]] - h[[i]], {i, 1, Floor[(Length[h] - 1)/2]}]]

BettiAssociation[g : {___List}] := AssociationThread[Range[0, ComplexDimension[g]] -> BettiVector[g]]

EulerBettiConsistencyQ[g : {___List}] := Module[{e1, e2},
    e1 = ComplexEulerCharacteristic[g];
    e2 = Total[(-1) ^ Range[0, ComplexDimension[g]] * BettiVector[g]];
    TrueQ[e1 == e2]
]

(*** Enumeration ***)

(* Simpler, robust enumeration of all complexes on a vertex set up to a maximum dimension.      *)
(* Strategy: list candidate simplices (non-empty) up to size d+1, ordered by nondecreasing size. *)
(* Recursively decide to include or skip each simplex, including only if all of its proper      *)
(* non-empty faces are already present, guaranteeing downward closure.                         *)

Options[EnumerateComplexes] = {"MaxCount" -> Infinity, "FullVertexSet" -> True};
EnumerateComplexes::large = "Enumeration may be very large (|V|=`` , d=``). Use option MaxCount to limit results.";

EnumerateComplexes[n_Integer ? NonNegative, args___] := EnumerateComplexes[Range[n], args]

EnumerateComplexes[verts_List, d_Integer : Infinity, OptionsPattern[]] := Module[
    {max = OptionValue["MaxCount"], full = TrueQ[OptionValue["FullVertexSet"]], maxDim = d,
     candidates, res = {}, count = 0, facesCache, base, dimLimit},
    If[verts === {}, Return[{{}}]];
    If[!(maxDim === Infinity || (IntegerQ[maxDim] && maxDim >= 0)), Return[$Failed]];
    dimLimit = If[maxDim === Infinity, Length[verts], maxDim + 1];
    (* If full vertex set required, pre-include all singletons and only enumerate size >= 2 simplices. *)
    base = If[full, List /@ verts, {}];
    candidates = SortBy[
        Subsets[verts, {If[full, 2, 1], dimLimit}],
        {Length[#], #} &
    ];
    If[Length[verts] > 7 && maxDim === Infinity && max === Infinity,
        Message[EnumerateComplexes::large, Length[verts], maxDim]
    ];
    facesCache = AssociationMap[
        If[Length[#] > 1, Subsets[#, {1, Length[#] - 1}], {}] &,
        candidates
    ];
    Clear[step];
    step[k_, current_] := Which[
        count >= max, Null,
        k > Length[candidates], (count++; AppendTo[res, current]),
        True,
            Module[{s = candidates[[k]], faces = facesCache[candidates[[k]]]},
                (* Skip branch *)
                step[k + 1, current];
                (* Include branch if closure condition satisfied *)
                If[SubsetQ[current, faces], step[k + 1, Append[current, s]]]
            ]
    ];
    step[1, base];
    SortBy[ComplexClosure /@ res, {Length, Identity}]
]

(*** Local operations ***)

LinkComplex[g : {___List}, s_List] := Module[{f = Select[g, SubsetQ[#, s] &]},
    ComplexClosure[Select[Complement[#, s] & /@ f, Intersection[#, s] === {} &]]
]

InducedSubcomplex[g : {___List}, verts : {___}] := ComplexClosure[Select[g, SubsetQ[verts, #] &]]

VertexDeletion[g : {___List}, v_] := InducedSubcomplex[g, Complement[ComplexVertexList[g], {v}]]

(*** Set-theoretic ops ***)

ComplexUnion[a : {___List}, b : {___List}] := ComplexClosure[Join[a, b]]
ComplexIntersection[a : {___List}, b : {___List}] := Select[a, MemberQ[b, #] &]
ComplexDifference[a : {___List}, b : {___List}] := ComplexClosure[Select[a, ! MemberQ[b, #] &]]

PureComplexQ[g : {___List}] := With[{dims = SimplexDimension /@ ComplexFacets[g]}, Length @ DeleteDuplicates[dims] == 1]

(*** Random models ***)

(* Linial–Meshulam style random complex: start with all vertices; attempt each (k+1)-subset with prob p given its boundary present. *)
RandomSimplicialComplex[n_Integer ? Positive, p_ ? NumericQ, maxDim_ : 3] := Module[
    {verts = Range @ n, simplices, dim},
    simplices = {{}}; (* include empty simplex for closure convenience *)
    simplices = Join[simplices, List /@ verts];
    For[dim = 1, dim <= maxDim, dim++,
        Do[
            If[RandomReal[] <= p && SubsetQ[simplices, Subsets[candidate, {dim}]],
                AppendTo[simplices, Sort[candidate]]
            ],
            {candidate, Subsets[verts, {dim + 1}]}
        ];
    ];
    ComplexClosure[simplices]
]

(* Random flag complex via Erdős–Rényi G(n,p) then clique expansion. *)
RandomFlagComplex[n_Integer ? Positive, p_ ? NumericQ, cliqueDim_ : Infinity] := Module[
    {edges, graph},
    edges = Select[Subsets[Range @ n, {2}], RandomReal[] <= p &];
    graph = Graph[UndirectedEdge @@@ edges];
    GraphComplex[graph, cliqueDim]
]

(*** Facet graph: dual graph of facets adjacency by ridge intersection ***)
FacetGraph[g : {___List}, opts : OptionsPattern[Graph]] := Module[{facets = ComplexFacets[g], pairs, adjQ},
    adjQ[a_, b_] := (
        SimplexDimension[a] == SimplexDimension[b] &&
        SimplexDimension[a] > 0 &&
        Length[Intersection[a, b]] == SimplexDimension[a]
    );
    pairs = Select[Subsets[Range[Length[facets]], {2}], adjQ[facets[[#[[1]]]], facets[[#[[2]]]]] &];
    Graph[Range[Length[facets]], UndirectedEdge @@@ pairs, opts]
]
