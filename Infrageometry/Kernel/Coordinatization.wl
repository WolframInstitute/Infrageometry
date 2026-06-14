Package["WolframInstitute`Infrageometry`"]

(* Metric coordinatization and covering of graphs:
   landmark (radar) coordinates and resolving sets, the resistance-matching
   spectral embedding, and minimum ball covers (r-domination). *)

PackageExport[RadarCoordinates]
PackageExport[ResolvingSetQ]
PackageExport[FindResolvingSet]
PackageExport[MetricDimension]
PackageExport[ResistanceCoordinates]
PackageExport[FindBallCover]
PackageExport[BallCoverQ]
PackageExport[DominationNumber]
PackageScope[resistanceEmbeddingMatrix]


(* ===================== Radar coordinates & resolving sets ===================== *)

(* distance vector of v to the landmark set b: (d(v, b_i))_i *)
RadarCoordinates[g_Graph, b_List, v_] /; MemberQ[VertexList[g], v] :=
    GraphDistance[g, v, #] & /@ b

(* association vertex -> radar coordinates over all of g *)
RadarCoordinates[g_Graph, b_List] :=
    AssociationThread[VertexList[g], Outer[GraphDistance[g, #1, #2] &, VertexList[g], b, 1]]

(* b resolves g iff the radar map v |-> (d(v, b_i))_i is injective over V(g) *)
ResolvingSetQ[g_Graph, b_List] :=
    DuplicateFreeQ[Outer[GraphDistance[g, #1, #2] &, VertexList[g], b, 1]]

(* up to n resolving sets (metric bases) by ascending size; m restricts the
   candidate sizes (All, an integer max, {min, max}, or {exact}). Subsets are
   enumerated in size-then-Gosper order so the first found is smallest. *)
FindResolvingSet[g_Graph, n_Integer : 1, m_ : All] :=
    Module[{v = VertexList[g], dm = GraphDistanceMatrix[g], vc = VertexCount[g], found = {}, mask, last},
        Map[v[[#]] &,
            Catch[
                Scan[
                    k |-> (
                        mask = 2^k - 1;
                        last = BitShiftLeft[2^k - 1, vc - k];
                        While[mask <= last,
                            With[{s = Pick[Range[vc], IntegerDigits[mask, 2, vc], 1]},
                                If[DuplicateFreeQ[dm[[All, s]]],
                                    AppendTo[found, s];
                                    If[Length[found] >= n, Throw[found]]
                                ]
                            ];
                            (* Gosper's hack: next k-subset bitmask in lex order. *)
                            mask = With[{c = BitAnd[mask, -mask]}, {r = mask + c},
                                BitOr[r, Quotient[BitXor[r, mask], 4 c]]]
                        ]
                    ),
                    Replace[m, {All :> Range[vc], _Integer :> Range[m], {min_, max_} :> Range[min, max], {num_} :> {num}}]
                ];
                Throw[found]
            ]
        ]
    ]

(* metric dimension: size of a smallest resolving set *)
MetricDimension[g_Graph] := Length @ First @ FindResolvingSet[g, 1, All]


(* ===================== Resistance coordinates ===================== *)

Options[ResistanceCoordinates] = {"Rescaling" -> "ResistanceMatching", "Dimension" -> Automatic, "Origin" -> None};

(* spectral embedding Phi with ||Phi(u) - Phi(v)||^2 == EffectiveResistance(u, v)
   (Klein-Randic).  "Rescaling" -> "None" gives plain Laplacian eigenvectors,
   "Diffusion" -> t the diffusion-map embedding; "Origin" -> v recentres on v. *)
ResistanceCoordinates[g_Graph, opts : OptionsPattern[]] :=
    With[{mat = resistanceEmbeddingMatrix[g, OptionValue["Rescaling"], OptionValue["Dimension"]], origin = OptionValue["Origin"]},
        With[{originVec = If[origin === None, ConstantArray[0., Length @ First @ mat], mat[[ First @ FirstPosition[VertexList[g], origin] ]]]},
            AssociationThread[VertexList[g], # - originVec & /@ mat]
        ]
    ]

(* coordinates of a single vertex *)
ResistanceCoordinates[g_Graph, v_, opts : OptionsPattern[]] /; MemberQ[VertexList[g], v] :=
    ResistanceCoordinates[g, opts][v]

resistanceEmbeddingMatrix[g_Graph, rescaling_, dimSpec_] :=
    With[{es = Eigensystem[N @ Normal @ KirchhoffMatrix[g]]},
        {ord = Ordering[es[[1]]]},
        {vals = es[[1, ord]], vecs = es[[2, ord]]},
        {keep = Select[Range @ Length @ vals, vals[[#]] > 10^-10 Max[Abs @ vals, 1] &]},
        {idx = Take[keep, Replace[dimSpec, {Automatic | All :> Length[keep], UpTo[k_Integer] :> Min[k, Length[keep]], k_Integer :> Min[k, Length[keep]]}]]},
        {weights = Replace[rescaling, {"ResistanceMatching" :> 1 / Sqrt[vals[[idx]]], "None" :> ConstantArray[1, Length[idx]], ("Diffusion" -> t_) :> Exp[-t vals[[idx]]]}]},
        Transpose[weights vecs[[idx]]]
    ]


(* ===================== Ball covers & domination ===================== *)

(* a minimum r-ball cover: a smallest centre set (chosen from all of V) whose radius-r balls
   cover the targets (every vertex by default, or a given subset) as a set-cover integer program.
   count = 1 (default) returns one cover as a centre list; n / UpTo[n] return up to n distinct
   minimum covers; All returns every one. Enumerating all minimum covers is #P-hard (the count of
   minimum set covers), so All / n>1 brute-force the size-k centre subsets and are cheap only for small g. *)
FindBallCover[g_Graph, r_ : 1, targets : (_List | All) : All, count : (_Integer | All | UpTo[_Integer]) : 1] :=
    With[
        {vs = VertexList[g]},
        {rows = If[targets === All, Range[Length[vs]], Flatten[FirstPosition[vs, #] & /@ targets]]},
        {cover = Map[Boole[# <= r] &, GraphDistanceMatrix[g][[rows]], {2}], x = Array[\[FormalX], Length[vs]]},
        {one = vs[[ Flatten @ Position[Round[x /. LinearOptimization[Total[x], Join[Thread[cover . x >= 1], Thread[0 <= x <= 1]], x \[Element] Vectors[Length[vs], Integers]]], 1] ]]},
        If[count === 1, one,
            With[{covers = Select[Subsets[vs, {Length[one]}], BallCoverQ[g, r, #, targets] &]},
                Replace[count, {All -> covers, (n_Integer | UpTo[n_]) :> Take[covers, UpTo[n]]}]
            ]
        ]
    ]

(* do the radius-r balls around the centres s cover the targets (all of V by default)? *)
BallCoverQ[g_Graph, r_, s_List, targets : (_List | All) : All] :=
    With[
        {vs = VertexList[g], dm = GraphDistanceMatrix[g]},
        {pos = Flatten[FirstPosition[vs, #] & /@ s], rows = If[targets === All, dm, dm[[Flatten[FirstPosition[vs, #] & /@ targets]]]]},
        AllTrue[rows, row |-> AnyTrue[pos, j |-> row[[j]] <= r]]
    ]

(* r-domination number: size of a minimum r-ball cover (of the targets) *)
DominationNumber[g_Graph, r_ : 1, targets : (_List | All) : All] := Length @ FindBallCover[g, r, targets]
