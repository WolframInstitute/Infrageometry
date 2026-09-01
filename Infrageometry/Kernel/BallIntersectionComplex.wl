Package["WolframInstitute`Infrageometry`"]

PackageExport[MiniballRadius]
PackageExport[BallIntersectionComplex]
PackageExport[CechComplex]
PackageExport[BallIntersectionFiltrationValue]
PackageExport[BallIntersectionFiltration]
PackageExport[CechFiltration]
PackageExport[BallIntersectionBifiltration]
PackageScope[commonRadiusFn]
PackageScope[metricMatrix]


(*** Order-k ball-intersection complexes (Vietoris-Rips <-> Cech) ***)

(* Convention: closed balls B(x, r), equal radii. Two meet iff d(x_i, x_j) <= 2 r,
   so BallIntersectionComplex[data, r, 2] = VietorisRipsComplex[data, 2 r]. For equal
   radii a common intersection point exists iff the smallest enclosing ball of the
   centres has radius <= r, so the Cech filtration value is the miniball radius. *)

(* radius of the smallest ball containing the points *)
MiniballRadius[pts_List] := BoundingRegion[N @ pts, "MinBall"][[2]]

(* sigma admitted iff every k-subset of its balls has a common point;
   k = 2 is Vietoris-Rips (pairwise), k = Infinity is Cech (full nerve). *)
Options[BallIntersectionComplex] = {"Metric" -> Automatic, "IntersectionTest" -> Automatic, "MaxDimension" -> Infinity};
BallIntersectionComplex[data_List, r_ ? NumericQ, k : (_Integer | Infinity) : Infinity, OptionsPattern[]] :=
    Module[
        {n = Length[data], metric, itest, maxDim, radius, qualify, admitted, prev, prevQ, m, cands},
        metric = Replace[OptionValue["Metric"], Automatic -> EuclideanDistance];
        itest = OptionValue["IntersectionTest"];
        maxDim = OptionValue["MaxDimension"];
        radius = commonRadiusFn[data, metric];
        qualify = If[itest === Automatic,
            s |-> radius[s] <= r,
            s |-> TrueQ[itest[RegionIntersection @@ (Ball[data[[#]], r] & /@ s)]]
        ];
        admitted = {List /@ Range[n]};
        m = 2;
        While[Last[admitted] =!= {} && m <= maxDim + 1,
            prev = Last[admitted];
            prevQ = AssociationThread[prev -> True];
            cands = DeleteDuplicates @ Map[Sort,
                Catenate[(s |-> (Append[s, #] & /@ Complement[Range[n], s])) /@ prev]
            ];
            cands = Select[cands, AllTrue[Subsets[#, {m - 1}], KeyExistsQ[prevQ, #] &] &];
            AppendTo[admitted, If[m <= k, Select[cands, qualify], cands]];
            m++
        ];
        Catenate[admitted]
    ]

(* the nerve: a simplex iff all its balls share a common point *)
CechComplex[data_List, r_ ? NumericQ, opts : OptionsPattern[BallIntersectionComplex]] :=
    BallIntersectionComplex[data, r, Infinity, opts]

(* radius at which the |s| balls first acquire a common point: miniball of the
   centres (Euclidean) or, over a finite metric, the smallest r for which some
   sample point lies within r of every centre (intrinsic intersection oracle). *)
commonRadiusFn[data_, EuclideanDistance] := s |-> MiniballRadius[data[[s]]]
(* graph metric: data are centre vertices (any subset of V); a common point may be
   ANY vertex, so candidates range over all of V while columns track the centres. *)
commonRadiusFn[data_, g_ ? GraphQ] :=
    With[{rows = GraphDistanceMatrix[g][[ Flatten[FirstPosition[VertexList[g], #] & /@ data] ]]},
        s |-> Min[Max /@ Transpose[rows[[s]]]]]
commonRadiusFn[data_, metric_] := With[{mat = metricMatrix[data, metric]}, s |-> Min[Max /@ mat[[All, s]]]]

metricMatrix[data_, m_ ? MatrixQ] := m
metricMatrix[data_, g_ ? GraphQ] := GraphDistanceMatrix[g]
metricMatrix[data_, f_] := Outer[f, data, data, 1]

(* birth radius of sigma in C^(k): max miniball over its k-subsets (its own
   miniball when |sigma| <= k), monotone under faces. *)
Options[BallIntersectionFiltrationValue] = {"Metric" -> Automatic};
BallIntersectionFiltrationValue[data_List, sigma_List, k : (_Integer | Infinity) : Infinity, OptionsPattern[]] :=
    With[{radius = commonRadiusFn[data, Replace[OptionValue["Metric"], Automatic -> EuclideanDistance]]},
        If[Length[sigma] <= k, radius[sigma], Max[radius /@ Subsets[sigma, {k}]]]
    ]

(* association r -> C^(k)_r over the (sorted) radii, ready for PersistenceIntervals *)
BallIntersectionFiltration[data_List, radii : {__ ? NumericQ}, k : (_Integer | Infinity) : Infinity, opts : OptionsPattern[BallIntersectionComplex]] :=
    With[{rs = Sort[radii]}, AssociationThread[rs -> (BallIntersectionComplex[data, #, k, opts] & /@ rs)]]

CechFiltration[data_List, radii : {__ ? NumericQ}, opts : OptionsPattern[BallIntersectionComplex]] :=
    BallIntersectionFiltration[data, radii, Infinity, opts]

(* the (r, k) object: association k -> (association r -> complex). For fixed r the
   nesting C^(k) contains C^(k+1) runs as k grows, saturating to Cech at k = d + 1
   (Helly) when the balls are convex. *)
BallIntersectionBifiltration[data_List, radii : {__ ? NumericQ}, orders : {__}, opts : OptionsPattern[BallIntersectionComplex]] :=
    AssociationMap[BallIntersectionFiltration[data, radii, #, opts] &, orders]

(* index-keyed data associations *)
BallIntersectionComplex[data_Association, r_, k : (_Integer | Infinity) : Infinity, opts : OptionsPattern[]] := BallIntersectionComplex[Values[data], r, k, opts]
CechComplex[data_Association, r_, opts : OptionsPattern[]] := CechComplex[Values[data], r, opts]
BallIntersectionFiltration[data_Association, radii_List, k : (_Integer | Infinity) : Infinity, opts : OptionsPattern[]] := BallIntersectionFiltration[Values[data], radii, k, opts]
