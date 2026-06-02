Package["WolframInstitute`Infrageometry`"]

(* Topological Data Analysis helpers *)

PackageExport[VietorisRipsThresholdGraph]
PackageExport[VietorisRipsComplex]
PackageExport[VietorisRipsFiltration]
PackageExport[MiniballRadius]
PackageExport[BallIntersectionComplex]
PackageExport[CechComplex]
PackageExport[BallIntersectionFiltrationValue]
PackageExport[BallIntersectionFiltration]
PackageExport[CechFiltration]
PackageExport[BallIntersectionBifiltration]
PackageScope[commonRadiusFn]
PackageScope[metricMatrix]
PackageExport[BettiCurves]
PackageExport[BettiTable]
PackageExport[PersistentHomology]
PackageExport[PersistenceIntervals]
PackageExport[PersistenceDiagram]
PackageExport[PopularNetwork]
PackageExport[PopularNetworkNames]



Options[VietorisRipsThresholdGraph] = {"Metric" -> Automatic, "IncludeLoops" -> False, "VertexCoordinates" -> True};
VietorisRipsThresholdGraph[data : {__List}, r_ ? NumericQ, OptionsPattern[]] := Block[
    {metric, n, edges, coordsQ},
    n = Length[data];
    metric = Replace[OptionValue["Metric"], Automatic -> EuclideanDistance];
    coordsQ = TrueQ[OptionValue["VertexCoordinates"]];
    edges = Select[Subsets[Range[n], {2}], metric @@ data[[#]] <= r &];
    Graph[
        Range[n],
        UndirectedEdge @@@ edges,
        VertexCoordinates -> If[coordsQ, AssociationThread[Range[n] -> data], Automatic],
        GraphLayout -> If[coordsQ && (VectorQ[First[data], NumericQ] && Length[First[data]] <= 3), "LinearEmbedding", Automatic]
    ]
]

(* Graph form: the metric is the graph (shortest-path) distance, so the radius-r threshold graph is the r-th power G^r (weighted graphs use their edge weights). *)
VietorisRipsThresholdGraph[g_ ? GraphQ, r_ ? NumericQ] :=
    AdjacencyGraph[VertexList[g], Map[Boole[0 < # <= r] &, GraphDistanceMatrix[g], {2}]]


(* Vietoris–Rips complex via clique complex of threshold graph; optional dimension cap. *)
VietorisRipsComplex[data_List, r_ ? NumericQ, k : (_Integer | Infinity) : Infinity] := Block[{g},
    g = VietorisRipsThresholdGraph[data, r];
    GraphComplex[g, k]
]

(* Graph form: Vietoris–Rips complex of a graph at scale r = clique complex of its r-th power; at r = 1 this is GraphComplex[g]. *)
VietorisRipsComplex[g_ ? GraphQ, r_ ? NumericQ, k : (_Integer | Infinity) : Infinity] :=
    GraphComplex[VietorisRipsThresholdGraph[g, r], k]

(* Filtration: association radius -> complex. Radii will be sorted unless already monotone. *)
Options[VietorisRipsFiltration] = {"MaxDimension" -> Infinity, "Sort" -> True};
VietorisRipsFiltration[data_List, radii : {__ ? NumericQ}, OptionsPattern[]] := Block[{rs, k},
    rs = If[TrueQ[OptionValue["Sort"]], Sort[radii], radii];
    k = OptionValue["MaxDimension"];
    AssociationThread[rs -> (VietorisRipsComplex[data, #, k] & /@ rs)]
]

(* Betti curves: association radius -> BettiVector (possibly truncated to MaxDimension). *)
Options[BettiCurves] = {"MaxDimension" -> Automatic};
BettiCurves[data_List, radii : {__ ? NumericQ}, opts : OptionsPattern[]] := Block[{filtration, k},
    filtration = VietorisRipsFiltration[data, radii, "MaxDimension" -> OptionValue["MaxDimension"] /. Automatic -> Infinity];
    k = OptionValue["MaxDimension"] /. Automatic -> Infinity;
    Association @ KeyValueMap[(#1 -> With[{bv = SimplexCardinalities[#2]}, If[k === Infinity, bv, Take[bv, UpTo[k + 1]]]]) &, filtration]
]

(* Convert Betti curves association to a rectangular table (matrix) with radii rows. *)
BettiTable[data_List, radii : {__ ? NumericQ}, opts : OptionsPattern[BettiCurves]] := Block[{assoc, dims},
    assoc = BettiCurves[data, radii, FilterRules[{opts}, Options[BettiCurves]]];
    dims = If[assoc === <||>, 0, Max[Length /@ Values[assoc]]];
    <|
        "Radii" -> Keys[assoc],
        "Betti" -> If[dims == 0, {}, (PadRight[#, dims, 0] & /@ Values[assoc])],
        "Dimensions" -> If[dims == 0, {}, Range[0, dims - 1]]
    |>
]


(* Convenience upvalues allowing data associations keyed by indices *)
VietorisRipsThresholdGraph[data_Association, r_, opts : OptionsPattern[]] := VietorisRipsThresholdGraph[Values[data], r, opts]
VietorisRipsComplex[data_Association, r_, k : (_Integer | Infinity) : Infinity] := VietorisRipsComplex[Values[data], r, k]
VietorisRipsFiltration[data_Association, radii_List, opts : OptionsPattern[]] := VietorisRipsFiltration[Values[data], radii, opts]
BettiCurves[data_Association, radii_List, opts : OptionsPattern[]] := BettiCurves[Values[data], radii, opts]
BettiTable[data_Association, radii_List, opts : OptionsPattern[]] := BettiTable[Values[data], radii, opts]


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


(*** Persistent Homology (Z2 field) ***)

Options[PersistentHomology] = {"MaxDimension" -> Automatic};

(* Standard persistence: one global GF(2) column reduction over all simplices
   ordered by (birth, dim, simplex). A column reducing to empty marks its simplex
   POSITIVE (creates a class); a non-empty column marks it NEGATIVE (its pivot is
   the positive simplex it kills, a finite bar). Positive simplices never killed
   give infinite bars in their own dimension — including the top dimension.
   Zero-persistence bars (birth == death) are dropped. *)
Clear[PersistenceIntervals]
PersistenceIntervals[filtration_Association, OptionsPattern[PersistentHomology]] := Block[
    {maxDim, birthTime, dimOf, topDim, simplices, ordered, index, boundary, xor,
     reduced = <||>, low = <||>, killed, result = <||>},
    maxDim = OptionValue["MaxDimension"] /. Automatic :> Infinity;
    birthTime = <||>;
    Do[Scan[If[! KeyExistsQ[birthTime, #], birthTime[#] = r] &, filtration[r]], {r, Keys[filtration]}];
    dimOf[s_] := Length[s] - 1;
    topDim = Max[dimOf /@ Keys[birthTime], 0];
    simplices = Select[Keys[birthTime], dimOf[#] <= Min[maxDim + 1, topDim] &];
    ordered = SortBy[simplices, {birthTime[#], Length[#], #} &];
    index = AssociationThread[ordered -> Range[Length[ordered]]];
    boundary[s_] := Sort @ Lookup[index, Select[Subsets[s, {Length[s] - 1}], KeyExistsQ[index, #] &]];
    xor[a_, b_] := Sort @ Complement[Union[a, b], Intersection[a, b]];
    Do[
        With[{j = index[s]},
            reduced[j] = boundary[s];
            While[reduced[j] =!= {} && KeyExistsQ[low, Last[reduced[j]]], reduced[j] = xor[reduced[j], reduced[low[Last[reduced[j]]]]]];
            If[reduced[j] =!= {}, low[Last[reduced[j]]] = j]
        ],
        {s, ordered}
    ];
    killed = AssociationThread[Keys[low] -> True];
    Do[result[d] = {}, {d, 0, Min[maxDim /. Infinity -> topDim, topDim]}];
    Scan[
        With[{j = index[#], d = dimOf[#]},
            Which[
                reduced[j] === {} && ! KeyExistsQ[killed, j] && d <= maxDim,
                    result[d] = Append[result[d], {birthTime[#], Infinity}],
                reduced[j] =!= {},
                    With[{pivot = ordered[[Last[reduced[j]]]]},
                        If[birthTime[pivot] =!= birthTime[#] && dimOf[pivot] <= maxDim,
                            result[dimOf[pivot]] = Append[result[dimOf[pivot]], {birthTime[pivot], birthTime[#]}]]
                    ]
            ]
        ] &,
        ordered
    ];
    KeySort[result]
];

PersistentHomology[filtration_Association, opts : OptionsPattern[]] := PersistenceIntervals[filtration, opts]
PersistentHomology[data_List, radii_List, opts : OptionsPattern[]] := Block[{f},
    f = VietorisRipsFiltration[data, radii, "MaxDimension" -> (OptionValue["MaxDimension"] /. Automatic -> Infinity)];
    PersistentHomology[f, opts]
]

PersistenceDiagram[filtration_Association, opts : OptionsPattern[PersistentHomology]] := Block[{intervals},
    intervals = PersistentHomology[filtration, opts];
    Catenate @ KeyValueMap[Table[{dim, seq[[i, 1]], seq[[i, 2]]}, {i, Length[seq]}] &, intervals]
]

PersistenceDiagram[data_List, radii_List, opts : OptionsPattern[PersistentHomology]] :=
    PersistenceDiagram[VietorisRipsFiltration[data, radii, "MaxDimension" -> (OptionValue["MaxDimension"] /. Automatic -> Infinity)], opts]


(*** Popular networks utility ***)

$PopularNetworks := $PopularNetworks = <|
    (* Selected SNAP datasets: direct .txt.gz edge lists. NOTE: All treated as undirected here. *)
    "SNAP-Facebook" -> <|"URLs" -> {"https://snap.stanford.edu/data/facebook_combined.txt.gz"}, "Format" -> "EdgeList", "Description" -> "Aggregated Facebook ego-net (4,039 nodes, 88,234 edges).", "Source" -> "SNAP"|>,
    "SNAP-GrQc" -> <|"URLs" -> {"https://snap.stanford.edu/data/ca-GrQc.txt.gz"}, "Format" -> "EdgeList", "Description" -> "General Relativity & Quantum Cosmology co-authorship network.", "Source" -> "SNAP"|>,
    "SNAP-HepPh" -> <|"URLs" -> {"https://snap.stanford.edu/data/ca-HepPh.txt.gz"}, "Format" -> "EdgeList", "Description" -> "High Energy Physics (Phenomenology) co-authorship network.", "Source" -> "SNAP"|>,
    "SNAP-AstroPh" -> <|"URLs" -> {"https://snap.stanford.edu/data/ca-AstroPh.txt.gz"}, "Format" -> "EdgeList", "Description" -> "Astro Physics arXiv co-authorship network.", "Source" -> "SNAP"|>,
    "SNAP-CondMat" -> <|"URLs" -> {"https://snap.stanford.edu/data/ca-CondMat.txt.gz"}, "Format" -> "EdgeList", "Description" -> "Condensed Matter arXiv co-authorship network.", "Source" -> "SNAP"|>,
    "SNAP-EmailEnron" -> <|"URLs" -> {"https://snap.stanford.edu/data/email-Enron.txt.gz"}, "Format" -> "EdgeList", "Description" -> "Enron email communication (direction ignored).", "Source" -> "SNAP"|>,
    "SNAP-EmailEuCore" -> <|"URLs" -> {"https://snap.stanford.edu/data/email-Eu-core.txt.gz"}, "Format" -> "EdgeList", "Description" -> "EU research institution email network (direction ignored).", "Source" -> "SNAP"|>,
    "SNAP-WikiVote" -> <|"URLs" -> {"https://snap.stanford.edu/data/wiki-Vote.txt.gz"}, "Format" -> "EdgeList", "Description" -> "Wikipedia adminship vote network (direction ignored).", "Source" -> "SNAP"|>,
    "SNAP-GoogleWeb" -> <|"URLs" -> {"https://snap.stanford.edu/data/web-Google.txt.gz"}, "Format" -> "EdgeList", "Description" -> "Google 2002 web graph (directed edges treated undirected).", "Source" -> "SNAP"|>,
    "SNAP-RoadCA" -> <|"URLs" -> {"https://snap.stanford.edu/data/roadNet-CA.txt.gz"}, "Format" -> "EdgeList", "Description" -> "California road network (large; >2M edges).", "Source" -> "SNAP"|>,
    "SNAP-RoadPA" -> <|"URLs" -> {"https://snap.stanford.edu/data/roadNet-PA.txt.gz"}, "Format" -> "EdgeList", "Description" -> "Pennsylvania road network (large).", "Source" -> "SNAP"|>,
    "SNAP-RoadTX" -> <|"URLs" -> {"https://snap.stanford.edu/data/roadNet-TX.txt.gz"}, "Format" -> "EdgeList", "Description" -> "Texas road network (large).", "Source" -> "SNAP"|>,
    "SNAP-LiveJournal1" -> <|"URLs" -> {"https://snap.stanford.edu/data/soc-LiveJournal1.txt.gz"}, "Format" -> "EdgeList", "Description" -> "LiveJournal social network (very large; ~69M edges).", "Source" -> "SNAP"|>,
    "SNAP-BitcoinOTC" -> <|"URLs" -> {"https://snap.stanford.edu/data/soc-sign-bitcoin-otc.txt.gz"}, "Format" -> "EdgeList", "Description" -> "Bitcoin OTC trust network (signed; signs discarded).", "Source" -> "SNAP"|>,
    (* Non-SNAP classic small GML datasets with stable hosts (Netzschleuder provides direct format endpoints). *)
    "CElegansNeural" -> <|"URLs" -> {"https://raw.githubusercontent.com/eflegara/Network-Science-Lectures/master/datasets/celegansneural.gml"}, "Format" -> "GML", "Description" -> "C. elegans chemical+electrical synapse connectome (~297 neurons).", "Source" -> "White et al. 1986; Newman; Network-Science-Lectures"|>,
    "NetScience" -> <|"URLs" -> {"https://raw.githubusercontent.com/eflegara/Network-Science-Lectures/master/datasets/netscience.gml"}, "Format" -> "GML", "Description" -> "Network science co-authorship (subset; ~379 nodes).", "Source" -> "Newman; Network-Science-Lectures"|>,
    "Dolphins" -> <|"URLs" -> {"https://raw.githubusercontent.com/eflegara/Network-Science-Lectures/master/datasets/dolphins.gml"}, "Format" -> "GML", "Description" -> "Dolphin social network (Lusseau et al.).", "Source" -> "Lusseau; Network-Science-Lectures"|>,
    "LesMiserables" -> <|"URLs" -> {"https://raw.githubusercontent.com/eflegara/Network-Science-Lectures/master/datasets/lesmis.gml"}, "Format" -> "GML", "Description" -> "Les Misérables character co-occurrence network.", "Source" -> "Knuth / Newman; Network-Science-Lectures"|>,
    "PolBlogs" -> <|"URLs" -> {"https://raw.githubusercontent.com/eflegara/Network-Science-Lectures/master/datasets/polblogs%20copy.gml"}, "Format" -> "GML", "Description" -> "US political blogs hyperlink network (2004).", "Source" -> "Adamic & Glance 2005; Network-Science-Lectures"|>,
    "PowerGrid" -> <|"URLs" -> {"https://raw.githubusercontent.com/eflegara/Network-Science-Lectures/master/datasets/power.gml"}, "Format" -> "GML", "Description" -> "Western US power grid network.", "Source" -> "Watts & Strogatz 1998; Network-Science-Lectures"|>,
    "WordNet" -> <|"URLs" -> {"https://raw.githubusercontent.com/eflegara/Network-Science-Lectures/master/datasets/word-net.gml"}, "Format" -> "GML", "Description" -> "Word adjacency (word-net) network.", "Source" -> "Newman; Network-Science-Lectures"|>,
    "Yeast" -> <|"URLs" -> {"https://raw.githubusercontent.com/eflegara/Network-Science-Lectures/master/datasets/Yeast.paj"}, "Format" -> "Pajek", "Description" -> "Yeast protein interaction network (Pajek format).", "Source" -> "Network-Science-Lectures"|>,
    "Jazz" -> <|"URLs" -> {"https://raw.githubusercontent.com/eflegara/Network-Science-Lectures/master/datasets/jazz.net"}, "Format" -> "Pajek", "Description" -> "Jazz musicians collaboration network.", "Source" -> "Gleiser & Danon; Network-Science-Lectures"|>,
    "HeroNetwork" -> <|"URLs" -> {"https://raw.githubusercontent.com/eflegara/Network-Science-Lectures/master/datasets/hero-network.csv"}, "Format" -> "CSV", "Description" -> "Hero network (comic/book social network) from repository CSV.", "Source" -> "Network-Science-Lectures"|>
|>;

PopularNetworkNames[] := Block[{names = Keys[$PopularNetworks], ex = {}},
    Quiet @ Check[ex = ExampleData[{"NetworkGraph"}]; Null];
    Complement[names, Flatten[List[ex]]]
]

(* Simple cache so repeated downloads are avoided *)
If[! ValueQ[$PopularNetworkCache], $PopularNetworkCache = <||>];

(* what can request the actual Graph, or just metadata fields. Default is "Graph". *)
(* NOTE: Format "Graphlet" (legacy entries may still use "GML") refers to Graph Modeling Language .gml files, not Geography Markup Language. *)
PopularNetwork[name_String, what : ("Graph" | "Description" | "Source" | All) : "Graph", opts : OptionsPattern[Graph]] := Block[
    {meta = Lookup[$PopularNetworks, name, None], urls, fmt, desc, src, g, loader, csvLoader, importGraph, genericLoader},
    If[meta === None, Return[$Failed]];
    urls = Lookup[meta, "URLs", {}];
    fmt  = Lookup[meta, "Format", "EdgeList"];
    desc = Lookup[meta, "Description", name];
    src  = Lookup[meta, "Source", ""];

     (* Edge list (.txt) loader. *)
    edgeLoader[u_] := Block[{cache = Lookup[$PopularNetworkCache, Key[{name, u}], None], lines, edges},
        If[cache =!= None, Return[Graph[cache, opts]]];
        lines = Quiet @ Check[Import[u, "Lines"], {}];
        lines = Select[lines, (StringTrim[#] =!= "" && ! StringStartsQ[StringTrim[#], ("#" | "%" | "//")]) &];
        edges = StringSplit /@ lines;
        edges = Select[edges, Length[#] >= 2 &];
        edges = UndirectedEdge @@@ (Take[#, 2] & /@ edges);
        If[edges === {}, Return[$Failed]];
        $PopularNetworkCache[{name, u}] = edges;
        Graph[edges, opts]
    ];

    importGraph[type_][u_] := Block[{cache = Lookup[$PopularNetworkCache, Key[{name, u}], None], g},
        If[cache =!= None, Return[cache /; GraphQ[cache]]];
        g = Quiet @ Check[Import[u, type], $Failed];
        If[! GraphQ[g], Return[$Failed]];
        $PopularNetworkCache[{name, u}] = g; g
    ];

    csvLoader[u_] := Block[
        {cache = Lookup[$PopularNetworkCache, Key[{name, u}], None], data, rows, edges},
        If[cache =!= None, Return[Graph[cache, opts]]];
        data = Quiet @ Check[Import[u, "CSV"], {}];
        If[data === {} || ! ListQ[data], Return[$Failed]];
        rows = Select[data, (ListQ[#] && Length[#] >= 2) &];
        If[rows === {}, Return[$Failed]];
        edges = UndirectedEdge @@@ rows;
        $PopularNetworkCache[{name, u}] = edges;
        Graph[edges, opts]
    ];

    genericLoader[u_] := Block[{cache = Lookup[$PopularNetworkCache, Key[{name, u}], None], g3},
        If[cache =!= None, Return[cache /; GraphQ[cache]]];
        g3 = Quiet @ Check[Import[u], $Failed];
        If[GraphQ[g3], $PopularNetworkCache[{name, u}] = g3; g3, $Failed]
    ];

    loader = Switch[
        fmt,
        "EdgeList", edgeLoader,
        "CSV", csvLoader,
        "Graphlet" | "GML", importGraph["Graphlet"],
        "Pajek", importGraph["Pajek"],
        _, genericLoader
    ];

    g = FirstCase[urls, u_ :> With[{r = loader[u]}, If[GraphQ[r], r, Nothing]], $Failed];
    If[g === $Failed, Return[$Failed]];
    Which[
        what === "Graph", g,
        what === "Description", desc,
        what === "Source", src,
        what === All, <|"Graph" -> g, "Description" -> desc, "Source" -> src|>,
        True, $Failed
    ]
]

