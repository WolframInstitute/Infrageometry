Package["WolframInstitute`Infrageometry`"]

(* Topological Data Analysis helpers *)

PackageExport[VietorisRipsThresholdGraph]
PackageExport[VietorisRipsComplex]
PackageExport[VietorisRipsFiltration]
PackageExport[BettiCurves]
PackageExport[BettiTable]
PackageExport[PersistentHomology]
PackageExport[PersistenceIntervals]
PackageExport[PersistenceDiagram]
PackageExport[PopularNetwork]
PackageExport[PopularNetworkNames]
PackageExport[PopularHypergraph]
PackageExport[PopularHypergraphNames]


Options[VietorisRipsThresholdGraph] = {"Metric" -> Automatic, "IncludeLoops" -> False, "VertexCoordinates" -> True};
VietorisRipsThresholdGraph[data : {__List}, r_ ? NumericQ, OptionsPattern[]] := Block[
    {metric, n, edges, coordsQ},
    n = Length[data];
    metric = Replace[OptionValue["Metric"], Automatic -> EuclideanDistance];
    coordsQ = TrueQ[OptionValue["VertexCoordinates"]];
    edges = Select[Subsets[Range[n], {2}], metric @@ (data[[#]]&) <= r &];
    Graph[
        Range[n],
        UndirectedEdge @@@ edges,
        VertexCoordinates -> If[coordsQ, AssociationThread[Range[n] -> data], Automatic],
        GraphLayout -> If[coordsQ && (VectorQ[First[data], NumericQ] && Length[First[data]] <= 3), "LinearEmbedding", Automatic]
    ]
]


(* Vietoris–Rips complex via clique complex of threshold graph; optional dimension cap. *)
VietorisRipsComplex[data_List, r_ ? NumericQ, k : (_Integer | Infinity) : Infinity] := Block[{g},
    g = VietorisRipsThresholdGraph[data, r];
    GraphComplex[g, k]
]

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
    Association @ KeyValueMap[(#1 -> With[{bv = BettiVector[#2]}, If[k === Infinity, bv, Take[bv, UpTo[k + 1]]]]) &, filtration]
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


(*** Persistent Homology (Z2 field) ***)

Options[PersistentHomology] = {"MaxDimension" -> Automatic};

(* Core reduction over GF(2) returning pairings for one dimension step. *)
Clear[PersistenceIntervals]
PersistenceIntervals[filtration_Association, OptionsPattern[PersistentHomology]] := Block[
    {maxDim, radii, birthTime, simplicesByDim, result = <||>, allSimplices, dims, orderKey, reduceDimension},
    radii = Keys[filtration];
    maxDim = OptionValue["MaxDimension"] /. Automatic :> Infinity;
    birthTime = Association[];
    Do[Scan[(If[! KeyExistsQ[birthTime, #], birthTime[#] = r]) &, filtration[r]], {r, radii}];
    allSimplices = Keys[birthTime];
    simplicesByDim = GroupBy[allSimplices, Length[#] - 1 &];
    dims = Sort[Keys[simplicesByDim]];
    orderKey[s_] := {birthTime[s], Length[s], s};
    reduceDimension[d_] := Block[{rows, cols, rowIndex, low = <||>, intervals = {}, colFaces, col, pivot},
        If[d == 0, Return[Null]];
        rows = SortBy[Lookup[simplicesByDim, d - 1, {}], orderKey];
        cols = SortBy[Lookup[simplicesByDim, d, {}], orderKey];
        rowIndex = AssociationThread[rows -> Range[Length[rows]]];
        colFaces[s_] := Sort @ Map[rowIndex, Select[Subsets[s, {Length[s] - 1}], KeyExistsQ[rowIndex, #] &]];
        Do[
            col = colFaces[c];
            While[col =!= {} && KeyExistsQ[low, Last[col]],
                col = Sort @ Complement[Union[col, low[Last[col]]], Intersection[col, low[Last[col]]]];
            ];
            If[col =!= {},
                pivot = Last[col];
                AppendTo[intervals, {birthTime[rows[[pivot]]], birthTime[c]}];
                low[pivot] = col;
            ];
            , {c, cols}];
        If[rows =!= {},
            With[{pairedRows = Keys[low]},
                Do[If[! MemberQ[pairedRows, i], AppendTo[result[d - 1], {birthTime[rows[[i]]], Infinity}]], {i, Length[rows]}]
            ]
        ];
        If[intervals =!= {}, result[d - 1] = Join[Lookup[result, d - 1, {}], intervals]];
    ];
    Scan[If[maxDim =!= Infinity && # > maxDim + 1, Nothing, reduceDimension[#]] &, dims];
    Do[If[! KeyExistsQ[result, d], result[d] = {}], {d, 0, Min[maxDim /. Infinity -> 0, Max[Append[dims, 0]]]}];
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


(*** Popular hypergraphs utility (native higher-order datasets) ***)

$PopularHypergraphs := $PopularHypergraphs = <|
    "FIM-Mushroom" -> <|"URLs" -> {"http://fimi.uantwerpen.be/data/mushroom.dat"}, "Format" -> "Transaction", "Description" -> "Mushroom dataset; each transaction treated as a hyperedge over discrete item ids.", "Source" -> "FIMI Repository"|>,
    "FIM-Chess" -> <|"URLs" -> {"http://fimi.uantwerpen.be/data/chess.dat"}, "Format" -> "Transaction", "Description" -> "Chess endgame (KRK) positions; each record as itemset hyperedge.", "Source" -> "FIMI Repository"|>,
    "FIM-Retail" -> <|"URLs" -> {"http://fimi.uantwerpen.be/data/retail.dat"}, "Format" -> "Transaction", "Description" -> "Retail market-basket data (sparse, many distinct items).", "Source" -> "FIMI Repository"|>,
    "FIM-T10I4D100K" -> <|"URLs" -> {"http://fimi.uantwerpen.be/data/T10I4D100K.dat"}, "Format" -> "Transaction", "Description" -> "Synthetic sparse transactions (T10I4D100K).", "Source" -> "FIMI Repository"|>,
    "FIM-T40I10D100K" -> <|"URLs" -> {"http://fimi.uantwerpen.be/data/T40I10D100K.dat"}, "Format" -> "Transaction", "Description" -> "Synthetic denser transactions (T40I10D100K).", "Source" -> "FIMI Repository"|>,
    "FIM-Accidents" -> <|"URLs" -> {"http://fimi.uantwerpen.be/data/accidents.dat"}, "Format" -> "Transaction", "Description" -> "Traffic accidents attribute itemsets (large).", "Source" -> "FIMI Repository"|>,
    "FIM-Kosarak" -> <|"URLs" -> {"http://fimi.uantwerpen.be/data/kosarak.dat"}, "Format" -> "Transaction", "Description" -> "Kosarak (Hungarian online news portal click-stream).", "Source" -> "FIMI Repository"|>,
    "FIM-Pumsb" -> <|"URLs" -> {"http://fimi.uantwerpen.be/data/pumsb.dat"}, "Format" -> "Transaction", "Description" -> "US Census (PUMS) derived itemset encoding (pumsb).", "Source" -> "FIMI Repository"|>,
    "FIM-PumsbStar" -> <|"URLs" -> {"http://fimi.uantwerpen.be/data/pumsb_star.dat"}, "Format" -> "Transaction", "Description" -> "US Census (PUMS) condensed variant (pumsb*).", "Source" -> "FIMI Repository"|>,
    "FIM-BMSWebView1" -> <|"URLs" -> {"http://fimi.uantwerpen.be/data/BMS-WebView-1.dat"}, "Format" -> "Transaction", "Description" -> "Click-stream sessions (BMS-WebView-1).", "Source" -> "FIMI Repository"|>,
    "FIM-BMSWebView2" -> <|"URLs" -> {"http://fimi.uantwerpen.be/data/BMS-WebView-2.dat"}, "Format" -> "Transaction", "Description" -> "Click-stream sessions (BMS-WebView-2).", "Source" -> "FIMI Repository"|>,
    "FIM-Connect" -> <|"URLs" -> {"http://fimi.uantwerpen.be/data/connect.dat"}, "Format" -> "Transaction", "Description" -> "Connect-4 game positions as itemsets (large).", "Source" -> "FIMI Repository"|>
|>;

PopularHypergraphNames[] := Keys[$PopularHypergraphs]

If[! ValueQ[$PopularHypergraphCache], $PopularHypergraphCache = <||>];

(* Options could be extended later; placeholder for symmetry. *)
Options[PopularHypergraph] = {};
PopularHypergraph[name_String, what : ("Hypergraph" | "Description" | "Source" | "IncidenceGraph" | "2SectionGraph" | All) : "Hypergraph", OptionsPattern[]] := Block[
    {meta = Lookup[$PopularHypergraphs, name, None], urls, fmt, desc, src, hg, loader, transactionLoader, incidenceGraph, twoSectionGraph},
    If[meta === None, Return[$Failed]];
    urls = Lookup[meta, "URLs", {}];
    fmt = Lookup[meta, "Format", "Transaction"];
    desc = Lookup[meta, "Description", name];
    src = Lookup[meta, "Source", ""];

    transactionLoader[u_] := Block[{cache = Lookup[$PopularHypergraphCache, Key[{name, u}], None], lines, edges},
        If[cache =!= None, Return[cache]];
        lines = Quiet @ Check[Import[u, "Lines"], {}];
        lines = Select[lines, StringTrim[#] =!= "" &];
        edges = Select[# =!= "" &] @* StringSplit @* StringTrim /@ lines;
        $PopularHypergraphCache[{name, u}] = edges;
        edges
    ];

    loader = Switch[fmt,
        "Transaction", transactionLoader,
        _, (Return[$Failed] &)
    ];

    hg = FirstCase[urls, u_ :> With[{r = loader[u]}, If[ListQ[r] && VectorQ[r, ListQ], r, Nothing]], $Failed];
    If[hg === $Failed, Return[$Failed]];

    incidenceGraph[edges_List] := Block[{items, itemVertices, edgeVertices, vItems, vEdges},
        items = Union[Flatten[edges]];
        itemVertices = Thread[items -> ("v" <> ToString /@ Range[Length[items]])];
        (* Represent hyperedges as indexed symbols H1, H2, ... to avoid collision. *)
        edgeVertices = Table[Unique["H"], {Length[edges]}];
        vItems = Values[itemVertices];
        vEdges = edgeVertices;
        Graph[Join[vItems, vEdges],
            Flatten @ MapThread[(With[{ev = #2}, UndirectedEdge[#, ev] & /@ (itemVertices /@ #1)]) &, {edges, vEdges}],
            GraphLayout -> "BipartiteEmbedding"
        ]
    ];

    twoSectionGraph[edges_List] := Block[{pairs},
        pairs = DeleteDuplicates[Sort /@ Catenate[Subsets[#, {2}] & /@ Select[edges, Length[#] >= 2 &]]];
        Graph[UndirectedEdge @@@ pairs]
    ];

    Which[
        what === "Hypergraph", hg,
        what === "Description", desc,
        what === "Source", src,
        what === "IncidenceGraph", incidenceGraph[hg],
        what === "2SectionGraph", twoSectionGraph[hg],
        what === All, <|"Hypergraph" -> hg, "Description" -> desc, "Source" -> src|>,
        True, $Failed
    ]
];


