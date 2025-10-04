Package["WolframInstitute`Infrageometry`"]

(* Hypergraph / transaction dataset utilities and analysis helpers *)

PackageExport[PopularHypergraph]
PackageExport[PopularHypergraphNames]

PackageExport[HypergraphVertexSet]
PackageExport[HypergraphVertexCount]
PackageExport[HyperedgeCount]
PackageExport[HyperedgeSizes]
PackageExport[HyperedgeSizeDistribution]
PackageExport[HypergraphDegree]
PackageExport[HypergraphMaximalEdges]
PackageExport[HypergraphComplex]
PackageExport[HypergraphLineGraph]
PackageExport[Hypergraph2Section]
PackageExport[Weighted2SectionGraph]
PackageExport[HypergraphSummary]


(* Registry of FIM transaction datasets *)
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
If[! ValueQ[$PopularHypergraphCache], $PopularHypergraphCache = <||>]

Options[PopularHypergraph] = {}
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
        edgeVertices = Table[Unique["H"], {Length[edges]}];
        vItems = Values[itemVertices]; vEdges = edgeVertices;
        Graph[Join[vItems, vEdges],
            Flatten @ MapThread[(With[{ev = #2}, UndirectedEdge[#, ev] & /@ (itemVertices /@ #1)]) &, {edges, vEdges}],
            GraphLayout -> "BipartiteEmbedding"]
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
        True, $Failed]
]

(* Core hypergraph utility functions *)
HypergraphVertexSet[edges_List] := Union[Flatten[edges]]
HypergraphVertexCount[edges_List] := Length[HypergraphVertexSet[edges]]
HyperedgeCount[edges_List] := Length[edges]
HyperedgeSizes[edges_List] := Length /@ edges
HyperedgeSizeDistribution[edges_List] := Tally[HyperedgeSizes[edges]]
HypergraphDegree[edges_List] := Counts[Flatten[DeleteDuplicates /@ edges]]
HypergraphMaximalEdges[edges_List] := Module[{u = DeleteDuplicates[edges]}, Select[u, Function[ed, ! AnyTrue[DeleteCases[u, ed], SubsetQ[#, ed] &]]]]
HypergraphComplex[edges_List] := DeleteDuplicates @ Catenate[Subsets[#, {1, All}] & /@ edges]
HypergraphLineGraph[edges_List] := Module[{u = Range[Length[edges]], ints}, ints = Select[Subsets[u, {2}], Intersection @@ (edges[[#]]&) =!= {} &]; Graph[UndirectedEdge @@@ ints]]
Hypergraph2Section[edges_List] := Module[{pairs}, pairs = DeleteDuplicates @ Sort /@ Catenate[Subsets[#, {2}] & /@ Select[edges, Length[#] >= 2 &]]; Graph[UndirectedEdge @@@ pairs]]
Weighted2SectionGraph[edges_List, minSupport_ : 1] := Module[{assoc = <||>}, Do[If[Length[ed] >= 2, Scan[(assoc[#] = Lookup[assoc, #, 0] + 1) &, Subsets[ed, {2}]]], {ed, edges}];
    With[{kept = Select[assoc, # >= minSupport &]}, Graph[UndirectedEdge @@@ Keys[kept], EdgeWeight -> Values[kept]]]]
HypergraphSummary[edges_List] := Module[{v = HypergraphVertexSet[edges], sizes = HyperedgeSizes[edges], deg = HypergraphDegree[edges]}, <|
    "VertexCount" -> Length[v],
    "HyperedgeCount" -> Length[edges],
    "Vertices" -> v,
    "SizeStats" -> If[sizes === {}, <||>, <|"Min" -> Min[sizes], "Max" -> Max[sizes], "Mean" -> N @ Mean[sizes], "Median" -> Median[sizes]|>],
    "AvgDegree" -> If[deg === <||>, 0., N @ Mean[Values[deg]]],
    "DegreeDistribution" -> Normal[deg]
|>]

