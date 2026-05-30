(* ::Package:: *)
Package["WolframInstitute`Infrageometry`"]

PackageExport["SimplicialData"]
PackageExport["SimplicialSet"]
PackageExport["SimplicialSetQ"]
PackageExport["DeltaComplexQ"]
PackageExport["SimplicialComplexQ"]

(* ::Section:: *)
(* Simplicial set *)

SimplicialData::invalid = "Invalid input data";

Options[SimplicialSet] = {"IncludeDegeneracies" -> True, "FlattenMultiplicities" -> True};

(* SimplicialData is determined by a collection of face maps
        { d_j^k: X_k -> X_(k-1) | j=0,...,k; k=1,...,d }
   and degeneracy maps
        { s_j^k: X_k -> X_(k+1) | j=0,...,k; k=0,...,d-1 }
   represented by sparse arrays and organized by the dimension k=1,...,d as follows:
        - faceBlocks: { {d^1_0, d^1_1}, {d^2_0, d^2_1, d^2_2}, ..., {d^d_0, ..., d^d_d}}
        - degeneracyBlocks: {{s^0_0}, {s^1_0, s^1_1}, ..., {s^(d-1)_0, ..., s^(d-1)_(d-1)}}
   SimplicialData constructor only checks that the provided face maps have the correct dimensions
        (n_0, n_1), (n_1, n_2), ..., (n_(d-1), n_d),
   and similarly for degeneracy maps with the dimensions transposed.
    *)
SimplicialData[faceBlocks_List, degeneracyBlocks_List] :=
  Module[ {dimensions},
    (* Handle empty case *)
    If[faceBlocks === {} && degeneracyBlocks === {},
      Return[SimplicialData[{}, {}, {}]]
    ];

    (* Compute dimensions *) 
    dimensions = Prepend[Last /@ #, First @ First @ #] &@ Map[Dimensions @* First] @ faceBlocks;

    (* Check that blocks have correct dimensions *)
    If[ Not @ And[ Length[degeneracyBlocks] == Length[dimensions] - 1,
      AllTrue[ MapIndexed[Rule, Transpose[{faceBlocks, degeneracyBlocks}]],
        With[{k = Last[#], faces = First @ First[#], degeneracies = Last @ First[#]}, And[
          Length[faces] == k+1,
          AllTrue[faces, Dimensions[#] == {dimensions[[k]], dimensions[[k + 1]]} &],
          Length[degeneracies] == k,
          AllTrue[degeneracies, Dimensions[#] == {dimensions[[k+1]], dimensions[[k]]} &]
      ]]& ]],
      Message[SimplicialData::invalid]; 
      Return[$Failed];
    ];

    SimplicialData[dimensions, faceBlocks, degeneracyBlocks]
  ]

(* A list of (degenerate) simplices is completed into a simplicial set as follows:
     - Consequent vertex multiplicites are treated as degeneracies unless IncludeDegeneracies -> False.
     - Simplex multiplicities lead to multi-valued face and degeneracy maps unless FlattenMultiplicities -> True. *)
SimplicialSet[input: {___List}, OptionsPattern[]] := 
  Module[{filteredInput, givenSimplices, d, faceAndDegeneracyBlocks, faceBlocks, degeneracyBlocks, includeDegeneracies, flattenMultiplicities},
    (* Filter out empty subsets *)
    filteredInput = DeleteCases[input, {}];
    
    (* Early exit if nothing is left *)
    If[filteredInput === {},
      Return[SimplicialData[{}, {}, {}]]
    ];
    
    includeDegeneracies = OptionValue["IncludeDegeneracies"];
    flattenMultiplicities = OptionValue["FlattenMultiplicities"];

    (* Label given simplices and group them by their dimension:
        { d -> <| simplex1 -> {1}, simplex2 -> {2}, ... |>, d-1 -> ... } *)
    givenSimplices = Normal[
      Merge[Catenate] @* MapIndexed[Rule] /@
        KeySortBy[Minus]@
          Merge[{#, AssociationMap[Function[x, {}], Range[0, Max[Keys[#], 0]]]}, First] &@
            KeySelect[# >= 0 &]@
              GroupBy[(Length[#] - 1) &]@
                If[flattenMultiplicities, DeleteDuplicates[filteredInput], filteredInput]
    ];

    (* Handle edge cases: no simplices or only 0-simplices *)
    d = Length[givenSimplices];
    Which[
      d == 0, 
        Return[SimplicialData[{}, {}, {}]],
      d == 1,
        Return[SimplicialData[{Max[Values[Last[First[givenSimplices]]]]}, {}, {}]]
    ];

    (* Construct face and degeneracy matrices by folding over pairs of dimensions (d, d-1)
       and inductively completing the Delta-set by adding induced boundaries. *)
    faceAndDegeneracyBlocks = FoldPairList[determineFaceAndDegeneracyBlocks[#1, #2, includeDegeneracies] &, givenSimplices];

    (* { { d^1_0, d^1_1 }, ..., { d^d_0,...,d^d_(d+1) } } *) 
    faceBlocks = Reverse @ faceAndDegeneracyBlocks[[All,1]]; 
    (* { { s^0_0 }, ..., { s^(d-1)_0,...,s^(d-1)_(d-1) } } *)
    degeneracyBlocks = Reverse @ faceAndDegeneracyBlocks[[All,2]];

    SimplicialData[faceBlocks, degeneracyBlocks]
  ]

determineFaceAndDegeneracyBlocks[simplices_, faces_, includeDegeneracies_: True] := 
  Module[{labeledSimplices, k, labeledFaces, maxFaceLabel, maxDeclaredFaceLabel, maxSimplexLabel, boundaryRules},
    k = First[simplices]; (* Simplex dimension *)
    labeledSimplices = Last[simplices]; (* Labeled k-simplices *)
    labeledFaces = Last[faces];  (* Labeled (k-1)-simplices (faces) *)
    If[Length[labeledSimplices] === 0, Return[{{}, {}, k - 1 -> labeledFaces}]];
    maxSimplexLabel = Max[labeledSimplices];
    maxDeclaredFaceLabel = maxFaceLabel = Max[labeledFaces, 0];
    (* Constructs d^k_i : {k-simplex label, (i+1)-th face label} -> True|False (degeneracy)
    for each face-relation, grouped by i. If includeDegeneracies is False, all values are False. *)
    boundaryRules = Table[Flatten[KeyValueMap[{simplex, simplexLabels}|->
        (Thread[Tuples[{simplexLabels, #}] -> 
          includeDegeneracies && i <= k && simplex[[i]] === simplex[[i + 1]]]) &@
        First[Lookup[labeledFaces, {#},
          AssociateTo[labeledFaces, # -> {++maxFaceLabel}];
          {maxFaceLabel}]] &@ Drop[simplex, {i}],
        labeledSimplices], 1],
      {i, 1, k + 1}];
    (* Converts into block matrices { { d^k_0,...,d^k_(k+1) }, { s^(k-1)_0,...,s^(k-1)_(k-1) } } *)
    {{SparseArray[#, {maxFaceLabel, maxSimplexLabel}]& @* Map[Reverse@First[#] -> 1 &] /@ boundaryRules,
      SparseArray[#, {maxSimplexLabel, maxFaceLabel}]& @* Map[Boole] /@ Most[boundaryRules]},
      k - 1 -> labeledFaces}
  ]

(* ::Subsection:: *)
(* Properties of simplicial set *)

sd_SimplicialData[prop_String, args___] := SimplicialDataProps[sd, prop, args];

SimplicialDataProps[SimplicialData[dimensions_, _, _], "Dimension", ___] := Length[dimensions] - 1;
SimplicialDataProps[SimplicialData[dimensions_, _, _], "Dimensions", ___] := dimensions;

(* Face maps in different formats *)
SimplicialDataProps[SimplicialData[_, faceBlocks_, _], "FaceBlocks", ___] := faceBlocks;

SimplicialDataProps[SimplicialData[dimensions_, faceBlocks_, _], "FaceMapsBlocks", ___] := 
  With[{d = Length[dimensions] - 1}, Table[faceBlocks[[Max[k, 1];;d, k + 1]], {k, 0, d}]];

SimplicialDataProps[SimplicialData[dimensions_, faceBlocks_, _], "FaceMaps", ___] := 
  Module[ {d, totalDim, offsets},
    d = Length[dimensions] - 1;
    If[d < 0, Return[{SparseArray[{}, {0, 0}]}]];
    totalDim = Total[dimensions];
    offsets = Accumulate[Prepend[Most[dimensions], 0]];
    Table[ Module[{matrix = SparseArray[{}, {totalDim, totalDim}],
      blocks = faceBlocks[[ Max[k, 1];;d, k + 1 ]]},
      MapIndexed[{block, i} |-> (matrix = insertSparseArrayAt[matrix, block, 
        {offsets[[Max[k,1] + First[i] - 1]], offsets[[Max[k,1] + First[i]]]}]), blocks];
      matrix
    ], {k, 0, d}]
  ];

(* Degeneracy maps in different formats *)
SimplicialDataProps[SimplicialData[_, _, degeneracyBlocks_], "DegeneracyBlocks", ___] := degeneracyBlocks;

SimplicialDataProps[SimplicialData[dimensions_, _, degeneracyBlocks_], "DegeneracyMapsBlocks", ___] := 
  With[{d = Length[dimensions] - 1}, Table[degeneracyBlocks[[k;;d, k]], {k, 1, d}]];

SimplicialDataProps[SimplicialData[dimensions_, _, degeneracyBlocks_], "DegeneracyMaps", ___] := 
  Module[ {d, totalDim, offsets},
    d = Length[dimensions] - 1;
    If[d < 0, Return[{SparseArray[{}, {0, 0}]}]];
    totalDim = Total[dimensions];
    offsets = Accumulate[Prepend[Most[dimensions], 0]];
    Table[Module[{matrix = SparseArray[{}, {totalDim, totalDim}],
      blocks = degeneracyBlocks[[k;;d, k]]},
      MapIndexed[{block, i} |-> (matrix = insertSparseArrayAt[matrix, block, 
        {offsets[[ k + First[i]]], offsets[[ k + First[i] - 1]]}]), blocks];
      matrix
    ], {k, 1, d}]
  ];
    
insertSparseArrayAt[target_SparseArray, source_SparseArray, {rowOffset_, colOffset_}] :=
  Module[{sourceRules = Most[ArrayRules[source]], shiftedRules},
    shiftedRules = ({#[[1, 1]] + rowOffset, #[[1, 2]] + colOffset} -> #[[2]]) & /@ sourceRules;
    SparseArray[Join[Most[ArrayRules[target]], shiftedRules], Dimensions[target]]
  ];

SimplicialDataProps[_, "Properties", ___] := {
	"Dimensions", "Dimension", 
	"FaceBlocks", "FaceMapsBlocks", "FaceMaps",
	"DegeneracyBlocks", "DegeneracyMapsBlocks", "DegeneracyMaps"
};

(* ::Subsection:: *)
(* Rendering of simplicial data *)

SimplicialData /: MakeBoxes[sd : HoldPattern[SimplicialData[dimensions_, faceBlocks_, degeneracyBlocks_]], form_] :=
    BoxForm`ArrangeSummaryBox[
      "SimplicialData",
      sd,
      None,
      {
        {BoxForm`SummaryItem[{"Dimension:  ", Length[dimensions] - 1}]},
        {BoxForm`SummaryItem[{"Simplices:  ", dimensions}]}
      },
      {},
      form,
      "Interpretable" -> Automatic
  ]

(* ::Subsection:: *)
(* Additional functions *)

(* SimplicialSetQ checks if face and degeneracy blocks correspond to set maps, satisfy the simplicial identities, and are compatible *)
SimplicialSetQ[SimplicialData[_, faceBlocks_, degeneracyBlocks_]] := 
  And[
    faceMapsQ[faceBlocks],
    degeneracyMapsQ[degeneracyBlocks],

    simplicialFaceIdentitiesQ[faceBlocks],
    simplicialDegeneracyIdentitiesQ[degeneracyBlocks],

    simplicialFaceDegeneracyCompatibilityQ[faceBlocks, degeneracyBlocks] 
  ]

(* DeltaComplexQ checks that the face blocks represent maps that satisfy the simplicial identities and that all degeneracy maps are zero *)
DeltaComplexQ[SimplicialData[_, faceBlocks_, degeneracyBlocks_]] :=
  And[
    AllTrue[Flatten[degeneracyBlocks, 1], ArrayRules[#] === {{_, _} -> 0}&],
    faceMapsQ[faceBlocks],
    simplicialFaceIdentitiesQ[faceBlocks]
  ]

(* Check simplicial face degeneracy compatibility *)
simplicialFaceDegeneracyCompatibilityQ[faceBlocks_, degeneracyBlocks_] :=
  And[ 
    (* Check that a degenerate simplex contracts onto its degeneration face *)
    AllTrue[Flatten[Table[{k, j}, {k, 1, Length @ faceBlocks}, {j, 1, k}], 1], pair |->
      With[{k = First @ pair, j = Last @ pair},
        With[{identity = IdentityMatrix[Last @ Dimensions[degeneracyBlocks[[k, j]]], SparseArray]},
          And[
            sparseMatrixEqualOnNonzeroCols[faceBlocks[[k, j]] . degeneracyBlocks[[k, j]], identity],
            sparseMatrixEqualOnNonzeroCols[faceBlocks[[k, j + 1]] . degeneracyBlocks[[k, j]], identity]
          ]
        ]
      ]
    ],

    (* Check that taking face and degeneracy maps at different boundaries do not interfere *)
    AllTrue[Flatten[Table[{k, i, j}, {k, 1, Length @ faceBlocks - 1}, {i, 1, k}, {j, 1, i}], 2], triple |->
      With[{k = triple[[1]], i = triple[[2]], j = triple[[3]]},
        And[
          sparseMatrixEqualOnNonzeroCols[faceBlocks[[k + 1, j]] . degeneracyBlocks[[k + 1, i + 1]], degeneracyBlocks[[k, i]] . faceBlocks[[k, j]]],
          sparseMatrixEqualOnNonzeroCols[faceBlocks[[k + 1, i + 2]] . degeneracyBlocks[[k + 1, j]], degeneracyBlocks[[k, j]] . faceBlocks[[k, i + 1]]]
        ]
      ]
    ]
  ]

(* Check set map: each simplex has exactly one i-th face *)
faceMapsQ[faceBlocks_] :=
    AllTrue[ Flatten[faceBlocks, 1], {block} |-> 
      AllTrue[ Range[Last @ Dimensions @ block], block[[All, #]]["NonzeroValues"] === {1} &]
    ] 

(* Check simplicial face identities *)
simplicialFaceIdentitiesQ[faceBlocks_] := 
    AllTrue[Range[Length[faceBlocks] - 1], k |->
      AllTrue[Flatten[Table[{i, j}, {i, 1, k + 1}, {j, i + 1, k + 2}], 1], pair |->
        With[{i = First @ pair, j = Last @ pair},
          faceBlocks[[k, i]] . faceBlocks[[k + 1, j]] == faceBlocks[[k, j - 1]] . faceBlocks[[k + 1, i]]
        ]
      ]
    ]

(* Check set map: each simplex is the i-th degeneracy face of maximally one other simplex *)
degeneracyMapsQ[degeneracyBlocks_] :=
    AllTrue[ Flatten[degeneracyBlocks, 1], {block} |-> 
      AllTrue[ Range[Last @ Dimensions @ block],
        AnyTrue[ {{}, {1}} , SameAs[block[[All, #]]["NonzeroValues"]] ]&
    ]]

(* Check simplicial degeneracy identities *)
simplicialDegeneracyIdentitiesQ[degeneracyBlocks_] :=
  AllTrue[Range[Length @ degeneracyBlocks - 1], k |->
    AllTrue[Flatten[Table[{i, j}, {i, 1, k}, {j, i + 1, k + 1}], 1], pair |->
      With[{i = First @ pair, j = Last @ pair},
        sparseMatrixEqualOnNonzeroCols[degeneracyBlocks[[k + 1, j]] . degeneracyBlocks[[k , i]], degeneracyBlocks[[k + 1, i]] . degeneracyBlocks[[k, j - 1]]]
      ]
    ]
  ]

(* Helper function to compare sparse matrices only in columns where both are nonzero *)
sparseMatrixEqualOnNonzeroCols[lhs_SparseArray, rhs_SparseArray] :=
  Module[{lhsNonzeroCols, rhsNonzeroCols, commonCols},
    lhsNonzeroCols = Union[Last /@ lhs["NonzeroPositions"]];
    rhsNonzeroCols = Union[Last /@ rhs["NonzeroPositions"]];
    commonCols = Intersection[lhsNonzeroCols, rhsNonzeroCols];
    If[Length[commonCols] == 0, True,
      AllTrue[commonCols, col |-> lhs[[All, col]] === rhs[[All, col]]]
    ]
  ];


(* SimplicialComplexQ checks if the face maps correspond to a simplicial complex *)
SimplicialComplexQ[SimplicialData[_, faceBlocks_, _]] := 
    Module[ {totalFace, kSimplexCount, totalBoundary, elements},
        AllTrue[ Range[ Length[faceBlocks] ], { k } |-> (
            kSimplexCount = Last @ Dimensions[ First @ faceBlocks[[k]] ];
            totalFace = Total @ faceBlocks[[k]];
            totalBoundary = If[ k == 1, totalFace, totalBoundary = totalBoundary . totalFace ];
            elements = (Sort @* Flatten @* (totalBoundary[[All, #]]["NonzeroPositions"]&)) /@ Range[kSimplexCount]; 

            And[
                (* Check that each k-simplex has k+1 unique faces *) 
                AllTrue[ Range[kSimplexCount], totalFace[[All, #]]["NonzeroValues"] === ConstantArray[1, k+1]& ],

                (* Check that each k-simplex has boundary consisting of a unique combination of k+1 elements  *) 
                AllTrue[ elements , Length[#] === k + 1& ],
                Length @ Union[elements] === kSimplexCount 
            ] )
        ]
    ]
