Package["WolframInstitute`Infrageometry`"]

PackageExport[InfraSubstrate]
PackageExport[InfraSubstrateStyle]
PackageExport[InfraSubstrateCode]


(* ===================== InfraSubstrate ===================== *)


$substrateClasses = <|
  "OpenManifold" -> {
    "SquareMeshGraph", "CubeMeshGraph",
    "TriangularTilingGraph", "SquareTilingGraph", "HexagonalTilingGraph", "HyperbolicTilingGraph",
    "SquareGridGraph", "CubicGridGraph" },
  "ClosedManifold" -> {
    "SphereMeshGraph",
    "SquareTorusGraph", "TriangularTorusGraph", "HexagonalTorusGraph",
    "UniformLengthSphereGraph", "UniformLengthProlateEllipsoidGraph", "UniformLengthTriaxialEllipsoidGraph", "BuckyballGraph" },
  "Fractal" -> { "SierpinskiTriangleGraph", "MengerCarpetGraph", "MengerSpongeGraph" },
  "Exotic" -> { "BinaryTreeGraph", "DilutedTreeGraph", "CompleteGraph" },
  "WolframModel" -> { "wm6655", "wm8619", "wm1811" }
|>;

Options[ InfraSubstrate ] = { "KeepCoordinates" -> False, "Inflate" -> None };

InfraSubstrate[ ] := $substrateClasses

InfraSubstrate[ All ] := Catenate @ Values @ $substrateClasses

InfraSubstrate[ name_String ] := InfraSubstrate[ name, "Medium" ]

InfraSubstrate[ name_String, size_, style : ( _String | Automatic ) : Automatic,
    opts : OptionsPattern[ { InfraSubstrate, Graph } ] ] :=
  With[
    { own = FilterRules[ { opts }, Options @ InfraSubstrate ] },
    { g = ReleaseHold @ substrateCode[ name, size, OptionValue[ InfraSubstrate, own, "Inflate" ] ],
      keep = TrueQ @ OptionValue[ InfraSubstrate, own, "KeepCoordinates" ] },
    Graph[ g, FilterRules[ { opts }, Options @ Graph ],
      substrateCoordinates[ g, keep ],
      Sequence @@ InfraSubstrateStyle[ name, Replace[ style, Automatic :> defaultAmbientStyle[ g, size ] ] ] ] ]


(* ===================== InfraSubstrateCode ===================== *)


Options[ InfraSubstrateCode ] = Options[ InfraSubstrate ];

InfraSubstrateCode[ name_String ] := InfraSubstrateCode[ name, "Medium" ]

InfraSubstrateCode[ name_String, size_, opts : OptionsPattern[ { InfraSubstrate, Graph } ] ] :=
  With[
    { own = FilterRules[ { opts }, Options @ InfraSubstrate ] },
    { code = substrateCode[ name, size, OptionValue[ InfraSubstrate, own, "Inflate" ] ] },
    { g = ReleaseHold @ code },
    { arguments = DeleteCases[
        Join[ FilterRules[ { opts }, Options @ Graph ],
          { substrateCoordinates[ g, TrueQ @ OptionValue[ InfraSubstrate, own, "KeepCoordinates" ] ] } ],
        VertexCoordinates -> _List ] },
    Replace[ Join[ publicSymbols @ code, HoldComplete @@ arguments ], {
      HoldComplete[ body_ ] :> HoldForm @ body,
      HoldComplete[ body_, args__ ] :> HoldForm @ Graph[ body, args ] } ] ]


(* ===================== InfraSubstrateStyle ===================== *)


InfraSubstrateStyle[ ] := { "Default", "Small", "Medium", "Large" }

InfraSubstrateStyle[ "Default" ] = { };

InfraSubstrateStyle[ "Small" ] = {
  EdgeStyle -> Directive[ StandardGray, Opacity[ 0.35 ] ],
  VertexStyle -> Directive[ StandardGray, Opacity[ 0.5 ], EdgeForm[ { GrayLevel[ 0 ], Opacity[ 0.65 ] } ] ],
  VertexSize -> { "Scaled", 0.013 } };

InfraSubstrateStyle[ "Medium" ] = {
  EdgeStyle -> Directive[ StandardGray, Opacity[ 0.3 ] ],
  VertexStyle -> Directive[ StandardGray, Opacity[ 0.45 ], EdgeForm[ { GrayLevel[ 0 ], Opacity[ 0.6 ] } ] ],
  VertexSize -> { "Scaled", 0.009 } };

InfraSubstrateStyle[ "Large" ] = {
  EdgeStyle -> Directive[ StandardGray, Opacity[ 0.22 ] ],
  VertexStyle -> Directive[ StandardGray, Opacity[ 0.33 ], EdgeForm[ { GrayLevel[ 0 ], Opacity[ 0.45 ] } ] ],
  VertexSize -> { "Scaled", 0.006 } };

(* a custom look for one substrate at one size is one more definition above this fallback *)
InfraSubstrateStyle[ name_String, size_String ] := InfraSubstrateStyle @ size


(* ===================== The roster ===================== *)

substrateCode[ name_, size_, None ] := substrateCode[ name, size ]

substrateCode[ name_, size_, inflate_ ] :=
  Join[ HoldComplete @ InflateGraph, substrateCode[ name, size ],
    HoldComplete @@ Replace[ inflate, amount : Except[ { ___Rule } ] :> { "ExtraVertices" -> amount } ] ] /.
    HoldComplete[ head_, rest__ ] :> HoldComplete @ head[ rest ]

(* removing the rim leaves a few dangling spikes on a mesh; one deletion pass reaches them all *)
substrateCode[ "SquareMeshGraph", size_ ] :=
  With[ { measure = size /. { "Small" -> 0.0085, "Medium" -> 0.0028, "Large" -> 0.00082 } },
    HoldComplete @ With[
      { mesh = BoundarylessGraph @ DiscretizeRegion[ Rectangle[ ], MaxCellMeasure -> measure, PrecisionGoal -> Infinity ] },
      VertexDelete[ mesh, Pick[ VertexList @ mesh, VertexDegree @ mesh, 1 ] ] ] ]

substrateCode[ "CubeMeshGraph", size_ ] :=
  With[ { measure = size /. { "Small" -> 0.004, "Medium" -> 0.00133, "Large" -> 0.0004 } },
    HoldComplete @ With[
      { mesh = BoundarylessGraph @ DiscretizeRegion[ Cuboid[ ], MaxCellMeasure -> measure, PrecisionGoal -> Infinity ] },
      VertexDelete[ mesh, Pick[ VertexList @ mesh, VertexDegree @ mesh, 1 ] ] ] ]

(* DiscretizeRegion ignores MaxCellMeasure on a special surface region unless PrecisionGoal is
   given too, and then only in the {"Area" -> m} form; the low goal leaves refined vertices off
   the unit sphere, so they are normalized back onto it *)
substrateCode[ "SphereMeshGraph", size_ ] :=
  With[ { area = size /. { "Small" -> 0.5, "Medium" -> 0.1, "Large" -> 0.02 } },
    HoldComplete @ With[
      { mesh = DiscretizeRegion[ Sphere[ ], MaxCellMeasure -> { "Area" -> area }, PrecisionGoal -> 1 ] },
      Graph[ IndexGraph @ MeshConnectivityGraph @ mesh,
        VertexCoordinates -> Normalize /@ MeshCoordinates @ mesh ] ] ]

substrateCode[ "TriangularTilingGraph", size_ ] :=
  With[ { radius = size /. { "Small" -> 5, "Medium" -> 9, "Large" -> 16 } },
    HoldComplete @ BoundarylessGraph[ TessellationNeighborhoodGraph[ { 3, 6 }, radius ], Method -> "MaxDegree" ] ]

substrateCode[ "SquareTilingGraph", size_ ] :=
  With[ { radius = size /. { "Small" -> 7, "Medium" -> 12, "Large" -> 22 } },
    HoldComplete @ BoundarylessGraph[ TessellationNeighborhoodGraph[ { 4, 4 }, radius ], Method -> "MaxDegree" ] ]

substrateCode[ "HexagonalTilingGraph", size_ ] :=
  With[ { radius = size /. { "Small" -> 8, "Medium" -> 14, "Large" -> 25 } },
    HoldComplete @ BoundarylessGraph[ TessellationNeighborhoodGraph[ { 6, 3 }, radius ], Method -> "MaxDegree" ] ]

substrateCode[ "HyperbolicTilingGraph", size_ ] :=
  With[ { radius = size /. { "Small" -> 3, "Medium" -> 4, "Large" -> 5 } },
    HoldComplete @ BoundarylessGraph[ TessellationNeighborhoodGraph[ { 3, 7 }, radius ], Method -> "MaxDegree" ] ]

substrateCode[ "SquareGridGraph", size_ ] :=
  With[ { dims = size /. { "Small" -> { 10, 10 }, "Medium" -> { 17, 17 }, "Large" -> { 32, 32 } } },
    HoldComplete @ BoundarylessGraph[ GridGraph @ dims, Method -> "MaxDegree" ] ]

(* GridGraph indexes with the first dimension fastest, and without explicit coordinates a 3D
   grid falls back to a 2D spring layout *)
substrateCode[ "CubicGridGraph", size_ ] :=
  With[ { dims = size /. { "Small" -> { 5, 5, 5 }, "Medium" -> { 7, 7, 7 }, "Large" -> { 10, 10, 10 } } },
    HoldComplete @ BoundarylessGraph[
      Graph[ GridGraph @ dims, VertexCoordinates -> Reverse /@ Tuples[ Range /@ Reverse @ dims ] ],
      Method -> "MaxDegree" ] ]

(* the tessellation names a torus vertex by its cell and, on the two-vertex cells, its
   sublattice, which offsets it by half a cell; the honeycomb carries two vertices per cell,
   so its torus takes half-size dims *)
substrateCode[ name : "SquareTorusGraph" | "TriangularTorusGraph" | "HexagonalTorusGraph", size_ ] :=
  With[
    { shape = StringDelete[ name, "TorusGraph" ],
      dims = size /. If[ name === "HexagonalTorusGraph",
        { "Small" -> { 7, 7 }, "Medium" -> { 15, 10 }, "Large" -> { 25, 20 } },
        { "Small" -> { 10, 10 }, "Medium" -> { 20, 15 }, "Large" -> { 40, 25 } } ] },
    { m = First @ dims, n = Last @ dims },
    HoldComplete @ With[
      { torus = TorusTessellation[ dims, shape ] },
      Graph[ torus, VertexCoordinates -> Map[
        v |-> With[ { s = If[ Length @ v >= 3, v[[ 3 ]], 0 ] },
          { u = 2 Pi ( v[[ 1 ]] + s / 2 ) / m, w = 2 Pi ( v[[ 2 ]] + s / 2 ) / n },
          { ( 1 + 0.4 Cos[ w ] ) Cos[ u ], ( 1 + 0.4 Cos[ w ] ) Sin[ u ], 0.4 Sin[ w ] } ],
        VertexList @ torus ] ] ] ]

substrateCode[ "UniformLengthSphereGraph", size_ ] :=
  With[ { count = size /. { "Small" -> 100, "Medium" -> 300, "Large" -> 1000 } },
    HoldComplete @ UniformLengthGraph[ BoundaryDiscretizeRegion @ Ellipsoid[ { 0, 0, 0 }, { 1, 1, 1 } ],
      count, "KeepCoordinates" -> True ] ]

substrateCode[ "UniformLengthProlateEllipsoidGraph", size_ ] :=
  With[ { count = size /. { "Small" -> 100, "Medium" -> 300, "Large" -> 1000 } },
    HoldComplete @ UniformLengthGraph[ BoundaryDiscretizeRegion @ Ellipsoid[ { 0, 0, 0 }, { 5, 1, 1 } ],
      count, "KeepCoordinates" -> True ] ]

substrateCode[ "UniformLengthTriaxialEllipsoidGraph", size_ ] :=
  With[ { count = size /. { "Small" -> 100, "Medium" -> 300, "Large" -> 1000 } },
    HoldComplete @ UniformLengthGraph[ BoundaryDiscretizeRegion @ Ellipsoid[ { 0, 0, 0 }, { 4, 2, 1 } ],
      count, "KeepCoordinates" -> True ] ]

substrateCode[ "BuckyballGraph", size_ ] :=
  With[ { steps = size /. { "Small" -> 1, "Medium" -> 2, "Large" -> 4 } },
    HoldComplete @ With[
      { ball = ResourceFunction[ "BuckyballGraph" ][ steps ] },
      Graph[ ball, VertexCoordinates -> GraphEmbedding @ ball ] ] ]

substrateCode[ "SierpinskiTriangleGraph", size_ ] :=
  With[ { steps = size /. { "Small" -> 4, "Medium" -> 5, "Large" -> 6 } },
    HoldComplete @ IndexGraph @ MeshConnectivityGraph @ SierpinskiMesh @ steps ]

substrateCode[ "MengerCarpetGraph", size_ ] :=
  With[ { steps = size /. { "Small" -> 2, "Medium" -> 3, "Large" -> 4 } },
    HoldComplete @ IndexGraph @ MeshConnectivityGraph @ MengerMesh @ steps ]

substrateCode[ "MengerSpongeGraph", size_ ] :=
  With[ { steps = size /. { "Small" -> 1, "Medium" -> 2, "Large" -> 3 } },
    HoldComplete @ IndexGraph @ MeshConnectivityGraph @ MengerMesh[ steps, 3 ] ]

substrateCode[ "BinaryTreeGraph", size_ ] :=
  With[ { count = size /. { "Small" -> 63, "Medium" -> 255, "Large" -> 1023 } },
    HoldComplete @ KaryTree @ count ]

substrateCode[ "CompleteGraph", size_ ] :=
  With[ { count = size /. { "Small" -> 10, "Medium" -> 30, "Large" -> 90 } },
    HoldComplete @ CompleteGraph @ count ]

(* the branching sequence doubles on the levels whose index is a perfect power 1/exponent, so
   the tree grows subexponentially: its ball of radius r carries about r^(1/exponent) branchings *)
substrateCode[ "DilutedTreeGraph", size_ ] :=
  With[ { spec = size /. { "Small" -> { 1/2, 16 }, "Medium" -> { 1/2, 26 }, "Large" -> { 1/2, 42 } } },
    { exponent = First @ spec, depth = Last @ spec },
    HoldComplete @ BranchingSequenceTree @ Table[
      If[ MemberQ[ Ceiling[ Range[ depth ]^( 1 / exponent ) ], level ], 2, 1 ], { level, depth } ] ]

$wolframModels = <|
  "wm6655" -> {
    { { 1, 2 }, { 1, 3 } } -> { { 1, 2 }, { 1, 4 }, { 2, 4 }, { 3, 4 } }, { { 1, 1 }, { 1, 1 } },
    { "Small" -> 7, "Medium" -> 9, "Large" -> 11 } },
  "wm8619" -> {
    { { 1, 2, 2 }, { 1, 3, 4 } } -> { { 4, 5, 5 }, { 5, 3, 2 }, { 1, 2, 5 } }, { { 1, 1, 1 }, { 1, 1, 1 } },
    { "Small" -> 100, "Medium" -> 300, "Large" -> 1000 } },
  "wm1811" -> {
    { { 1, 1, 2 }, { 1, 3, 4 } } -> { { 4, 4, 3 }, { 2, 5, 3 }, { 2, 5, 3 } }, { { 1, 1, 1 }, { 1, 1, 1 } },
    { "Small" -> 100, "Medium" -> 300, "Large" -> 1000 } }
|>;

substrateCode[ id_String /; StringMatchQ[ id, "wm" ~~ DigitCharacter .. ], size_ ] :=
  With[
    { model = Replace[ $wolframModels @ id, _Missing :>
        { First @ Flatten[ { ResourceFunction[ "WolframModelData" ][ id, "Rule" ] }, 2 ],
          ResourceFunction[ "WolframModelData" ][ id, "InitialCondition" ],
          { "Small" -> 6, "Medium" -> 8, "Large" -> 10 } } ] },
    { rule = model[[ 1 ]], init = model[[ 2 ]], steps = size /. model[[ 3 ]] },
    HoldComplete @ With[
      { state = ResourceFunction[ "WolframModel" ][ rule, init, steps, "FinalState" ] },
      If[ AllTrue[ state, Length @ # === 2 & ],
        Graph @ DeleteDuplicates[ UndirectedEdge @@@ Sort /@ Select[ state, Apply @ UnsameQ ] ],
        UndirectedGraph @ ResourceFunction[ "HypergraphToGraph" ][ state ] ] ] ]

(* ===================== Graph options ===================== *)

substrateCoordinates[ g_Graph, keep_ ] :=
  Which[
    Options[ g, VertexCoordinates ] === { VertexCoordinates -> Automatic }, Sequence @@ { },
    keep, VertexCoordinates -> GraphEmbedding @ g,
    True, Sequence @@ { VertexCoordinates -> Automatic,
      GraphLayout -> { "VertexLayout" -> "SpringElectricalEmbedding",
        "Dimension" -> Last @ Dimensions @ GraphEmbedding @ g } } ]

defaultAmbientStyle[ g_, size_ ] :=
  Replace[ size, Except[ "Small" | "Medium" | "Large" ] :>
    Which[ VertexCount @ g <= 250, "Small", VertexCount @ g <= 800, "Medium", True, "Large" ] ]

SetAttributes[ $public, HoldAllComplete ]

publicSymbols[ held_ ] :=
  held /. Map[
      s |-> s -> ToExpression[
        StringDelete[ SymbolName @ Unevaluated @ s, "$" ~~ EndOfString ], InputForm, $public ],
      Cases[ held, s_Symbol /; StringEndsQ[ Context @ Unevaluated @ s, "`PackagePrivate`" ],
        Infinity, Heads -> True ] ] /.
    $public[ x_ ] :> x
