Package["WolframInstitute`Infrageometry`"]

PackageExport[DisplacementCompose]
PackageExport[DisplacementScale]
PackageExport[DisplacementNegative]
PackageExport[DisplacementInverse]
PackageExport[DisplacementSum]
PackageExport[DisplacementCommutator]
PackageExport[DisplacementBracket]
PackageExport[DisplacementMagnitude]
PackageExport[DisplacementReduce]
PackageExport[DisplacementSingleValuedQ]
PackageExport[DisplacementBijectionQ]
PackageExport[DisplacementIsomorphismQ]
PackageExport[ContinuousDisplacementQ]
PackageExport[RandomDisplacement]
PackageExport[FindKillingDisplacement]
PackageExport[KillingDisplacementMagnitude]
PackageExport[PolarDisplacements]
PackageExport[GradientDisplacement]
PackageExport[TranslationDisplacement]
PackageExport[DisplacementPlot]

PackageScope[displacementGammaSet]
PackageScope[displacementNegativeAt]
PackageScope[displacementSetCenters]
PackageScope[displacementDistances]
PackageScope[displacementGeodesicCounts]
PackageScope[displacementSetDistance]


(* ===================== Displacements ===================== *)

(* A displacement is an association v -> { w1, w2, ... } (multivalued in
   general; values are always lists), thought of as v -> exp_v(r X) for a
   vector field X at scale r = DisplacementMagnitude -- a section of the
   scale-r tangent bundle whose fiber over v is the r-ball at v.  Flows
   compose as maps, but by Baker-Campbell-Hausdorff

     Phi_Y . Phi_X = exp( X + Y + (1/2)[X, Y] + O(r^3) ),

   composition is NOT the sum: the sum is the bisector of the two
   composition orders (the +-(1/2)[X, Y] terms cancel), negation is the
   metric reflection through the base point, and general scalars are
   endpoints of t-scaled geodesics (rounded target distance, straightest
  candidates kept by geodesic flux). Weak k-continuity asks for one close
  target pair across each edge; Hausdorff and strong variants control all
  targets. Genuine metric ties stay multivalued and DisplacementReduce
  contracts them by iterated centres. *)

(* flows act left to right: DisplacementCompose[X, Y] = Phi_Y . Phi_X *)
DisplacementCompose[ displacements__Association ] :=
  Fold[ { done, next } |-> ( Union @@ Lookup[ next, # ] & ) /@ done, { displacements } ]

(* (t D)(v): endpoints of the geodesics v -> D(v) scaled to t times their length *)
DisplacementScale[ graph_Graph, displacement_Association, t_ ] :=
  AssociationMap[
    ( Union @@ Table[ displacementGammaSet[ graph, #, target, t ], { target, displacement @ # } ] ) &,
    Keys @ displacement ]

DisplacementNegative[ graph_Graph, displacement_Association ] :=
  DisplacementScale[ graph, displacement, -1 ]

DisplacementInverse[ displacement_Association ] :=
  With[ { vertices = Keys @ displacement },
    AssociationMap[
      { vertex } |-> Select[ vertices, MemberQ[ displacement @ #, vertex ] & ],
      vertices ] ]

(* X + Y: bisector of Phi_Y Phi_X (v) and Phi_X Phi_Y (v) -- the two orders are
   exp( X + Y +- (1/2)[X, Y] + O(r^3) ), so their midpoints realise
   exp_v( X + Y ) with the commutator cancelled *)
DisplacementSum[ graph_Graph, displacement1_Association, displacement2_Association ] :=
  With[ { order12 = DisplacementCompose[ displacement1, displacement2 ],
          order21 = DisplacementCompose[ displacement2, displacement1 ] },
    AssociationMap[
      ( Union @@ Flatten[ Table[ displacementGammaSet[ graph, end1, end2, 1/2 ],
          { end1, order12 @ # }, { end2, order21 @ # } ], 1 ] ) &,
      Keys @ displacement1 ] ]

(* exact group commutator for bijective displacements *)
DisplacementCommutator[ graph_Graph, displacement1_Association, displacement2_Association ] :=
  DisplacementCompose[
    displacement1, displacement2,
    DisplacementInverse @ displacement1, DisplacementInverse @ displacement2 ]

DisplacementCommutator[ graph_Graph, displacement1_Association, displacement2_Association, point_ ] :=
  DisplacementCommutator[ graph, displacement1, displacement2 ] @ point

(* metric bracket candidate Phi_{-Y} . Phi_{-X} . Phi_Y . Phi_X
   = exp( r^2 [X, Y] + O(r^3) ), second order in the scale *)
DisplacementBracket[ graph_Graph, displacement1_Association, displacement2_Association ] :=
  AssociationMap[ DisplacementBracket[ graph, displacement1, displacement2, # ] &,
    Keys @ displacement1 ]

DisplacementBracket[ graph_Graph, displacement1_Association, displacement2_Association, point_ ] :=
  Fold[
    { points, step } |-> Union @@ ( step /@ points ),
    { point },
    { displacement1, displacement2,
      displacementNegativeAt[ graph, displacement1, # ] &,
      displacementNegativeAt[ graph, displacement2, # ] & } ]

DisplacementMagnitude[ graph_Graph, displacement_Association ] :=
  Max @ KeyValueMap[
    { point, targets } |-> Max @ Table[ GraphDistance[ graph, point, target ], { target, targets } ],
    displacement ]

(* contract each value set to its centre (minimal eccentricity under the mutual
   graph distances, centre drawn from the set itself), iterated to a fixed
   point; ties keep the set multivalued *)
DisplacementReduce[ graph_Graph, displacement_Association ] :=
  Map[ FixedPoint[ displacementSetCenters @ graph, # ] &, displacement ]


(* ===================== Predicates ===================== *)

DisplacementSingleValuedQ[ displacement_Association ] :=
  AllTrue[ Values @ displacement, Length @ # == 1 & ]

DisplacementBijectionQ[ displacement_Association ] :=
  DisplacementSingleValuedQ @ displacement &&
    Sort @ Catenate @ Values @ displacement === Sort @ Keys @ displacement

DisplacementIsomorphismQ[ graph_Graph, displacement_Association ] :=
  DisplacementBijectionQ @ displacement &&
    AllTrue[ EdgeList @ graph,
      EdgeQ[ graph, UndirectedEdge @@ Catenate @ Lookup[ displacement, List @@ # ] ] & ]

Options[ ContinuousDisplacementQ ] = { Method -> "Weak" };

ContinuousDisplacementQ[ graph_Graph, displacement_Association, opts : OptionsPattern[] ] :=
  ContinuousDisplacementQ[ graph, displacement, 1, opts ]

(* weak: one close pair; Hausdorff: every target has a close partner;
   strong: every cross-pair is close *)
ContinuousDisplacementQ[
    graph_Graph, displacement_Association, k_, OptionsPattern[] ] :=
  AllTrue[ EdgeList @ graph,
    { edge } |-> displacementSetDistance[
      graph, displacement @ First @ edge, displacement @ Last @ edge,
      OptionValue[ Method ] ] <= k ]


(* ===================== Canonical displacements ===================== *)

(* polar pair at a centre: { radial, angular } -- radial steps along the
   geodesics from the centre (outward by default, inward with
   "Direction" -> "Inward"), angular steps along the cross edges of equal
   distance; a vertex with no admissible step stays put *)
Options[ PolarDisplacements ] = { "Direction" -> "Outward" };

PolarDisplacements[ graph_Graph, center_, OptionsPattern[] ] :=
  With[
    { dist = displacementDistances[ graph, center ],
      sign = Switch[ OptionValue[ "Direction" ], "Outward", 1, "Inward", -1 ] },
    { AssociationMap[
        { v } |-> Replace[ Select[ AdjacencyList[ graph, v ], dist[ # ] == dist[ v ] + sign & ], { } -> { v } ],
        VertexList @ graph ],
      AssociationMap[
        { v } |-> Replace[ Select[ AdjacencyList[ graph, v ], dist[ # ] == dist[ v ] & ], { } -> { v } ],
        VertexList @ graph ] }
  ]

(* steepest ascent of a vertex function: v -> the neighbours maximising the
   increase of f; local maxima stay put.  The outward radial displacement is
   the gradient of the distance from the centre. *)
GradientDisplacement[ graph_Graph, f_Association ] :=
  AssociationMap[
    { v } |-> With[ { best = MaximalBy[ AdjacencyList[ graph, v ], f ] },
      If[ f @ First @ best > f @ v, best, { v } ] ],
    VertexList @ graph ]

(* translation along an embedding: v -> vertices whose coordinates are nearest
   to position(v) + vector *)
TranslationDisplacement[ graph_Graph, vector_List ] :=
  With[
    { position = AssociationThread[ VertexList @ graph, GraphEmbedding @ graph ],
      nearest = Nearest[ GraphEmbedding @ graph -> VertexList @ graph ] },
    AssociationMap[ nearest[ position @ # + vector ] &, VertexList @ graph ] ]


(* ===================== Generation ===================== *)

(* random continuous displacement: breadth-first shell extension -- targets
   drawn from the radius ball, kept within one step of the targets of
   already-assigned neighbours -- followed by repair sweeps over the edges
   still violating 1-continuity, restarting from a fresh draw if a run of
   sweeps fails to converge *)
RandomDisplacement[ graph_Graph, radius_ : 1 ] :=
  Module[ { targets, order, violating },
    order = First @ Last @ Reap[ BreadthFirstScan[ graph, First @ VertexList @ graph,
      { "DiscoverVertex" -> ( Sow[ #1 ] & ) } ] ];
    Do[
      targets = Association[];
      Do[
        targets[ vertex ] = With[
          { neighborTargets = Catenate @ Lookup[ targets,
              Intersection[ AdjacencyList[ graph, vertex ], Keys @ targets ], { } ],
            ball = Union[ { vertex }, AdjacencyList[ graph, vertex, radius ] ] },
          { admissible = Fold[ Intersection, ball,
              Table[ Union[ { target }, AdjacencyList[ graph, target ] ], { target, neighborTargets } ] ] },
          { RandomChoice @ If[ admissible === { },
              MinimalBy[ ball, { candidate } |->
                Max @ Table[ GraphDistance[ graph, candidate, target ], { target, neighborTargets } ] ],
              admissible ] } ],
        { vertex, order } ];
      Do[
        violating = Union @ Catenate @ Select[ List @@@ EdgeList[ graph ],
          { edge } |-> GraphDistance[ graph, First @ targets @ First @ edge, First @ targets @ Last @ edge ] > 1 ];
        If[ violating === { }, Break[ ] ];
        Do[
          targets[ vertex ] = With[
            { neighborTargets = Catenate @ Lookup[ targets, AdjacencyList[ graph, vertex ] ],
              ball = Union[ { vertex }, AdjacencyList[ graph, vertex, radius ] ] },
            { RandomChoice @ MinimalBy[ ball, { candidate } |->
                { Max @ Table[ GraphDistance[ graph, candidate, target ], { target, neighborTargets } ],
                  GraphDistance[ graph, vertex, candidate ] } ] } ],
          { vertex, violating } ],
        { 50 } ];
      If[ violating === { }, Break[ ] ],
      { 5 } ];
    targets ]

(* smallest Killing displacement: nontrivial graph automorphism of minimal
   magnitude, as a displacement *)
FindKillingDisplacement[ graph_Graph ] :=
  First @ FindKillingDisplacement[ graph, All ]

FindKillingDisplacement[ graph_Graph, All ] :=
  With[
    { vertices = VertexList @ graph },
    { permutations = DeleteCases[ GroupElements @ GraphAutomorphismGroup @ graph, Cycles[ { } ] ] },
    { displacements = Table[
        AssociationThread[ vertices, List /@ Permute[ vertices, permutation ] ],
        { permutation, permutations } ] },
    MinimalBy[ displacements, DisplacementMagnitude[ graph, # ] & ]
  ]

KillingDisplacementMagnitude[ graph_Graph ] :=
  Min @ Append[
    Map[ DisplacementMagnitude[ graph, # ] &, FindKillingDisplacement[ graph, All ] ],
    Infinity ]


(* ===================== Plotting ===================== *)

(* displacements as bent arcs v -> w over the graph's own embedding; the k-th
   displacement of the sequence gets the k-th Standard (ColorData 97) colour *)
Options[ DisplacementPlot ] = { ImageSize -> 320 };

DisplacementPlot[ graph_Graph, displacement_Association, opts : OptionsPattern[] ] :=
  DisplacementPlot[ graph, { displacement }, opts ]

DisplacementPlot[ graph_Graph, displacements : { __Association }, OptionsPattern[] ] :=
  With[
    { position = AssociationThread[ VertexList @ graph, GraphEmbedding @ graph ] },
    Show[
      Graph[ graph, VertexCoordinates -> GraphEmbedding @ graph, VertexSize -> Small,
        VertexStyle -> LightGray, EdgeStyle -> Opacity[ 0.4, LightGray ] ],
      Graphics @ Table[
        With[
          { pairs = DeleteCases[
              Catenate @ KeyValueMap[
                { vertex, targets } |-> Table[ { position @ vertex, position @ target }, { target, targets } ],
                displacements[[ index ]] ],
              { p_, p_ } ] },
          { ColorData[ 97 ][ index ], Arrowheads[ 0.02 ],
            Arrow @ BezierCurve @ { #[[ 1 ]], ( #[[ 1 ]] + #[[ 2 ]] )/2 + 0.2 { 1, -1 } Reverse[ #[[ 2 ]] - #[[ 1 ]] ], #[[ 2 ]] } & /@ pairs } ],
        { index, Length @ displacements } ],
      ImageSize -> OptionValue[ ImageSize ]
    ]
  ]


(* ===================== Helpers ===================== *)

(* gamma(t) on the extended geodesic a = gamma(0), b = gamma(1): points on the
   ray a -> b (t >= 0) or its opposite ray (t < 0) at distance closest to
   |t| d(a, b) from a; among those the straightest, by maximal geodesic flux
   sigma(p, q) sigma(q, s) / sigma(p, s) through the middle point of the
   aligned triple (Menger betweenness) *)
displacementGammaSet[ graph_, a_, b_, t_ ] :=
  With[
    { da = displacementDistances[ graph, a ], db = displacementDistances[ graph, b ],
      sigmaA = displacementGeodesicCounts[ graph, a ], sigmaB = displacementGeodesicCounts[ graph, b ] },
    { ray = Select[ VertexList @ graph,
        If[ t >= 0,
          da[ # ] + db[ # ] == da[ b ] || da[ # ] == da[ b ] + db[ # ],
          db[ # ] == da[ # ] + da[ b ]
        ] & ] },
    { closest = MinimalBy[ ray, Abs[ da[ # ] - Abs[ t ] da[ b ] ] & ] },
    MaximalBy[ closest,
      Which[
        t < 0,                        sigmaA[ # ] sigmaA[ b ] / sigmaB[ # ],
        da[ # ] + db[ # ] == da[ b ], sigmaA[ # ] sigmaB[ # ] / sigmaA[ b ],
        True,                         sigmaA[ b ] sigmaB[ # ] / sigmaA[ # ]
      ] & ]
  ]

(* the negative displacement evaluated lazily at one point *)
displacementNegativeAt[ graph_, displacement_, point_ ] :=
  Union @@ Table[ displacementGammaSet[ graph, point, target, -1 ], { target, displacement @ point } ]

(* one centre step: members of minimal eccentricity within the set *)
displacementSetCenters[ graph_ ][ targets_List ] :=
  MinimalBy[ targets,
    { candidate } |-> Max @ Table[ GraphDistance[ graph, candidate, target ], { target, targets } ] ]

displacementSetDistance[ graph_, targets1_, targets2_, "Weak" ] :=
  Min @ Flatten @ Outer[ GraphDistance[ graph, #1, #2 ] &, targets1, targets2, 1, 1 ]

displacementSetDistance[ graph_, targets1_, targets2_, "Hausdorff" ] :=
  Max[
    Max @ Map[ target1 |-> Min @ Map[ GraphDistance[ graph, target1, # ] &, targets2 ], targets1 ],
    Max @ Map[ target2 |-> Min @ Map[ GraphDistance[ graph, target2, # ] &, targets1 ], targets2 ] ]

displacementSetDistance[ graph_, targets1_, targets2_, "Strong" ] :=
  Max @ Flatten @ Outer[ GraphDistance[ graph, #1, #2 ] &, targets1, targets2, 1, 1 ]

displacementDistances[ graph_, source_ ] :=
  AssociationThread[ VertexList @ graph, GraphDistance[ graph, source ] ]

(* sigma(source, v): number of geodesics, by BFS layers *)
displacementGeodesicCounts[ graph_, source_ ] :=
  Module[ { dist = displacementDistances[ graph, source ], counts = Association[ source -> 1 ] },
    Do[
      counts[ vertex ] = Total @ Lookup[ counts,
        Select[ AdjacencyList[ graph, vertex ], dist[ # ] == dist[ vertex ] - 1 & ] ],
      { vertex, SortBy[ Select[ VertexList @ graph, 0 < dist[ # ] < Infinity & ], dist ] }
    ];
    counts
  ]
