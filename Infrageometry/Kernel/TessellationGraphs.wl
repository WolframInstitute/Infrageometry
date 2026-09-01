Package["WolframInstitute`Infrageometry`"]

PackageExport[TessellationGraph]
PackageExport[TorusTessellation]
PackageExport[TessellationCurvature]
PackageExport[TessellationEulerCharacteristic]
PackageExport[TessellationGenus]
PackageExport[TessellationNeighborhoodGraph]
PackageExport[CosetEnumeration]
PackageExport[LowIndexMaps]
PackageExport[RotationMapGraph]

PackageScope[ArchimedeanTessellation]
PackageScope[vertexFaceSizes]
PackageScope[detectRegularConfig]
PackageScope[mapFaces]
PackageScope[rectifyMapData]
PackageScope[truncateMapData]
PackageScope[canonicalConfiguration]
PackageScope[uniformDefect]
PackageScope[archimedeanSolidName]
PackageScope[rectifyMap]
PackageScope[truncateMap]
PackageScope[expandMap]
PackageScope[bevelMap]
PackageScope[growTessellation]
PackageScope[tessellationFacesGraph]
PackageScope[graphDistanceBall]
PackageScope[euclideanDisk]
PackageScope[hyperbolicDisk]
PackageScope[withGraphOptions]
PackageScope[sphericalDisk]
PackageScope[euclideanRectangle]
PackageScope[euclideanReflect]
PackageScope[hyperbolicReflect]
PackageScope[sphericalReflect]
PackageScope[uniformDisk]
PackageScope[uniformDiskHyperbolic]
PackageScope[growUniformTessellation]
PackageScope[completeVertexCorona]
PackageScope[regularLeftPolygon]
PackageScope[placeRegularPolygon]
PackageScope[uniformInteriorAngle]
PackageScope[solveUniformEdge]
PackageScope[mobiusDisk]
PackageScope[invMobiusDisk]
PackageScope[RegularMap]
PackageScope[SchlafliTessellation]
PackageScope[RegularMapsAt]
PackageScope[findGenerators]
PackageScope[sphericalGroup]
PackageScope[shapeName]
PackageScope[hyperbolicMap]
PackageScope[psl2Group]
PackageScope[mobiusPerm]
PackageScope[nthRegularMap]
PackageScope[cosetInv]
PackageScope[vonDyckRelators]
PackageScope[cosetTableClose]
PackageScope[lowIndexCosetTables]
PackageScope[cosetCanonicalTable]
PackageScope[uniformCycleQ]
PackageScope[regularActionQ]

(* Tessellations, encapsulated: every way this paclet builds a tiling graph.
   TessellationGraph -- compact quotients (regular {p, q} maps, uniform / Archimedean maps,
   flat tori); TessellationNeighborhoodGraph -- the unwrapped radius-r patch of the infinite
   tiling; TorusTessellation -- the flat-torus engine. Below them the machinery in the order
   used: map invariants, Conway operators and Archimedean maps, regular maps as coset graphs
   (SchlafliTessellation), and the general Todd-Coxeter coset enumeration. *)

(* ===================== The unified tessellation generator ===================== *)

(* TessellationGraph is the single public entry point for every map family below.
   {p, q} (a 2-integer Schlafli symbol) gives a regular map; a longer vertex
   configuration gives a uniform / Archimedean map. The second argument selects the
   realisation: an integer sizes it (n x n flat torus / n-th hyperbolic quotient),
   an integer pair {m, n} gives a rectangular flat torus, and a finite group or an
   explicit (r, s) generation carries the coset graph directly. *)
Options[TessellationGraph] = {Method -> Automatic};
TessellationGraph[{p_Integer, q_Integer}, n_Integer : 1, opts : OptionsPattern[{TessellationGraph, Graph}]] :=
  withGraphOptions[SchlafliTessellation[{p, q}, n, Sequence @@ FilterRules[{opts}, Options[SchlafliTessellation]]], opts];
TessellationGraph[{p_Integer, q_Integer}, {m_Integer, n_Integer}, opts : OptionsPattern[{TessellationGraph, Graph}]] :=
  withGraphOptions[TorusTessellation[{m, n}, shapeName[{p, q}]], opts];
TessellationGraph[{p_Integer, q_Integer}, grp_ /; ! MatchQ[grp, _Integer | {_Integer, _Integer} | _Rule | {___Rule}], opts : OptionsPattern[{TessellationGraph, Graph}]] :=
  withGraphOptions[RegularMap[{p, q}, grp], opts];
TessellationGraph[config_List /; Length[config] >= 3, n_Integer : 1, opts : OptionsPattern[{TessellationGraph, Graph}]] :=
  withGraphOptions[ArchimedeanTessellation[config, n], opts];

TessellationGraph::deferred =
  "The uniform map `1` is not built by this constructor (snub, elongated, and other non-Conway families are not yet supported).";

(* ===================== Torus tessellations ===================== *)

(* TorusTessellation[{m, n}, shape] returns the vertex-transitive flat-torus
   Cayley graph carrying the regular {p, q}-tessellation indicated by shape;
   shape defaults to "Triangular" (the most isotropic discrete plane).
     "Square"     -- {4, 4}, 4-regular, Cay(Z_m x Z_n, {+-e_1, +-e_2})
     "Triangular" -- {3, 6}, 6-regular, Cay(Z_m x Z_n, {+-e_1, +-e_2, +-(e_1+e_2)})
     "Hexagonal"  -- {6, 3}, 3-regular, two-orbit Cay on Z_m x Z_n x Z_2 *)

TorusTessellation[ { m_Integer, n_Integer }, opts : OptionsPattern[ ] ] :=
  TorusTessellation[ { m, n }, "Triangular", opts ]

TorusTessellation[ { m_Integer, n_Integer }, "Square", opts : OptionsPattern[ ] ] :=
  Graph[ GraphProduct[ CycleGraph[ m ], CycleGraph[ n ], "Cartesian" ], opts, VertexCoordinates -> Automatic ]

TorusTessellation[ { m_Integer, n_Integer }, "Triangular", opts : OptionsPattern[ ] ] :=
  Graph[
    Flatten @ Table[
      { { i, j } <-> { Mod[ i + 1, m ], j }
      , { i, j } <-> { i, Mod[ j + 1, n ] }
      , { i, j } <-> { Mod[ i + 1, m ], Mod[ j + 1, n ] } },
      { i, 0, m - 1 }, { j, 0, n - 1 }
    ],
    opts
  ]

TorusTessellation[ { m_Integer, n_Integer }, "Hexagonal", opts : OptionsPattern[ ] ] :=
  Graph[
    Flatten @ Table[
      { { i, j, 0 } <-> { i, j, 1 }
      , { i, j, 0 } <-> { Mod[ i - 1, m ], j, 1 }
      , { i, j, 0 } <-> { i, Mod[ j - 1, n ], 1 } },
      { i, 0, m - 1 }, { j, 0, n - 1 }
    ],
    opts
  ]




(* ===================== Unwrapped tessellation patches ===================== *)

(* TessellationNeighborhoodGraph[{p, q}, r] is the radius-r graph-distance ball cut from the
   infinite regular {p, q} tessellation of its covering surface -- the non-compact
   companion to TessellationGraph (which wraps the tiling onto a compact torus / coset
   quotient). One ring-growth skeleton, dispatched by curvature (p-2)(q-2): the
   Euclidean plane (== 4), the hyperbolic plane (> 4), or the closing-up sphere (< 4);
   interior vertices have degree q, the cut boundary fewer. *)
TessellationNeighborhoodGraph[{p_Integer, q_Integer}, r_Integer : 3, opts : OptionsPattern[Graph]] :=
  withGraphOptions[
    With[{c = (p - 2) (q - 2)},
      Which[c < 4, sphericalDisk[p, q, r], c == 4, euclideanDisk[p, q, r], True, hyperbolicDisk[p, q, r]]],
    opts];

TessellationNeighborhoodGraph[{p_Integer, q_Integer}, {m_Integer, n_Integer}, opts : OptionsPattern[Graph]] :=
  If[(p - 2) (q - 2) == 4, withGraphOptions[euclideanRectangle[p, q, m, n], opts],
    Message[TessellationNeighborhoodGraph::eucrect, {p, q}]; $Failed];

TessellationNeighborhoodGraph::eucrect =
  "The rectangular form is defined only for a Euclidean {p,q} ((p-2)(q-2)==4); `1` is not Euclidean -- use an integer radius.";

(* a vertex configuration (longer than a {p, q} Schlafli symbol) cuts the radius-r ball from
   the infinite uniform / Archimedean tiling of that configuration -- the same ball, grown one
   corona at a time. One corona engine for both flat and curved cases (see growUniformTessellation),
   dispatched on the angle defect Sum 1/f_i - (k-2)/2: a regular (all-equal) configuration forwards
   to the {p, q} engine; a Euclidean uniform tiling (defect 0) grows in the plane; a hyperbolic one
   (defect < 0) grows in the Poincare disk; a spherical one (defect > 0) is the finite Archimedean
   solid, whose ball saturates to the whole solid. *)
TessellationNeighborhoodGraph[config_List /; Length[config] >= 3, r_Integer : 3, opts : OptionsPattern[Graph]] :=
  withGraphOptions[
    With[{defect = uniformDefect @ config},
      Which[
        Equal @@ config, TessellationNeighborhoodGraph[{First @ config, Length @ config}, r],
        defect == 0 && MemberQ[$euclideanUniformConfigs, canonicalConfiguration @ config], uniformDisk[config, r],
        defect < 0, uniformDiskHyperbolic[config, r],
        defect > 0, With[{g = ArchimedeanTessellation[config]},
          If[GraphQ[g], NeighborhoodGraph[g, First @ VertexList @ g, r], $Failed]],
        True, Message[TessellationNeighborhoodGraph::deferred, config]; $Failed]],
    opts];

(* the Euclidean uniform tilings the corona-growth engine builds (canonical configurations);
   the snub / elongated families are chiral and stay deferred, matching TessellationGraph *)
$euclideanUniformConfigs = {{3, 6, 3, 6}, {3, 4, 6, 4}, {4, 6, 12}, {4, 8, 8}, {3, 12, 12}};

TessellationNeighborhoodGraph::deferred =
  "The unwrapped patch of the uniform tiling `1` is not built (the Euclidean snub / elongated families are chiral -- use TessellationGraph for their compact quotient).";


(* ===================== The ring-growth engine ===================== *)

(* grow the tiling one reflection ring at a time: each face is the list of its vertex
   coordinates, reflected across each edge to its neighbour; keep a neighbour only when
   its centroid lies in the (bounded) region, so the growth self-terminates. centKey
   snaps centroids to dedup faces reached along different reflection paths. *)
growTessellation[seed_, reflect_, centKey_, inRegion_] :=
  Module[{p = Length @ seed, faces = {seed}, frontier = {seed}, seen = <|centKey[seed] -> True|>, new},
    While[frontier =!= {},
      new = Flatten[Reap[Do[Do[
          With[{nf = reflect[f[[i]], f[[Mod[i, p] + 1]], #] & /@ f}, {k = centKey @ nf},
            If[inRegion @ Mean @ nf && ! KeyExistsQ[seen, k], seen[k] = True; Sow @ nf]],
        {i, p}], {f, frontier}]][[2]], 1];
      faces = Join[faces, new]; frontier = new];
    faces];

(* assemble the 1-skeleton from the grown faces: snap corners to a grid (coincident
   corners agree to ~1e-12, distinct ones are separated by >> tol), so equal vertices
   merge; vertices keep their (averaged) geometric coordinates *)
tessellationFacesGraph[faces_, toVec_, tol_] :=
  With[{vecs = toVec /@ Flatten[faces, 1]},
    {keys = Round[vecs, tol]},
    {uniq = DeleteDuplicates @ keys},
    {idOf = AssociationThread[uniq -> Range @ Length @ uniq]},
    {fids = Lookup[idOf, #] & /@ TakeList[keys, Length /@ faces]},
    {edges = DeleteCases[
       DeleteDuplicates @ Flatten[
         (fc |-> UndirectedEdge @@ Sort[#] & /@ Partition[Append[fc, First @ fc], 2, 1]) /@ fids],
       _?(Apply[SameQ])]},
    Graph[Range @ Length @ uniq, edges,
      VertexCoordinates -> Lookup[GroupBy[Transpose[{keys, vecs}], First -> Last, Mean], uniq]]];

(* the graph-distance-r ball around the vertex closest to ref *)
graphDistanceBall[g_, ref_, r_] :=
  NeighborhoodGraph[g,
    VertexList[g][[First @ Ordering[SquaredEuclideanDistance[ref, #] & /@ GraphEmbedding @ g, 1]]], r];


(* ===================== Per-curvature backends ===================== *)

(* reflect z across the line through a, b in the plane *)
euclideanReflect[a_, b_, z_] := a + (b - a) Conjugate[(z - a)/(b - a)];

(* reflect z across the geodesic through interior points a, b of the Poincare disk:
   inversion in the orthogonal circle, or a diameter reflection when a, b, 0 are collinear *)
hyperbolicReflect[a_, b_, z_] :=
  With[{det = Im[Conjugate[a] b]},
    If[Abs[det] < 10.^-12,
      Exp[2 I Arg[a]] Conjugate[z],
      With[{c = ((Abs[a]^2 + 1) Im[b] - (Abs[b]^2 + 1) Im[a])/(2 det) +
               I ((Abs[b]^2 + 1) Re[a] - (Abs[a]^2 + 1) Re[b])/(2 det)},
        c + (Abs[c]^2 - 1)/Conjugate[z - c]]]];

(* reflect z across the great-circle plane of edge a, b on the unit sphere *)
sphericalReflect[a_, b_, z_] := With[{n = Normalize @ Cross[a, b]}, z - 2 (z . n) n];

(* Euclidean {p,q} ((p-2)(q-2)==4): unit regular p-gon, ordinary line reflections; the
   graph-distance-r ball lies within Euclidean distance r*edge of the centre vertex *)
euclideanDisk[p_, q_, r_] :=
  With[{seed = N @ Table[Exp[I 2 Pi k/p], {k, 0, p - 1}]}, {e = 2 Sin[Pi/p]},
    graphDistanceBall[
      tessellationFacesGraph[
        growTessellation[seed, euclideanReflect, Round[Mean @ #, 10.^-6] &, Abs[#] <= 1 + (r + 1) e &],
        {Re @ #, Im @ #} &, 10.^-6],
      {0, 0}, r]];

(* hyperbolic {p,q} ((p-2)(q-2)>4): regular p-gon of circumradius cosh R = cot(pi/p)cot(pi/q)
   in the Poincare disk; B_r lies within hyperbolic distance R + r*edge of the centre *)
hyperbolicDisk[p_, q_, r_] :=
  With[{cc = Cot[Pi/p] Cot[Pi/q]}, {r0 = Tanh[ArcCosh[cc]/2], rr = ArcCosh[cc]},
    {seed = N @ Table[r0 Exp[I 2 Pi k/p], {k, 0, p - 1}]},
    {elen = ArcCosh[1 + 2 Abs[seed[[1]] - seed[[2]]]^2/((1 - Abs[seed[[1]]]^2) (1 - Abs[seed[[2]]]^2))]},
    {rho = rr + (r + 1) elen},
    graphDistanceBall[
      tessellationFacesGraph[
        growTessellation[seed, hyperbolicReflect, Round[Mean @ #, 10.^-5] &, 2 ArcTanh[Abs[#]] <= rho &],
        {Re @ #, Im @ #} &, 10.^-5],
      {0, 0}, r]];

(* spherical {p,q} ((p-2)(q-2)<4): regular p-gon of angular circumradius cos R = cot(pi/p)cot(pi/q)
   on the unit sphere; reflections across edge great-circle planes close the tiling up into the
   finite Platonic graph -- B_r is an open cap for small r, the whole solid once r reaches the diameter *)
sphericalDisk[p_, q_, r_] :=
  With[{cR = Cot[Pi/p] Cot[Pi/q]}, {sR = Sqrt[1 - cR^2]},
    {seed = N @ Table[{sR Cos[2 Pi k/p], sR Sin[2 Pi k/p], cR}, {k, 0, p - 1}]},
    graphDistanceBall[
      tessellationFacesGraph[
        growTessellation[seed, sphericalReflect, Round[Mean @ #, 10.^-5] &, True &],
        Identity, 10.^-3],
      {0, 0, 1}, r]];

(* ===================== Uniform / Archimedean corona growth ===================== *)

(* One corona engine for the uniform tilings of all three constant-curvature model geometries,
   in the conformal planar picture: distances live in the plane (curvature parameter u = 1, unit
   edges) or the Poincare disk (u = cosh(s/2) > 1, edge length s). Angles are veridical in either,
   so the corona combinatorics are identical; only the polygon placement, the direction at a
   vertex, and the edge step are geometry-specific (keyed on u). The interior angle of a regular
   f-gon is the single formula 2 ArcSin[Cos[Pi/f] / u] (u = 1 recovers the Euclidean (f-2) Pi / f),
   and the shared edge length solves Sum_i interior(f_i) = 2 Pi. *)

(* Euclidean uniform disk: grow to a margin past radius r (graph distance >= Euclidean distance
   for unit edges, so B_r is contained), then cut B_r *)
uniformDisk[config_, r_] :=
  graphDistanceBall[
    tessellationFacesGraph[
      growUniformTessellation[config, 1, Abs[#] <= r + 2.5 &], {Re @ #, Im @ #} &, 10.^-5],
    {0, 0}, r];

(* hyperbolic uniform disk: solve the common edge length, grow in the Poincare disk to a margin
   past hyperbolic radius r * s (edges have hyperbolic length s), then cut B_r *)
uniformDiskHyperbolic[config_, r_] :=
  With[{u = solveUniformEdge[config]}, {s = 2 ArcCosh[u]},
    graphDistanceBall[
      tessellationFacesGraph[
        growUniformTessellation[config, u, Abs[#] < 1 && 2 ArcTanh[Abs[#]] <= (r + 2) s &], {Re @ #, Im @ #} &, 10.^-5],
      {0, 0}, r]];

(* interior angle of a regular f-gon of edge length s in curvature -1 / 0 / +1, u = cosh(s/2) /
   1 / cos(s/2); the common edge length of the tiling makes the corners sum to 2 Pi *)
uniformInteriorAngle[f_, u_] := 2 ArcSin[Cos[Pi / f] / u];
solveUniformEdge[config_] := Re[u /. FindRoot[Total[uniformInteriorAngle[#, u] & /@ config] == 2 Pi, {u, 1.3}]];

(* disk isometries (0 <-> a) used by the hyperbolic placement and direction *)
mobiusDisk[a_, z_] := (z + a) / (1 + Conjugate[a] z);
invMobiusDisk[a_, z_] := (z - a) / (1 - Conjugate[a] z);

(* the regular f-gon (edge length set by u) sharing directed edge a -> b and lying to its left.
   Euclidean (u = 1): each corner turns left by the exterior angle 2 Pi / f. Hyperbolic: transport
   a standard origin-centred f-gon (circumradius from sinh R = sinh(s/2) / sin(Pi/f)) by the disk
   isometry carrying its first edge onto a -> b. *)
regularLeftPolygon[a_, b_, f_] :=
  FoldList[Plus, a, Table[(b - a) Exp[I 2. Pi k / f], {k, 0, f - 2}]];
placeRegularPolygon[a_, b_, f_, 1] := regularLeftPolygon[a, b, f];
placeRegularPolygon[a_, b_, f_, u_] :=
  With[{rho = Tanh[ArcSinh[Sinh[ArcCosh[u]] / Sin[Pi / f]] / 2]},
    {std = Table[rho Exp[I 2. Pi j / f], {j, 0, f - 1}]},
    {theta = Arg[invMobiusDisk[a, b]] - Arg[invMobiusDisk[std[[1]], std[[2]]]]},
    Table[mobiusDisk[a, Exp[I theta] invMobiusDisk[std[[1]], std[[j]]]], {j, 1, f}]];

(* direction of the edge v -> w and the neighbour one edge-step from v in direction dir:
   Euclidean tangents are differences; hyperbolic ones are read through the isometry sending v -> 0 *)
vertexDirection[v_, w_, 1] := Arg[w - v];
vertexDirection[v_, w_, u_] := Arg[invMobiusDisk[v, w]];
edgeStep[v_, dir_, 1, s_] := v + Exp[I dir];
edgeStep[v_, dir_, u_, s_] := mobiusDisk[v, Tanh[s / 2] Exp[I dir]];

(* grow the uniform tiling corona by corona: seed the full corona of the origin vertex (the
   k polygons of config in CCW order, interior angles summing to 2 Pi), then repeatedly complete
   every boundary vertex whose partial corona pins its configuration, keeping only faces whose
   centroid lies in the region so the growth self-terminates *)
growUniformTessellation[config_, u_, inRegion_] :=
  Module[{s = If[u === 1, 1, 2 ArcCosh[u]], cumAngle, faces, seen, growing = True, added},
    cumAngle = Most @ Prepend[Accumulate[uniformInteriorAngle[#, u] & /@ config], 0.];
    faces = Select[MapThread[placeRegularPolygon[0, edgeStep[0, #2, u, s], #1, u] &, {config, cumAngle}], inRegion[Mean @ #] &];
    seen = Association[(Round[Mean @ #, 10.^-5] -> True) & /@ faces];
    While[growing, growing = False;
      Do[added = completeVertexCorona[corner[[1, 1]], corner, config, u, s, inRegion, seen];
        If[added =!= {},
          faces = Join[faces, added]; (seen[Round[Mean @ #, 10.^-5]] = True) & /@ added; growing = True],
        {corner, Values @ GroupBy[
          Flatten[(poly |-> With[{n = Length @ poly},
            Table[With[{v = poly[[i]], nxt = poly[[Mod[i, n] + 1]], prv = poly[[Mod[i - 2, n] + 1]]},
              {Round[{Re @ v, Im @ v}, 10.^-5], v, n, vertexDirection[v, nxt, u], vertexDirection[v, prv, u]}], {i, n}]]) /@ faces, 1],
          First -> Rest]}]];
    faces];

(* fill the angular gap around vertex v: `corner` lists {v, faceSize, startAngle, endAngle}
   for each placed face (each face sweeps CCW from startAngle to endAngle). When the placed sizes,
   read CCW from the open boundary, match a unique rotation/reflection of config, the remaining
   sizes of that rotation are placed CCW from the exposed edge; an ambiguous or complete corona
   adds nothing *)
completeVertexCorona[v_, corner_, config_, u_, s_, inRegion_, seen_] :=
  With[{sizes = corner[[All, 2]], starts = corner[[All, 3]], ends = corner[[All, 4]]},
    If[2 Pi - Total[uniformInteriorAngle[#, u] & /@ sizes] < 0.01, {},
      With[
        {begin = SelectFirst[starts, angle |-> NoneTrue[ends, Abs[Mod[angle - # + Pi, 2 Pi] - Pi] < 10.^-3 &]]},
        {order = SortBy[Range @ Length @ sizes, Mod[starts[[#]] - begin, 2 Pi] &]},
        {endAngle = ends[[Last @ order]],
         remaining = DeleteDuplicates @ Cases[
           Join @@ (Table[RotateLeft[#, i], {i, 0, Length @ config - 1}] & /@ {config, Reverse @ config}),
           seq_ /; Take[seq, Length @ order] === sizes[[order]] :> Drop[seq, Length @ order]]},
        If[Length @ remaining != 1, {},
          Last @ Fold[
            {state, size} |-> With[{poly = placeRegularPolygon[v, First @ state, size, u]},
              {poly[[-1]],
               If[! KeyExistsQ[seen, Round[Mean @ poly, 10.^-5]] && inRegion[Mean @ poly],
                 Append[Last @ state, poly], Last @ state]}],
            {edgeStep[v, endAngle, u, s], {}}, First @ remaining]]]]];


(* m x n rectangular patch of the Euclidean {p,q} tiling: the flat-torus construction with the
   wrap-around identifications dropped, so the patch has a boundary; dangling degree-1 hexagonal
   sites are trimmed *)
euclideanRectangle[4, 4, m_, n_] := GridGraph[{m, n}];

euclideanRectangle[3, 6, m_, n_] :=
  With[{verts = Flatten[Table[{i, j}, {i, 0, m}, {j, 0, n}], 1]},
    Graph[
      Flatten @ Table[
        {If[i < m, {i, j} <-> {i + 1, j}, Nothing],
         If[j < n, {i, j} <-> {i, j + 1}, Nothing],
         If[i < m && j < n, {i, j} <-> {i + 1, j + 1}, Nothing]},
        {i, 0, m}, {j, 0, n}],
      VertexCoordinates -> (v |-> v -> {v[[1]] + v[[2]]/2, v[[2]] Sqrt[3]/2}) /@ verts]];

euclideanRectangle[6, 3, m_, n_] :=
  With[{a0 = {-Sqrt[3]/2, 3/2}, a1 = {Sqrt[3]/2, 3/2}, d = {0, 1}},
    With[{g = Graph[
        Flatten @ Table[
          {{i, j, 0} <-> {i, j, 1},
           If[i > 0, {i, j, 0} <-> {i - 1, j, 1}, Nothing],
           If[j > 0, {i, j, 0} <-> {i, j - 1, 1}, Nothing]},
          {i, 0, m}, {j, 0, n}],
        VertexCoordinates -> Flatten[Table[
          {{i, j, 0} -> i a0 + j a1, {i, j, 1} -> i a0 + j a1 + d}, {i, 0, m}, {j, 0, n}], 2]]},
      Subgraph[g, Select[VertexList @ g, VertexDegree[g, #] > 1 &]]]];




(* ===================== Map invariants: curvature, Euler characteristic, genus ===================== *)

(* the per-vertex face-size list: a Schlafli {p, q} has q p-gons around each vertex; a
   uniform configuration already is that cyclic list. Matches TessellationGraph's own
   dispatch (2-list = regular symbol, longer list = vertex configuration). *)
vertexFaceSizes[{p_Integer, q_Integer}] := ConstantArray[p, q];
vertexFaceSizes[config_List /; Length[config] >= 3] := config;

(* the face sizes read off the graph, assuming a REGULAR map: uniform degree q and every
   face the girth p, so the configuration is q copies of p. The default when no spec is
   passed. Mixed-face (Archimedean) maps are not recoverable from the abstract graph --
   the girth only sees their smallest face -- so pass their configuration explicitly. *)
detectRegularConfig[g_Graph] := ConstantArray[mapGirth @ g, First @ Union @ VertexDegree @ g];

(* combinatorial (angle-defect) Gaussian curvature at a vertex of the map:
   kappa = Sum 1/f_i - (k - 2)/2; sign is spherical / flat / hyperbolic and the geometric
   angle defect is 2 Pi kappa. Depends only on the local configuration, not the realisation.
   With a graph and no spec, the regular configuration is detected from the graph. *)
TessellationCurvature[spec_List] := uniformDefect[vertexFaceSizes[spec]];
TessellationCurvature[g_Graph] := TessellationCurvature[detectRegularConfig[g]];

(* Euler characteristic of the closed map, V - E + F read off the realised graph: each
   f-gon owns f of the V*k vertex-face corners, so F = V Sum 1/f_i (works for the mixed
   faces of an Archimedean map). Discrete Gauss-Bonnet gives the same value as V kappa.
   The spec defaults to the regular configuration detected from the graph. *)
TessellationEulerCharacteristic[graph_Graph, spec_List] :=
  VertexCount[graph] - EdgeCount[graph] + VertexCount[graph] Total[1 / vertexFaceSizes[spec]];
TessellationEulerCharacteristic[graph_Graph] := TessellationEulerCharacteristic[graph, detectRegularConfig[graph]];

(* orientable genus from the Euler characteristic: g = (2 - chi)/2 *)
TessellationGenus[graph_Graph, spec_List] := (2 - TessellationEulerCharacteristic[graph, spec]) / 2;
TessellationGenus[graph_Graph] := (2 - TessellationEulerCharacteristic[graph]) / 2;

(* Uniform / Archimedean maps: vertex-transitive tilings by regular polygons of several
   sizes, the vertex-transitive generalisation of the flag-transitive regular maps below. A uniform map has a vertex configuration (f_1. ... .f_k) -- the cyclic
   sequence of face sizes around every vertex. See Wiki/Concepts/UniformMaps.md.
   Refs: Grunbaum & Shephard, Tilings and Patterns (1987); Coxeter, Regular Polytopes;
   Conway, Burgiel & Goodman-Strauss, The Symmetries of Things (2008) -- Conway operators. *)

(* ===================== Conway operators (internal engine) ===================== *)

(* the Conway operators that build the Euclidean uniform tilings from a flat-torus seed;
   internal -- ArchimedeanTessellation is the only public entry point *)

(* rectification (ambo): the medial graph on the edges; takes {p, q} to (p.q.p.q) *)
rectifyMap[g_] := First @ rectifyMapData @ mapData @ g;

(* truncation: each degree-q vertex becomes a q-gon, each p-gon face a 2p-gon; {p,q} to (q.2p.2p) *)
truncateMap[g_] := First @ truncateMapData @ mapData @ g;

(* expansion (cantellation) = ambo of ambo; takes {p, q} to (p.4.q.4) *)
expandMap[g_] := First @ rectifyMapData @ rectifyMapData @ mapData @ g;

(* bevel (omnitruncation) = truncation of the rectification; takes {p, q} to (4.2p.2q) *)
bevelMap[g_] := First @ truncateMapData @ rectifyMapData @ mapData @ g;

(* ===================== Archimedean tessellations ===================== *)

(* the uniform map of vertex configuration config, sized n; curvature dispatch on the
   angle defect Sum 1/f_i vs (k-2)/2: spherical (Archimedean solids + prisms/antiprisms),
   Euclidean (uniform plane tilings on the torus), regular (all f_i equal) forwards to
   SchlafliTessellation *)
ArchimedeanTessellation[config_List, n_ : 1] :=
  Which[
    Equal @@ config, SchlafliTessellation[{First @ config, Length @ config}, n],
    uniformDefect[config] > 0, sphericalUniform[config],
    uniformDefect[config] == 0, euclideanUniform[canonicalConfiguration @ config, n],
    True, hyperbolicUniform[config, n]];

uniformDefect[c_] := Total[1/c] - (Length[c] - 2)/2;

sphericalUniform[config_] :=
  With[{key = canonicalConfiguration @ config},
    Which[
      KeyExistsQ[archimedeanSolidName, key], PolyhedronData[archimedeanSolidName[key], "SkeletonGraph"],
      Count[config, 4] == 2 && Length[config] == 3, prismGraph[First @ DeleteCases[config, 4]],
      Count[config, 3] == 3 && Length[config] == 4, antiprismGraph[First @ DeleteCases[config, 3]],
      True, Message[TessellationGraph::deferred,config]; $Failed]];

(* the seed torus is taken at size >= 5 so wraparound loops exceed the girth and the
   girth-cycle face recovery returns the true faces, not non-contractible cycles *)
euclideanUniform[key_, n_] :=
  With[{m = Max[n, 5]},
    Switch[key,
      {3, 6, 3, 6}, rectifyMap @ TorusTessellation[{m, m}, "Triangular"],
      {3, 4, 6, 4}, expandMap @ TorusTessellation[{m, m}, "Triangular"],
      {4, 6, 12}, bevelMap @ TorusTessellation[{m, m}, "Triangular"],
      {4, 8, 8}, truncateMap @ TorusTessellation[{m, m}, "Square"],
      {3, 12, 12}, truncateMap @ TorusTessellation[{m, m}, "Hexagonal"],
      _, Message[TessellationGraph::deferred,key]; $Failed]];

(* hyperbolic uniform maps: the same Conway operators, now applied to a hyperbolic
   regular seed SchlafliTessellation[{p, q}, n] instead of a flat torus. The vertex
   configuration is inverted to its generating (operator, seed {p, q}); snub / elongated
   families are not Conway images of a regular map and stay deferred. *)
hyperbolicUniform[config_, n_] :=
  With[{seed = conwaySeed @ canonicalConfiguration @ config},
    If[seed === $Failed,
      Message[TessellationGraph::deferred, config]; $Failed,
      First[seed] @ SchlafliTessellation[Last[seed], n]]];

(* invert the Conway formulas: rectify {p,q}=(p.q.p.q), truncate {p,q}=(q.2p.2p),
   expand {p,q}=(p.4.q.4), bevel {p,q}=(4.2p.2q) *)
conwaySeed[c_] := Which[
  Length[c] == 4 && c[[1]] == c[[3]] && c[[2]] == c[[4]], {rectifyMap, {c[[1]], c[[2]]}},
  Length[c] == 4 && c[[2]] == 4 && c[[4]] == 4, {expandMap, {c[[1]], c[[3]]}},
  Length[c] == 3 && MemberQ[c, 4], {bevelMap, DeleteCases[c, 4]/2},
  Length[c] == 3 && Length[Union @ c] == 2,
    {truncateMap, {First[Select[c, Count[c, #] == 2 &]]/2, First[Select[c, Count[c, #] == 1 &]]}},
  True, $Failed];

(* n-gonal prism: two n-cycles joined by rungs, vertex configuration (4.4.n) *)
prismGraph[n_] := Graph @ Join[
   Table[UndirectedEdge[{1, i}, {1, Mod[i, n] + 1}], {i, n}],
   Table[UndirectedEdge[{2, i}, {2, Mod[i, n] + 1}], {i, n}],
   Table[UndirectedEdge[{1, i}, {2, i}], {i, n}]];

(* n-gonal antiprism: two n-cycles joined by a triangle band, vertex configuration (3.3.3.n) *)
antiprismGraph[n_] := Graph @ Join[
   Table[UndirectedEdge[{1, i}, {1, Mod[i, n] + 1}], {i, n}],
   Table[UndirectedEdge[{2, i}, {2, Mod[i, n] + 1}], {i, n}],
   Table[UndirectedEdge[{1, i}, {2, i}], {i, n}],
   Table[UndirectedEdge[{1, i}, {2, Mod[i, n] + 1}], {i, n}]];

(* ===================== Map data: graph + face cycles ===================== *)

mapData[g_] := {g, mapFaces[g]};

(* the faces of a regular-map 1-skeleton: all shortest (girth-length) cycles *)
mapFaces[g_] := cycleVertices /@ FindCycle[g, {mapGirth[g]}, All];

mapGirth[g_] := First @ Select[Range[3, EdgeCount[g] + 1], FindCycle[g, {#}, 1] =!= {} &, 1];

cycleVertices[es_] :=
  With[{vs = List @@@ es},
    {start = If[MemberQ[vs[[2]], vs[[1, 2]]], vs[[1, 1]], vs[[1, 2]]]},
    Most @ FoldList[{prev, e} |-> First @ Complement[e, {prev}], start, vs]];

edgeKey[a_, b_] := Sort[{a, b}];

faceToEdges[fc_] := edgeKey @@@ Partition[Append[fc, First @ fc], 2, 1];

boundaryEdges[cyc_] := With[{m = Length[cyc]}, Table[UndirectedEdge[cyc[[i]], cyc[[Mod[i, m] + 1]]], {i, m}]];

(* cyclic order of the edges incident to v, chained from the face corners at v *)
vertexRotation[v_, efs_] :=
  With[
    {prs = DeleteDuplicates @ Flatten[
        (fl |-> Cases[Transpose[{fl, RotateLeft[fl]}],
            {a_, b_} /; MemberQ[a, v] && MemberQ[b, v] :> Sort[{a, b}]]) /@ efs, 1]},
    {es = DeleteDuplicates @ Flatten[prs, 1]},
    cycleVertices @ First @ FindCycle[Graph[UndirectedEdge @@@ prs], {Length @ es}, 1]];

(* {graph, faces} of the rectification: original faces (on edge-vertices) + vertex figures *)
rectifyMapData[{g_, fcs_}] :=
  With[{efs = faceToEdges /@ fcs},
    {faces = Join[efs, vertexRotation[#, efs] & /@ VertexList[g]]},
    {SimpleGraph @ Graph @ Flatten[boundaryEdges /@ faces], faces}];

(* {graph, faces} of the truncation: vertex polygons (on darts {v, e}) + face 2k-gons *)
truncateMapData[{g_, fcs_}] :=
  With[{efs = faceToEdges /@ fcs},
    {vpoly = (v |-> ({v, #} & /@ vertexRotation[v, efs])) /@ VertexList[g],
     fpoly = (f |-> With[{m = Length @ f, e = faceToEdges @ f},
          Flatten[Table[{{f[[i]], e[[i]]}, {f[[Mod[i, m] + 1]], e[[i]]}}, {i, m}], 1]]) /@ fcs},
    {faces = Join[vpoly, fpoly]},
    {SimpleGraph @ Graph @ Flatten[boundaryEdges /@ faces], faces}];

canonicalConfiguration[c_] := First @ Sort @ Join[
   Table[RotateLeft[c, i], {i, 0, Length[c] - 1}],
   Table[RotateLeft[Reverse @ c, i], {i, 0, Length[c] - 1}]];

archimedeanSolidName = <|
  {3, 4, 3, 4} -> "Cuboctahedron", {3, 5, 3, 5} -> "Icosidodecahedron",
  {3, 6, 6} -> "TruncatedTetrahedron", {3, 8, 8} -> "TruncatedCube",
  {4, 6, 6} -> "TruncatedOctahedron", {3, 10, 10} -> "TruncatedDodecahedron",
  {5, 6, 6} -> "TruncatedIcosahedron", {3, 4, 4, 4} -> "SmallRhombicuboctahedron",
  {4, 6, 8} -> "GreatRhombicuboctahedron", {3, 4, 5, 4} -> "SmallRhombicosidodecahedron",
  {4, 6, 10} -> "GreatRhombicosidodecahedron", {3, 3, 3, 3, 4} -> "SnubCube",
  {3, 3, 3, 3, 5} -> "SnubDodecahedron"|>;




(* Regular maps as coset graphs of (2,p,q)-generated groups; see Wiki/Concepts/RegularMaps.md.
   Refs: Coxeter & Moser, Generators and Relations for Discrete Groups (1957);
   Jones & Singerman, Theory of maps on orientable surfaces (1978);
   Conder's census of regular maps (math.auckland.ac.nz/~conder). *)


(* ===================== Regular maps via coset graphs ===================== *)

(* 1-skeleton of the regular map of type {p, q} carried by the (2,p,q)-generation
   r^p == s^q == (rs)^2 == 1: the coset graph on the vertex cosets G/<s>, joined by
   the edge-involution rs; q-regular with girth p, V == |G|/q, E == |G|/2, F == |G|/p *)
RegularMap[{p_, q_}, {r_Cycles, s_Cycles}] :=
  With[
    {t = PermutationProduct[r, s], deg = Max[PermutationMax[r], PermutationMax[s]]},
    {sub = PermutationPower[s, #] & /@ Range[0, q - 1],
     els = GroupElements[PermutationGroup[{r, s}]]},
    {key = g |-> Sort[PermutationList[PermutationProduct[g, #], deg] & /@ sub]},
    {reps = DeleteDuplicatesBy[els, key]},
    {idx = AssociationThread[key /@ reps -> Range[Length[reps]]]},
    Graph[Range[Length[reps]],
      DeleteCases[
        DeleteDuplicates @ Flatten @ Table[
          UndirectedEdge @@ Sort[{idx[key[g]],
             idx[key[PermutationProduct[g, PermutationPower[s, i], t]]]}],
          {g, reps}, {i, 0, q - 1}],
        _?(Apply[SameQ])]]];

RegularMap[{p_, q_}, grp_] := RegularMap[{p, q}, findGenerators[grp, p, q]];

(* one graph per isomorphism class of {p, q} map carried by PSL(2, ell), ell prime --
   all (2,p,q)-generations at level ell (exposes the several maps at one Hurwitz level) *)
RegularMapsAt[{p_, q_}, ell_?PrimeQ] :=
  With[{e = GroupElements[psl2Group[ell]], ord = GroupOrder[psl2Group[ell]]},
    DeleteDuplicates[
      RegularMap[{p, q}, #] & /@ Select[
        Tuples[{Select[e, PermutationOrder[#] == p &], Select[e, PermutationOrder[#] == q &]}],
        PermutationOrder[PermutationProduct[#[[1]], #[[2]]]] == 2 &&
         GroupOrder[PermutationGroup[#]] == ord &],
      IsomorphicGraphQ]];

(* n-th smallest regular map of type {p, q}, dispatched by Method. The default Automatic uses the
   fast realiser per curvature -- finite group (spherical), flat torus (Euclidean), PSL(2,ell)
   congruence quotient (hyperbolic) -- each of which is the map the general enumeration would return;
   if no realiser applies (a hyperbolic {p, q} with no congruence family) it falls back to the general
   coset enumeration at a small index budget. The realisers are also selectable directly:
   "Platonic" | "Torus" | "PSL2" force one path; "CosetEnumeration" (with the "MaxIndex" sub-option)
   forces the general low-index method, curvature-agnostic but feasible in pure WL only at small index. *)
Options[SchlafliTessellation] = {Method -> Automatic};

SchlafliTessellation[{p_, q_}, n_Integer : 1, opts : OptionsPattern[]] :=
  With[{method = OptionValue[Method], c = (p - 2) (q - 2)},
    With[{name = If[ListQ[method], First[method], method],
      budget = If[ListQ[method], Lookup[Rest[method], "MaxIndex", 24],
                  If[c < 4, 4 p q / (2 p + 2 q - p q), 24]]},
      Switch[name,
        Automatic,
          Which[
            c < 4,  RegularMap[{p, q}, sphericalGroup[{p, q}]],
            c == 4, TorusTessellation[{n, n}, shapeName[{p, q}]],
            True,   With[{psl = hyperbolicMap[{p, q}, n]}, If[psl =!= $Failed, psl, nthRegularMap[{p, q}, n, 24]]]],
        "Platonic",            RegularMap[{p, q}, sphericalGroup[{p, q}]],
        "Torus",               TorusTessellation[{n, n}, shapeName[{p, q}]],
        "PSL2" | "Congruence", hyperbolicMap[{p, q}, n],
        "CosetEnumeration",    nthRegularMap[{p, q}, n, budget],
        _, Message[SchlafliTessellation::badmethod, name]; $Failed]]];

SchlafliTessellation::badmethod = "Unknown Method `1`; use Automatic, \"Platonic\", \"Torus\", \"PSL2\", or \"CosetEnumeration\".";

(* n-th smallest regular (normal-subgroup) {p,q} map by index, from the general coset enumeration
   (LowIndexMaps below), built via the coset-graph builder RegularMap;
   $Failed if fewer than n regular maps occur up to index maxIndex *)
nthRegularMap[{p_, q_}, n_, maxIndex_] := Module[
  {maps = SortBy[Select[LowIndexMaps[p, q, maxIndex], #["Regular"] &], #["Index"] &]},
  If[Length[maps] < n, $Failed, RegularMap[{p, q}, maps[[n]]["Generators"]]]];


(* ===================== Sourcing the group (r, s) ===================== *)

(* first (r, s) of orders p, q with r s an involution that together generate grp;
   scans only the order-p x order-q elements (not all |G|^2 pairs), stops at the first hit *)
findGenerators[grp_, p_, q_] :=
  With[{ord = GroupOrder[grp], e = GroupElements[grp]},
    Module[{rs = Select[e, PermutationOrder[#] == p &], ss = Select[e, PermutationOrder[#] == q &]},
      Catch[
        Do[
          If[PermutationOrder[PermutationProduct[r, s]] == 2 &&
             GroupOrder[PermutationGroup[{r, s}]] == ord,
            Throw[{r, s}]],
          {r, rs}, {s, ss}];
        Missing["NotFound"]]]];

sphericalGroup[{3, 3}] := AlternatingGroup[4];
sphericalGroup[{3, 4}] := SymmetricGroup[4];
sphericalGroup[{4, 3}] := SymmetricGroup[4];
sphericalGroup[{3, 5}] := AlternatingGroup[5];
sphericalGroup[{5, 3}] := AlternatingGroup[5];

shapeName = <|{4, 4} -> "Square", {3, 6} -> "Triangular", {6, 3} -> "Hexagonal"|>;

(* n-th smallest PSL(2,ell) carrying a (2,p,q)-generation, by increasing prime ell.
   ell is capped because the brute-force generator search makes large PSL(2,ell)
   impractical; $Failed if the n-th map is not reached below the cap (e.g. n too large,
   or a {p,q} realisable only over prime-power fields, which this builder does not cover). *)
hyperbolicMap[{p_, q_}, n_] :=
  Module[{found = {}, ell = 1, g},
    While[Length[found] < n && ell < 50,
      ell = NextPrime[ell];
      g = findGenerators[psl2Group[ell], p, q];
      If[Head[g] === List, AppendTo[found, g]]];
    If[Length[found] < n, $Failed, RegularMap[{p, q}, Last[found]]]];

psl2Group[ell_] :=
  PermutationGroup[{mobiusPerm[ell, {1, 1, 0, 1}], mobiusPerm[ell, {0, -1, 1, 0}]}];

(* the Mobius map (a z + b)/(c z + d) mod ell as a permutation of P^1(F_ell), infinity = index ell+1 *)
mobiusPerm[ell_, {a_, b_, c_, d_}] :=
  With[{pts = Append[Range[0, ell - 1], Infinity], ix = z |-> If[z === Infinity, ell + 1, z + 1]},
    PermutationCycles @ Map[
      ix @ Which[
        # === Infinity, If[Mod[c, ell] == 0, Infinity, Mod[a PowerMod[c, -1, ell], ell]],
        Mod[c # + d, ell] == 0, Infinity,
        True, Mod[(a # + b) PowerMod[Mod[c # + d, ell], -1, ell], ell]] &,
      pts]];




(* ===================== General coset enumeration (Todd-Coxeter / low-index) =====================

   The curvature-agnostic method behind every regular map: maps of type {p, q} <-> finite-index
   subgroups of the von Dyck group D(p,q,2) = <x, y | x^p = y^q = (x y)^2 = 1>, x = face rotation
   (order p), y = vertex rotation (order q), x y = edge involution. Regular maps <-> NORMAL
   subgroups. This is what GAP/Magma do; the Wolfram Language has no finitely-presented-group
   machinery, so it is reimplemented here. Naive backtracking, so feasible only at small index
   (all five Platonic solids, small Euclidean/hyperbolic maps -- NOT the Klein quartic at index 168).
   Consumed by SchlafliTessellation's Method -> "CosetEnumeration" above.
   Word alphabet for subgroup generators: 1 = x, 2 = x^-1, 3 = y, 4 = y^-1. *)

cosetInv = {2, 1, 4, 3};
vonDyckRelators[p_, q_] := {ConstantArray[1, p], ConstantArray[3, q], {1, 3, 1, 3}};

(* [D(p,q,2) : H] for H = <subwords> by Todd-Coxeter with coincidence processing; $Failed if it
   exceeds maxc (infinite or too large). Run on the cyclic stabilizers it gives V, E, F directly. *)
CosetEnumeration[p_, q_, subwords_, maxc_] := Module[
  {rels = vonDyckRelators[p, q], tab = {{0, 0, 0, 0}}, repl = {1}, n = 1, rep, merge, coinc, scan, defc, w, c, i, g, qq = {}},
  rep[x0_] := Module[{x = x0}, While[repl[[x]] != x, x = repl[[x]]]; x];
  defc[cc_, gg_] := (n++; AppendTo[tab, {0, 0, 0, 0}]; AppendTo[repl, n]; tab[[cc, gg]] = n; tab[[n, cosetInv[[gg]]]] = cc);
  merge[a0_, b0_] := With[{a = rep[a0], b = rep[b0]}, If[a != b, repl[[Max[a, b]]] = Min[a, b]; AppendTo[qq, Max[a, b]]]];
  coinc[a_, b_] := Module[{e, d, e1, d1},
    qq = {}; merge[a, b];
    While[qq =!= {}, e = First[qq]; qq = Rest[qq];
      Do[d = tab[[e, g]];
        If[d != 0,
          tab[[d, cosetInv[[g]]]] = 0; e1 = rep[e]; d1 = rep[d];
          Which[
            tab[[e1, g]] != 0, merge[d1, tab[[e1, g]]],
            tab[[d1, cosetInv[[g]]]] != 0, merge[e1, tab[[d1, cosetInv[[g]]]]],
            True, tab[[e1, g]] = d1; tab[[d1, cosetInv[[g]]]] = e1]],
        {g, 4}]]];
  scan[cc_, rel_] := Module[{f = cc, b = cc, len = Length[rel], ii = 1, jj},
    jj = len;
    While[ii <= len && tab[[rep[f], rel[[ii]]]] != 0, f = rep[tab[[rep[f], rel[[ii]]]]]; ii++];
    While[jj >= ii && tab[[rep[b], cosetInv[[rel[[jj]]]]]] != 0, b = rep[tab[[rep[b], cosetInv[[rel[[jj]]]]]]]; jj--];
    Which[
      ii == jj + 1, If[rep[f] != rep[b], coinc[f, b]],
      ii == jj, tab[[rep[f], rel[[ii]]]] = rep[b]; tab[[rep[b], cosetInv[[rel[[ii]]]]]] = rep[f],
      True, Null]];
  Do[c = 1;
    Do[If[tab[[c, w[[i]]]] == 0, defc[c, w[[i]]]]; c = rep[tab[[c, w[[i]]]]], {i, Length[w] - 1}];
    g = Last[w];
    If[tab[[c, g]] == 0, tab[[c, g]] = 1; tab[[1, cosetInv[[g]]]] = c, If[rep[tab[[c, g]]] != 1, coinc[tab[[c, g]], 1]]],
    {w, subwords}];
  c = 1;
  While[c <= n && n <= maxc + 5,
    If[rep[c] == c, Do[If[rep[c] == c && tab[[c, g]] == 0, defc[c, g]; Do[scan[c, rel], {rel, rels}]], {g, 4}]];
    c++];
  If[n > maxc + 5, $Failed, Count[Range[n], _?(rep[#] == # &)]]];

(* forced deductions of all relators through a partial coset table; $Failed on contradiction *)
cosetTableClose[t0_, rels_] := Module[{t = t0, changed = True, m, f, b, i, j, len},
  Catch[
    While[changed, changed = False; m = Length[t];
      Do[len = Length[rel];
        f = c; i = 1; While[i <= len && t[[f, rel[[i]]]] != 0, f = t[[f, rel[[i]]]]; i++];
        b = c; j = len; While[j >= i && t[[b, cosetInv[[rel[[j]]]]]] != 0, b = t[[b, cosetInv[[rel[[j]]]]]]; j--];
        Which[
          i == j + 1, If[f != b, Throw[$Failed]],
          i == j,
            If[t[[f, rel[[i]]]] != 0 && t[[f, rel[[i]]]] != b, Throw[$Failed]];
            If[t[[b, cosetInv[[rel[[i]]]]]] != 0 && t[[b, cosetInv[[rel[[i]]]]]] != f, Throw[$Failed]];
            t[[f, rel[[i]]]] = b; t[[b, cosetInv[[rel[[i]]]]]] = f; changed = True,
          True, Null],
        {c, m}, {rel, rels}]];
    t]];

(* every standardized complete coset table of index <= maxIndex (one per subgroup) *)
lowIndexCosetTables[p_, q_, maxIndex_] := Module[{rels = vonDyckRelators[p, q], out = {}, search},
  search[t_] := Module[{m = Length[t], slot, c, g, t2},
    slot = Catch[Do[If[t[[cc, gg]] == 0, Throw[{cc, gg}]], {cc, m}, {gg, 4}]; None];
    If[slot === None, AppendTo[out, t]; Return[]];
    {c, g} = slot;
    Do[If[t[[d, cosetInv[[g]]]] == 0,
        t2 = t; t2[[c, g]] = d; t2[[d, cosetInv[[g]]]] = c;
        t2 = cosetTableClose[t2, rels]; If[t2 =!= $Failed, search[t2]]], {d, 1, m}];
    If[m < maxIndex,
      t2 = Append[t, {0, 0, 0, 0}]; t2[[c, g]] = m + 1; t2[[m + 1, cosetInv[[g]]]] = c;
      t2 = cosetTableClose[t2, rels]; If[t2 =!= $Failed, search[t2]]]];
  search[cosetTableClose[{{0, 0, 0, 0}}, rels]];
  out];

(* canonical form of a coset table: lex-least BFS relabel over all base cosets (a conjugacy invariant) *)
cosetCanonicalTable[t_] := First @ Sort @ Table[
  Module[{m = Length[t], map = ConstantArray[0, Length[t]], nxt = 2, queue, c, d, newt},
    map[[base]] = 1; queue = {base};
    While[queue =!= {}, c = First[queue]; queue = Rest[queue];
      Do[d = t[[c, g]]; If[map[[d]] == 0, map[[d]] = nxt++; AppendTo[queue, d]], {g, 4}]];
    newt = ConstantArray[0, {m, 4}];
    Do[newt[[map[[c]], g]] = map[[t[[c, g]]]], {c, m}, {g, 4}]; newt],
  {base, Length[t]}];

(* a permutation is a product of len-cycles covering all idx points (no fixed points) *)
uniformCycleQ[pl_, idx_, len_] := With[{cyc = First @ PermutationCycles[pl]},
  Total[Length /@ cyc] == idx && AllTrue[cyc, Length[#] == len &]];

(* normal subgroup <=> regular (Cayley) action <=> |<x,y>| == index *)
regularActionQ[{x_, y_}, idx_] := GroupOrder[PermutationGroup[PermutationCycles /@ {x, y}]] == idx;

(* skeleton of a rotation map {x, y}: vertices = y-cycles, edges = 2-cycles of (x y) joining them *)
RotationMapGraph[{x_, y_}] := Module[{ycyc = First @ PermutationCycles[y], vlab},
  vlab = Association @@ Flatten[MapIndexed[Function[{cyc, i}, (# -> First[i]) & /@ cyc], ycyc]];
  Graph[Range[Length[ycyc]], (UndirectedEdge @@ (vlab /@ #)) & /@ First @ PermutationCycles[PermutationProduct[x, y]]]];

(* every genuine {p,q} map of index <= maxIndex up to isomorphism:
   <|"Index", "Generators" -> {Cycles x, Cycles y}, "Skeleton", "Regular", "Genus"|> *)
LowIndexMaps[p_, q_, maxIndex_] := Module[{tabs, recs},
  tabs = DeleteDuplicatesBy[lowIndexCosetTables[p, q, maxIndex], cosetCanonicalTable];
  recs = {{#[[All, 1]], #[[All, 3]]}, Length[#]} & /@ tabs;
  recs = Select[recs, With[{x = #[[1, 1]], y = #[[1, 2]], idx = #[[2]]},
    uniformCycleQ[x, idx, p] && uniformCycleQ[y, idx, q] && uniformCycleQ[PermutationProduct[x, y], idx, 2]] &];
  Map[Function[rec, With[{perms = rec[[1]], idx = rec[[2]]},
    <|"Index" -> idx, "Generators" -> (PermutationCycles /@ perms), "Skeleton" -> RotationMapGraph[perms],
      "Regular" -> regularActionQ[perms, idx], "Genus" -> 1 - (idx/q - idx/2 + idx/p)/2|>]], recs]];


(* apply forwarded Graph options to a constructed graph, passing a non-graph result
   (e.g. $Failed from a deferred constructor) straight through *)
withGraphOptions[g_, opts___] := If[GraphQ[g], Graph[g, Sequence @@ FilterRules[{opts}, Options[Graph]]], g]
