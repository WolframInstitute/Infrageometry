Package["WolframInstitute`Infrageometry`"]

PackageExport[TessellationNeighborhoodGraph]

PackageScope[growTessellation]
PackageScope[tessellationFacesGraph]
PackageScope[graphDistanceBall]
PackageScope[euclideanDisk]
PackageScope[hyperbolicDisk]
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
