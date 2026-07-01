Package["WolframInstitute`Infrageometry`"]

PackageExport[TessellationGraph]
PackageExport[TessellationCurvature]
PackageExport[TessellationEulerCharacteristic]
PackageExport[TessellationGenus]

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

(* Uniform / Archimedean maps: vertex-transitive tilings by regular polygons of several
   sizes, the vertex-transitive generalisation of the flag-transitive regular maps in
   RegularMaps.wl. A uniform map has a vertex configuration (f_1. ... .f_k) -- the cyclic
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
