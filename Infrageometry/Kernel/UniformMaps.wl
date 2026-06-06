Package["WolframInstitute`Infrageometry`"]

PackageExport[ArchimedeanTessellation]
PackageExport[MapGenus]
PackageExport[UniformMapQ]

PackageScope[mapFaces]
PackageScope[rectifyMapData]
PackageScope[truncateMapData]
PackageScope[canonicalConfiguration]
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
    True, Message[ArchimedeanTessellation::deferred, config]; $Failed];

ArchimedeanTessellation::deferred =
  "The uniform map `1` is not built by this constructor (hyperbolic, snub, or elongated families are not yet supported).";

uniformDefect[c_] := Total[1/c] - (Length[c] - 2)/2;

sphericalUniform[config_] :=
  With[{key = canonicalConfiguration @ config},
    Which[
      KeyExistsQ[archimedeanSolidName, key], PolyhedronData[archimedeanSolidName[key], "SkeletonGraph"],
      Count[config, 4] == 2 && Length[config] == 3, prismGraph[First @ DeleteCases[config, 4]],
      Count[config, 3] == 3 && Length[config] == 4, antiprismGraph[First @ DeleteCases[config, 3]],
      True, Message[ArchimedeanTessellation::deferred, config]; $Failed]];

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
      _, Message[ArchimedeanTessellation::deferred, key]; $Failed]];

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


(* ===================== Genus and recognition ===================== *)

(* orientable genus of a uniform map of vertex configuration config from its vertex count:
   E = V k/2, F = V Sum 1/f_i, g = 1 - (V - E + F)/2 *)
MapGenus[config_List, g_Graph] :=
  1 - VertexCount[g] (1 - Length[config]/2 + Total[1/config])/2;

(* vertex-transitive connected simple graph -- the necessary 1-skeleton condition for a
   uniform map (regular-polygon faces is the embedding datum, supplied by construction) *)
UniformMapQ[g_Graph] := SimpleGraphQ[g] && ConnectedGraphQ[g] && VertexTransitiveGraphQ[g];


(* ===================== Map data: graph + face cycles ===================== *)

mapData[g_] := {g, mapFaces[g]};

(* the faces of a regular-map 1-skeleton: all shortest (girth-length) cycles *)
mapFaces[g_] := cycleVertices /@ FindCycle[g, {mapGirth[g]}, All];

mapGirth[g_] := First @ Select[Range[3, EdgeCount[g] + 1], FindCycle[g, {#}, 1] =!= {} &];

cycleVertices[es_] :=
  With[{vs = List @@@ es},
    {start = If[MemberQ[vs[[2]], vs[[1, 2]]], vs[[1, 1]], vs[[1, 2]]]},
    Most @ FoldList[Function[{prev, e}, First @ Complement[e, {prev}]], start, vs]];

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
