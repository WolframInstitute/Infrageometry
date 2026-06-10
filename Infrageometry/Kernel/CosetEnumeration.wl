Package["WolframInstitute`Infrageometry`"]

PackageExport[CosetEnumeration]
PackageExport[LowIndexMaps]
PackageExport[RotationMapGraph]

PackageScope[cosetInv]
PackageScope[vonDyckRelators]
PackageScope[cosetTableClose]
PackageScope[lowIndexCosetTables]
PackageScope[cosetCanonicalTable]
PackageScope[uniformCycleQ]
PackageScope[regularActionQ]

(* ===================== General coset enumeration (Todd-Coxeter / low-index) =====================

   The curvature-agnostic method behind every regular map: maps of type {p, q} <-> finite-index
   subgroups of the von Dyck group D(p,q,2) = <x, y | x^p = y^q = (x y)^2 = 1>, x = face rotation
   (order p), y = vertex rotation (order q), x y = edge involution. Regular maps <-> NORMAL
   subgroups. This is what GAP/Magma do; the Wolfram Language has no finitely-presented-group
   machinery, so it is reimplemented here. Naive backtracking, so feasible only at small index
   (all five Platonic solids, small Euclidean/hyperbolic maps -- NOT the Klein quartic at index 168).
   Consumed by SchlafliTessellation's Method -> "CosetEnumeration" (in RegularMaps.wl).
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
