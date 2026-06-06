Package["WolframInstitute`Infrageometry`"]

PackageExport[RegularMap]
PackageExport[SchlafliTessellation]
PackageExport[RegularMapsAt]
PackageExport[RegularMapGenus]

PackageScope[findGenerators]
PackageScope[sphericalGroup]
PackageScope[shapeName]
PackageScope[hyperbolicMap]
PackageScope[psl2Group]
PackageScope[mobiusPerm]

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

(* smallest regular map of type {p, q}; size n scales the Euclidean (flat-torus) and
   hyperbolic (n-th PSL(2,ell) quotient) cases, dispatch on the sign of (p-2)(q-2) - 4 *)
SchlafliTessellation[{p_, q_}, n_: 1] :=
  With[{c = (p - 2) (q - 2)},
    Which[
      c < 4,  RegularMap[{p, q}, sphericalGroup[{p, q}]],
      c == 4, TorusTessellation[{n, n}, shapeName[{p, q}]],
      True,   hyperbolicMap[{p, q}, n]]];

(* orientable genus of a type-{p, q} regular map from its edge count: 1 - E (1/p + 1/q - 1/2)
   (= 0 spherical, 1 Euclidean/torus, >= 2 hyperbolic) *)
RegularMapGenus[{p_, q_}, g_Graph] := 1 - EdgeCount[g] (1/p + 1/q - 1/2);

(* same genus read off the graph alone: a regular map is q-regular with girth p *)
RegularMapGenus[g_Graph] :=
  With[
    {q = First @ Union @ VertexDegree[g],
     p = SelectFirst[Range[3, EdgeCount[g]], FindCycle[g, {#}, 1] =!= {} &]},
    RegularMapGenus[{p, q}, g]];


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
