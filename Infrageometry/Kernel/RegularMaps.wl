Package["WolframInstitute`Infrageometry`"]

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
   (LowIndexMaps in CosetEnumeration.wl), built via the coset-graph builder RegularMap;
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
