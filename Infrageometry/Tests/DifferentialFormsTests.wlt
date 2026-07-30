BeginTestSection["DifferentialFormsTests"]

(* ===== Helpers: equality of sparse (co)chains up to dropped zeros ===== *)

cEqual[a_, b_] := DeleteCases[Merge[{a, Map[Minus, b]}, Total], 0] === <||>
fEqual[a_, b_] := AllTrue[Union[Keys[a], Keys[b]], cEqual[Lookup[a, Key[#], <||>], Lookup[b, Key[#], <||>]] &]
scale[c_, s_] := Map[s # &, c]

(* ===== Fixtures ===== *)

k3 = CompleteGraph[3];
k4 = CompleteGraph[4];
k5 = CompleteGraph[5];
claw = Graph[{1 <-> 2, 1 <-> 3, 1 <-> 4}];

aC = <|{1, 2} -> 1, {1, 3} -> 2, {1, 4} -> 0, {2, 3} -> -1, {2, 4} -> 3, {3, 4} -> 1|>;
bC = <|{1, 2} -> 0, {1, 3} -> 1, {1, 4} -> 2, {2, 3} -> 1, {2, 4} -> 0, {3, 4} -> -2|>;
cC = <|{1, 2} -> 2, {1, 3} -> 0, {1, 4} -> 1, {2, 3} -> 3, {2, 4} -> -1, {3, 4} -> 2|>;
fVals = <|1 -> 0, 2 -> 1, 3 -> 3, 4 -> 7|>;
gVals = <|1 -> 0, 2 -> 2, 3 -> 1, 4 -> 5|>;
fC = <|{1} -> 0, {2} -> 1, {3} -> 3, {4} -> 7|>;
gC = <|{1} -> 0, {2} -> 2, {3} -> 1, {4} -> 5|>;

genFormA[g_] := AssociationMap[v |-> Association[{#} -> 10 v + # & /@ AdjacencyList[g, v]], VertexList[g]];
genFormB[g_] := AssociationMap[v |-> Association[{#} -> v - 2 # & /@ AdjacencyList[g, v]], VertexList[g]];
genFormC[g_] := AssociationMap[v |-> Association[{#} -> 3 # - v & /@ AdjacencyList[g, v]], VertexList[g]];

(* ===== Restriction / integration: I is a left inverse of R ===== *)

VerificationTest[cEqual[IntegrationMap[k4, RestrictionMap[k4, aC]], aC], True, TestID -> "IR-id-1cochain-K4"]
VerificationTest[cEqual[IntegrationMap[k3, RestrictionMap[k3, <|{1, 2, 3} -> 5|>]], <|{1, 2, 3} -> 5|>], True, TestID -> "IR-id-2cochain-K3"]
VerificationTest[cEqual[IntegrationMap[k5, RestrictionMap[k5, aC]], aC], True, TestID -> "IR-id-1cochain-K5"]

(* ===== delta^2 = 0 ===== *)

VerificationTest[cEqual[Coboundary[k4, Coboundary[k4, fC]], <||>], True, TestID -> "delta2-zero-K4-0cochain"]
VerificationTest[cEqual[Coboundary[k4, Coboundary[k4, aC]], <||>], True, TestID -> "delta2-zero-K4-1cochain"]

(* ===== Chain map: I d = delta I on 1-forms; naive gives ratio 2/3 ===== *)

VerificationTest[cEqual[IntegrationMap[k4, FormDifferential[k4, genFormA[k4]]], Coboundary[k4, IntegrationMap[k4, genFormA[k4]]]], True, TestID -> "chain-map-K4"]
VerificationTest[cEqual[IntegrationMap[k5, FormDifferential[k5, genFormA[k5]]], Coboundary[k5, IntegrationMap[k5, genFormA[k5]]]], True, TestID -> "chain-map-K5"]
VerificationTest[cEqual[IntegrationMap[k4, NaiveDifferential[k4, genFormA[k4]]], scale[Coboundary[k4, IntegrationMap[k4, genFormA[k4]]], 2/3]], True, TestID -> "naive-ratio-2over3-K4"]

(* ===== d^2 = 0 on im R, but not on non-clique pairs ===== *)

VerificationTest[fEqual[FormDifferential[k4, FormDifferential[k4, ZeroForm[k4, fVals]]], <||>], True, TestID -> "d2-zero-imR-K4"]
VerificationTest[FormValue[FormDifferential[claw, FormDifferential[claw, ZeroForm[claw, <|1 -> 0, 2 -> 1, 3 -> 5, 4 -> 9|>]]], 1, {2, 3}], -4, TestID -> "d2-defect-claw"]
VerificationTest[Not[fEqual[FormDifferential[claw, FormDifferential[claw, ZeroForm[claw, <|1 -> 0, 2 -> 1, 3 -> 5, 4 -> 9|>]]], <||>]], True, TestID -> "d2-nonzero-claw"]

(* ===== Wedge vs cup: I(Ra ^ Rb) = 2 * antisym(a v b) ===== *)

VerificationTest[cEqual[IntegrationMap[k4, FormWedge[RestrictionMap[k4, aC], RestrictionMap[k4, bC]]], scale[AntisymmetrizedCup[k4, aC, bC], 2]], True, TestID -> "wedge-cup-factor2-K4"]

(* ===== Wedge on forms: strictly associative and graded-commutative ===== *)

VerificationTest[fEqual[FormWedge[FormWedge[genFormA[k5], genFormB[k5]], genFormC[k5]], FormWedge[genFormA[k5], FormWedge[genFormB[k5], genFormC[k5]]]], True, TestID -> "wedge-assoc-K5"]
VerificationTest[fEqual[FormWedge[genFormA[k5], genFormB[k5]], scale[FormWedge[genFormB[k5], genFormA[k5]], -1]], True, TestID -> "wedge-graded-comm-K5"]

(* ===== OrderedCochainCup, the bare Alexander-Whitney formula:
        strictly associative, NOT graded-commutative ===== *)

VerificationTest[cEqual[OrderedCochainCup[k4, OrderedCochainCup[k4, aC, bC], cC], OrderedCochainCup[k4, aC, OrderedCochainCup[k4, bC, cC]]], True, TestID -> "cup-assoc-K4"]
VerificationTest[Not[cEqual[OrderedCochainCup[k4, aC, bC], scale[OrderedCochainCup[k4, bC, aC], -1]]], True, TestID -> "cup-not-graded-comm-K4"]

(* ===== CochainCup, the cup product: graded-commutative but NOT associative (A-infinity).
        AntisymmetrizedCup is its alias. ===== *)

VerificationTest[cEqual[CochainCup[k4, aC, bC], scale[CochainCup[k4, bC, aC], -1]], True, TestID -> "cup-graded-comm-K4"]
VerificationTest[Not[cEqual[CochainCup[k4, CochainCup[k4, aC, bC], cC], CochainCup[k4, aC, CochainCup[k4, bC, cC]]]], True, TestID -> "cup-non-assoc-K4"]
VerificationTest[DeleteCases[Merge[{CochainCup[k4, CochainCup[k4, aC, bC], cC], Map[Minus, CochainCup[k4, aC, CochainCup[k4, bC, cC]]]}, Total], 0], <|{1, 2, 3, 4} -> -1/6|>, TestID -> "cup-associator-K4-is-minus-one-sixth"]
VerificationTest[cEqual[CochainCup[k4, aC, bC], AntisymmetrizedCup[k4, aC, bC]], True, TestID -> "antisym-cup-is-alias-of-cup"]
VerificationTest[Not[cEqual[CochainCup[k4, aC, bC], OrderedCochainCup[k4, aC, bC]]], True, TestID -> "cup-differs-from-ordered-cup"]

(* ===== The cup is unital and orientation-invariant, which the ordered cup is not ===== *)

VerificationTest[cEqual[CochainCup[k4, <|{1} -> 1, {2} -> 1, {3} -> 1, {4} -> 1|>, aC], aC], True, TestID -> "cup-unital-K4"]
VerificationTest[CochainValue[CochainCup[k4, aC, bC], {1, 3, 2}] === -CochainValue[CochainCup[k4, aC, bC], {1, 2, 3}], True, TestID -> "cup-is-alternating-K4"]

(* ===== I is a chain map but not a ring map; the defect is a coboundary ===== *)

VerificationTest[Not[cEqual[IntegrationMap[k4, FormWedge[RestrictionMap[k4, aC], RestrictionMap[k4, bC]]], OrderedCochainCup[k4, aC, bC]]], True, TestID -> "I-not-ring-map-K4"]
VerificationTest[cEqual[Coboundary[k4, DeleteCases[Merge[{OrderedCochainCup[k4, Coboundary[k4, fC], Coboundary[k4, gC]], Map[Minus, OrderedCochainCup[k4, Coboundary[k4, gC], Coboundary[k4, fC]]]}, Total], 0]], <||>], True, TestID -> "cup-commutator-closed-cocycle-K4"]

(* ===== The cup is an ORDERED cochain: reading it as alternating gives a different cochain.
        Regression guard for the two conventions sharing one storage format. ===== *)

awValue[al_, be_, p_, q_, t_] := CochainValue[al, Take[t, p + 1]] CochainValue[be, Take[t, -(q + 1)]]

VerificationTest[awValue[aC, bC, 1, 1, {1, 3, 2}], -2, TestID -> "cup-ordered-true-value-K4"]
VerificationTest[CochainValue[OrderedCochainCup[k4, aC, bC], {1, 3, 2}], -1, TestID -> "cup-ordered-misread-K4"]
VerificationTest[Not[awValue[aC, bC, 1, 1, {1, 3, 2}] === CochainValue[OrderedCochainCup[k4, aC, bC], {1, 3, 2}]], True, TestID -> "cup-not-alternating-K4"]
VerificationTest[OrderedCochainValue[OrderedCochainCup[k4, aC, bC], {1, 2, 3}], 1, TestID -> "ordered-value-increasing-K4"]
VerificationTest[OrderedCochainValue[OrderedCochainCup[k4, aC, bC], {1, 3, 2}], Missing["NonIncreasingTuple", {1, 3, 2}], TestID -> "ordered-value-non-increasing-K4"]

(* ===== Steenrod cup-1 is a primitive for the graded commutator of cocycles ===== *)

k6 = CompleteGraph[6];
randomCochain[g_, k_] := Association[# -> RandomInteger[{-4, 4}] & /@ Union[Sort /@ GraphComplex[g, {k + 1}]]]
cupOneIdentityQ[g_, p_, q_] := (SeedRandom[5];
  With[{al = Coboundary[g, randomCochain[g, p - 1]], be = Coboundary[g, randomCochain[g, q - 1]]},
    cEqual[Coboundary[g, CochainCupOne[g, al, be]],
      DeleteCases[Merge[{OrderedCochainCup[g, al, be], Map[-(-1)^(p q) # &, OrderedCochainCup[g, be, al]]}, Total], 0]]])

VerificationTest[Table[cupOneIdentityQ[k6, p, q], {p, 3}, {q, 3}], ConstantArray[True, {3, 3}], TestID -> "cup-one-primitive-K6-degrees-1-3"]
VerificationTest[CochainCupOne[k4, fC, aC], <||>, TestID -> "cup-one-vanishes-degree-0"]
VerificationTest[CochainCupOne[k4, aC, bC], <|{1, 3} -> -2, {2, 3} -> 1, {3, 4} -> 2|>, TestID -> "cup-one-is-minus-diagonal-K4"]

(* ===== The torus: the only example where the cohomological claims have content ===== *)

torus = IndexGraph[Graph[Flatten[Table[
  {{i, j} <-> {Mod[i + 1, 4], j}, {i, j} <-> {i, Mod[j + 1, 4]}, {i, j} <-> {Mod[i + 1, 4], Mod[j + 1, 4]}},
  {i, 0, 3}, {j, 0, 3}]]]];
cellsOf[g_, k_] := Union[Sort /@ GraphComplex[g, {k}]]
dMat[g_, k_] := With[{lo = cellsOf[g, k], hi = cellsOf[g, k + 1]},
  If[hi === {} || lo === {}, {}, Table[Sum[(-1)^(i - 1) Boole[Delete[c, i] == f], {i, k + 1}], {c, hi}, {f, lo}]]]
h2ExactQ[g_, gam_] := With[{m = dMat[g, 2], tri = cellsOf[g, 3]},
  MatrixRank[m] === MatrixRank[Join[m, Transpose[{Lookup[gam, Key[#], 0] & /@ tri}], 2]]]
h1Gens[g_] := Module[{acc = RowReduce[Transpose[dMat[g, 1]]], out = {}},
  Do[If[MatrixRank[Join[acc, {z}]] > MatrixRank[acc], AppendTo[out, z]; acc = Join[acc, {z}]], {z, NullSpace[dMat[g, 2]]}];
  AssociationThread[cellsOf[g, 2], #] & /@ out]
{z1, z2} = Take[h1Gens[torus], 2];

VerificationTest[{Length[cellsOf[torus, 1]], Length[cellsOf[torus, 2]], Length[cellsOf[torus, 3]], Length[cellsOf[torus, 4]]}, {16, 48, 32, 0}, TestID -> "torus-f-vector"]
VerificationTest[Length[h1Gens[torus]], 2, TestID -> "torus-H1-rank-2"]
VerificationTest[Length[cellsOf[torus, 3]] - MatrixRank[dMat[torus, 2]], 1, TestID -> "torus-H2-rank-1"]
VerificationTest[{cEqual[Coboundary[torus, z1], <||>], cEqual[Coboundary[torus, z2], <||>]}, {True, True}, TestID -> "torus-H1-gens-closed"]
VerificationTest[h2ExactQ[torus, DeleteCases[Merge[{OrderedCochainCup[torus, z1, z2], OrderedCochainCup[torus, z2, z1]}, Total], 0]], True, TestID -> "torus-graded-commutator-exact"]
VerificationTest[h2ExactQ[torus, DeleteCases[Merge[{OrderedCochainCup[torus, z1, z2], Map[Minus, OrderedCochainCup[torus, z2, z1]]}, Total], 0]], False, TestID -> "torus-difference-not-exact"]
VerificationTest[cEqual[Coboundary[torus, CochainCupOne[torus, z1, z2]], DeleteCases[Merge[{OrderedCochainCup[torus, z1, z2], OrderedCochainCup[torus, z2, z1]}, Total], 0]], True, TestID -> "torus-cup-one-primitive"]

(* ===== The 1/(p+q+1)! normalisation is forced: doubling it changes the H^2 class ===== *)

VerificationTest[Not[h2ExactQ[torus, OrderedCochainCup[torus, z1, z2]]], True, TestID -> "torus-cup-nonzero-in-H2"]
VerificationTest[Not[h2ExactQ[torus, AntisymmetrizedCup[torus, z1, z2]]], True, TestID -> "torus-antisym-nonzero-in-H2"]
VerificationTest[h2ExactQ[torus, DeleteCases[Merge[{OrderedCochainCup[torus, z1, z2], Map[Minus, AntisymmetrizedCup[torus, z1, z2]]}, Total], 0]], True, TestID -> "torus-cup-and-antisym-agree-in-H2"]
VerificationTest[h2ExactQ[torus, DeleteCases[Merge[{OrderedCochainCup[torus, z1, z2], Map[-2 # &, AntisymmetrizedCup[torus, z1, z2]]}, Total], 0]], False, TestID -> "torus-doubled-antisym-wrong-in-H2"]

(* ===== Exact-cochain tests of the commutator are vacuous: each term is separately exact ===== *)

VerificationTest[cEqual[OrderedCochainCup[k4, Coboundary[k4, fC], Coboundary[k4, aC]], Coboundary[k4, OrderedCochainCup[k4, fC, Coboundary[k4, aC]]]], True, TestID -> "exact-cup-closed-is-exact-K4"]

EndTestSection[]
