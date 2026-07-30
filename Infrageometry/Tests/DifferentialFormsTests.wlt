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

(* ===== Cup: strictly associative, NOT graded-commutative ===== *)

VerificationTest[cEqual[CochainCup[k4, CochainCup[k4, aC, bC], cC], CochainCup[k4, aC, CochainCup[k4, bC, cC]]], True, TestID -> "cup-assoc-K4"]
VerificationTest[Not[cEqual[CochainCup[k4, aC, bC], scale[CochainCup[k4, bC, aC], -1]]], True, TestID -> "cup-not-graded-comm-K4"]

(* ===== Antisymmetrized cup: graded-commutative but NOT associative (A-infinity) ===== *)

VerificationTest[cEqual[AntisymmetrizedCup[k4, aC, bC], scale[AntisymmetrizedCup[k4, bC, aC], -1]], True, TestID -> "antisym-cup-graded-comm-K4"]
VerificationTest[Not[cEqual[AntisymmetrizedCup[k4, AntisymmetrizedCup[k4, aC, bC], cC], AntisymmetrizedCup[k4, aC, AntisymmetrizedCup[k4, bC, cC]]]], True, TestID -> "antisym-cup-non-assoc-K4"]

(* ===== I is a chain map but not a ring map; the defect is a coboundary ===== *)

VerificationTest[Not[cEqual[IntegrationMap[k4, FormWedge[RestrictionMap[k4, aC], RestrictionMap[k4, bC]]], CochainCup[k4, aC, bC]]], True, TestID -> "I-not-ring-map-K4"]
VerificationTest[cEqual[Coboundary[k4, DeleteCases[Merge[{CochainCup[k4, Coboundary[k4, fC], Coboundary[k4, gC]], Map[Minus, CochainCup[k4, Coboundary[k4, gC], Coboundary[k4, fC]]]}, Total], 0]], <||>], True, TestID -> "cup-commutator-closed-cocycle-K4"]

EndTestSection[]
