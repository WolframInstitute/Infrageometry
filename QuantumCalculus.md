# Oliver Knill's Quantum Calculus — Summary & Adaptation Guide

> Source: [quantumcalculus.org](https://www.quantumcalculus.org/) — Oliver Knill (Harvard)
>
> Purpose: map Knill's discrete geometry program onto our **Infrageometry** paclet, identify
> what we already implement, and catalogue what we should adapt next.

---

## 1. Philosophical Core

Knill's program is **strict finitism applied to differential geometry**: every continuum concept
(curvature, cohomology, wave equation, geodesic flow) gets a purely combinatorial counterpart
on finite abstract simplicial complexes (or, more recently, on delta sets and quivers).
No limits, no smoothness, no infinite-dimensional Hilbert spaces — just finite matrices and
finite set operations.

Key mantras:

- *"A geometry starts when you have a derivative."* — A finite set of sets becomes geometric
  the moment it carries an exterior derivative `d` and therefore a Dirac operator `D = d + d*`.
- *Bosonic vs Fermionic*: integral-geometric quantities (lengths, areas, volumes — order-blind)
  are **Bosonic**; exterior-calculus quantities (Stokes theorem, cohomology — orientation-aware)
  are **Fermionic**. Most confusion in textbook calculus comes from mixing these two.
- *Supersymmetry*: the non-zero spectrum of `H = D²` restricted to even-dimensional forms
  equals the non-zero spectrum restricted to odd-dimensional forms. This single fact yields
  both the Euler–Poincaré formula and the Brouwer–Lefschetz fixed-point theorem by a
  super-trace deformation argument.

---

## 2. Existing Infrageometry Coverage

Our `Infrageometry.wl` (351 lines) already implements the following Knill-originating concepts:

| Knill Concept | Our Function(s) | Status |
|---|---|---|
| Simplicial complex closure | `ComplexClosure` | ✅ |
| f-vector / simplex cardinalities | `SimplexCardinalities` | ✅ |
| Euler characteristic `ω(x) = -(-1)^|x|` | `ComplexEulerCharacteristic` | ✅ |
| Fermi characteristic `∏ ω(x)` | `ComplexFermiCharacteristic` | ✅ |
| Unit sphere `S(x)` | `SimplexUnitSphere` | ✅ |
| Star `U(x)` and Core `C(x)` | `SimplexStar`, `SimplexCore` | ✅ |
| Contractibility / Sphere / Manifold tests | `ContractibleQ`, `ComplexSphereQ`, `ComplexManifoldQ` | ✅ |
| Inductive dimension | `ComplexInductiveDimension` | ✅ |
| Incidence (sign) matrix `d` | `ComplexIncidenceMatrix` | ✅ |
| Connection matrix `L` | `ConnectionMatrix` | ✅ |
| Green function matrix `g = L⁻¹` | `GreenFunctionMatrix` | ✅ |
| Dirac (Hodge) matrix `D = d + d*` | `DiracHodgeMatrix` | ✅ |
| Dirac (connection) matrix `D_L = L + L^T` | `DiracConnectionMatrix` | ✅ |
| Hodge Laplacian `H = D²` (block form) | `HodgeMatrix`, `HodgeLaplacianMatrix` | ✅ |
| Betti numbers | `BettiVector` | ✅ |
| Lefschetz number & curvature | `LefschetzNumber`, `LefschetzCurvature` | ✅ |
| Simplicial map / automorphism | `SimplicialMap` | ✅ |
| Complex polynomial / Poincaré polynomial | `ComplexPolynomial`, `PoincarePolynomial` | ✅ |
| Dehn–Sommerville check | `DehnSommervilleQ` | ✅ |
| Barycentric refinement | `BarycentricRefinement` | ✅ |
| Face graph / skeleton graph | `FaceGraph`, `ComplexGraph`, `SkeletonComplex` | ✅ |
| Alexandrov topology | `AlexandrovTopology` | ✅ |
| Geodesic flow on frames | `ComplexGeodesicFlow`, `SimplexOrbit`, `ComplexGeodesics` | ✅ |
| Super trace / super determinant | `SuperTrace`, `SuperDeterminant`, `PseudoDeterminant` | ✅ |
| Complex join | `ComplexJoin` | ✅ |
| Complex dual | `ComplexDual` | ✅ |

---

## 3. What's Missing — Adaptation Targets

### 3.1 Wave Equation & Discrete Dynamics

Knill's most active recent thread (2024–2026). The core idea: the wave equation
`u'' = -D² u` has the d'Alembert solution

```
u(t) = cos(D t) u(0) + t sinc(D t) u'(0)
```

which is entirely determined by the Dirac matrix. On a finite complex, `cos(Dt)` and `sinc(Dt)`
are just matrix functions — no PDE needed.

**Causality Principle**: if space is discrete and we insist on finite propagation speed, time
must also be discrete. This motivates studying the *discrete-time* wave equation instead,
where the unitary propagator `U` is polynomial in `D` rather than transcendental.

| What to implement | Description |
|---|---|
| `WaveEquationSolution[G, u0, v0, t]` | d'Alembert solution via matrix cos/sinc |
| `DiscreteWaveStep[G, u, v]` | Single discrete-time step: `u(t+1) = 2u(t) - u(t-1) + L u(t)` |
| `WavePropagator[G]` | The unitary `U = exp(i D t)` or its polynomial discrete-time approximation |

### 3.2 Wave Front Calculus (2025–2026, newest work)

A non-local exterior derivative defined via geodesic wave fronts at distance `h`:

> Given a k-form `f` on a Riemannian manifold, define a (k+1)-form by integrating `f` over
> the intersection of the geodesic sheet with the wave front `S_h(p)`. The resulting derivative
> depends only on values of `f` broadcast from distance `h` away.

Benefits:
- No limits or smoothness required — bounded operators on Hilbert space
- Survives on manifolds with singularities (polyhedra)
- Isospectral Lax deformations `D' = [B, D]` work without pseudo-differential operators
- The boundary operation becomes the adjoint of the exterior derivative
- Fields and geometries unify: both are elements in the same Hilbert space

For finite complexes, this amounts to a deformed exterior derivative parametrized by `h`.

| What to implement | Description |
|---|---|
| `WaveFrontDerivative[G, h]` | Non-local exterior derivative at wave-front distance h |
| `SphericalAverage[G, x, h]` | Averaging over the h-sphere in the graph metric |
| `WaveFrontCohomology[G, h]` | Cohomology of the deformed complex |

### 3.3 Dynamical Dirac & Isospectral Deformation

Lax pair deformation `D' = [B, D]` where `B = d - d*` deforms the Dirac operator isospectrally.
The exterior derivative evolves: `D(t) = c(t) + c(t)* + m(t)` where `c(t)` remains an exterior
derivative but acquires a diagonal "mass" term. Under the Connes distance interpretation, this
produces a naturally **expanding space**.

Knill's compact code for this:

```mathematica
(* Dynamical Dirac — Oliver Knill, September 2025 *)
s[x_] := Signature[x];
s[x_, y_] := If[SubsetQ[x, y] && (Length[x] == Length[y] + 1),
  s[Prepend[y, Complement[x, y][[1]]]] * s[x], 0];
d[G_, ST_] := Table[s[G[[i]], G[[j]] /. ST], {i, Length[G]}, {j, Length[G]}];
Dirac[G_, ST_] := d[G, ST] + Transpose[d[G, ST]];
```

| What to implement | Description |
|---|---|
| `IsospectralDeformation[G, t]` | Lax flow `D' = [d-d*, D]` integrated to time t |
| `ConnesDistance[G, D]` | Distance from Connes' spectral formula: `d(x,y) = sup{|f(x)-f(y)| : ‖[D,f]‖ ≤ 1}` |
| `DiracMass[G, t]` | Extract the diagonal mass term from the deformed Dirac |

### 3.4 Dynamical Connection & Green Functions

The connection matrix `L` and its inverse (Green function matrix `g`) can also be
dynamically deformed under graph automorphisms:

```mathematica
(* 15 lines — Oliver Knill, September 2025 *)
l[G_, T_] := Table[Euler[Intersection[CC[G, G[[i]]], CC[G, G[[j]] /. ST]]],
  {i, n}, {j, n}];
g[G_, T_] := Table[w[G[[i]]] w[G[[j]]] Euler[Intersection[UU[G, G[[i]]], UU[G, G[[j]] /. ST]]],
  {i, n}, {j, n}];
(* Key identity: Inverse[l] == Transpose[g] *)
(* Also: Total[Flatten[g]] == Euler[G] *)
(* When T = identity: Det[l] == Fermi[G] *)
```

Key results:
- **Unimodularity**: `det(L) = ±1` for any simplicial complex (equals the Fermi characteristic)
- **Energy theorem**: `sum g(x,y) = χ(G)` where `g = L⁻¹`
- **Hydrogen identity** (1D): `L - L⁻¹` has a block structure linking Bosonic and Fermionic stories

| What to implement | Description |
|---|---|
| `DynamicalConnectionMatrix[G, T]` | Connection matrix under automorphism T |
| `DynamicalGreenFunction[G, T]` | Green function matrix under automorphism T |
| `HydrogenOperator[G]` | `L - L⁻¹` for 1D complexes |

### 3.5 Curvature Generalizations

Knill has several curvature notions beyond what we implement:

1. **Index-expectation curvature** (Gauss-Bonnet panorama): pick a probability space of
   functions, define `K(x) = E[i_f(x)]` where `i_f(x) = 1 - χ(S(x) ∩ {f < f(x)})`.
   Gauss-Bonnet: `∑ K(x) = χ(G)`.

2. **Sectional curvature**: adapted from the Riemannian notion to simplicial complexes.

3. **Form curvatures**: curvature on k-forms, not just on 0-forms (vertices).

4. **Curvature for manifolds with boundary**: distinct interior and boundary curvature terms.

| What to implement | Description |
|---|---|
| `IndexExpectationCurvature[G, prob]` | Curvature via index averaging over a probability space |
| `SectionalCurvature[G, x, plane]` | Discrete sectional curvature |
| `FormCurvature[G, k]` | Curvature on k-forms |
| `BoundaryCurvature[G]` | Curvature for complexes with boundary |

### 3.6 Wu Characteristic & Quadratic Cohomology

The **Wu characteristic** generalizes Euler characteristic by summing over pairs of simplices:

```
ω_m(G) = ∑_{x₁ ∩ ... ∩ x_m ≠ ∅} ω(x₁) ⋯ ω(x_m)
```

For `m=2` this is the **quadratic characteristic**. Knill proves a Gauss-Bonnet analog and
develops a full **Wu cohomology** with Betti numbers generalizing classical simplicial cohomology.

Key result: the **fusion inequality** for Betti numbers when a complex is
decomposed into a closed and open pair.

| What to implement | Description |
|---|---|
| `WuCharacteristic[G, m]` | m-th Wu characteristic |
| `WuCohomology[G]` | Wu cohomology groups / Betti vector |
| `FusionInequality[G, A, U]` | Verify/compute fusion inequality for closed-open decomposition |

### 3.7 Delta Sets & Quivers

**Delta sets** (Δ-sets) generalize simplicial complexes by allowing ordered simplices without
degeneracies. They arise naturally from quivers (directed graphs) and have their own notion of
exterior derivative and cohomology.

From our `Notions.md`, delta sets sit at: Closure ✓, Ordered ✓, Degenerate ✗, Multiset ✗.

Knill shows that all the Gauss-Bonnet, Brouwer–Lefschetz, and super-symmetry machinery extends
to delta sets. The exterior derivative for delta sets is defined combinatorially from the face maps.

| What to implement | Description |
|---|---|
| `DeltaSet[quiver]` | Construct a delta set from a quiver/directed graph |
| `DeltaSetIncidenceMatrix[Δ]` | Exterior derivative for delta sets |
| `DeltaSetCohomology[Δ]` | Cohomology of a delta set |

### 3.8 Zeta Functions

Three types of zeta functions for simplicial complexes:

1. **Dirac zeta function**: `ζ_D(s) = str(|D|^{-s})` (super trace of powers of Dirac)
2. **Connection zeta function**: from the eigenvalues of the connection matrix L
3. **Lefschetz zeta function**: encoding the fixed-point data of a map

These connect to an "elementary dyadic Riemann hypothesis" for circular graphs.

| What to implement | Description |
|---|---|
| `DiracZetaFunction[G, s]` | Super-spectral zeta function of the Dirac operator |
| `ConnectionZetaFunction[G, s]` | Spectral zeta function of the connection matrix |
| `LefschetzZetaFunction[G, T, z]` | Lefschetz zeta function for a map T |

### 3.9 Interacting Geodesics & the Hopf-Rinow Problem

On a finite simplicial complex, a single geodesic (our `ComplexGeodesicFlow`) is deterministic
and periodic, but cannot reach every point. Knill's solution: use a **gas of many geodesics**.

- Release geodesics at different times from different initial frames
- With enough particles, the return times become astronomically large
- The geodesic gas produces fluctuations that, over time `O(log V)`, renders every point reachable
- Interactions can be added locally within each simplex (lattice gauge theory style)

For 2D triangulated lattices, the geodesic evolution becomes a **cellular automaton** on 12
orientation matrices of size M×M. Collisions/interactions are local per plaquette.

| What to implement | Description |
|---|---|
| `GeodesicGas[G, n]` | n interacting geodesics on the complex |
| `GeodesicCellularAutomaton[G]` | Cellular automaton evolution for geodesics on a lattice |
| `GeodesicReachability[G, x, t]` | Points reachable from x within time window t |

### 3.10 Analytic Torsion & Spectral Invariants

The **analytic torsion** is a spectral invariant derived from the Hodge Laplacian:

```
log T(G) = ½ ∑_k (-1)^{k+1} k · log det'(H_k)
```

where `det'` is the pseudo-determinant (product of nonzero eigenvalues). We already have
`PseudoDeterminant` and `SuperDeterminant` — this is close.

Knill also studies **spectral monotonicity** under barycentric refinement: the spectrum of the
Hodge Laplacian monotonically refines under subdivision.

| What to implement | Description |
|---|---|
| `AnalyticTorsion[G]` | Analytic torsion from Hodge block pseudo-determinants |
| `SpectralMonotonicity[G, n]` | Verify spectral monotonicity under n barycentric refinements |

### 3.11 Soft Barycentric Refinement & Fisk Manifolds

**Soft barycentric refinement**: a randomized subdivision where not all faces are subdivided.
Preserves manifold structure and has universality properties: the spectral density converges
to a universal distribution depending only on the maximal dimension.

**Fisk manifolds**: even-dimensional manifolds where every codimension-2 face has an even
number of top-dimensional faces — related to coloring problems.

| What to implement | Description |
|---|---|
| `SoftBarycentricRefinement[G, p]` | Random refinement with probability p per face |
| `FiskManifoldQ[G]` | Test whether G is a Fisk manifold |

---

## 4. Recommended Priority Order

Based on how directly the concepts extend our existing `Infrageometry.wl`:

1. **Wave Equation** (§3.1) — immediate: just matrix cos/sinc on our existing Dirac/Hodge matrices
2. **Wu Characteristic** (§3.6) — pure combinatorics, extends our Euler/Fermi characteristic pair
3. **Isospectral Deformation** (§3.3) — uses our existing Dirac matrix, adds dynamics
4. **Dynamical Connection** (§3.4) — extends our `ConnectionMatrix`/`GreenFunctionMatrix`
5. **Zeta Functions** (§3.8) — spectral analysis on our existing matrices
6. **Analytic Torsion** (§3.10) — uses our `PseudoDeterminant` + `HodgeMatrix`
7. **Curvature Generalizations** (§3.5) — enriches our curvature story
8. **Delta Sets** (§3.7) — extends the foundational data structure per `Notions.md`
9. **Interacting Geodesics** (§3.9) — extends our geodesic flow to multi-particle
10. **Wave Front Calculus** (§3.2) — cutting-edge 2025–2026 work, more experimental
11. **Soft Refinement & Fisk** (§3.11) — specialized but interesting

---

## 5. Key Knill Papers (arXiv)

| Paper | Year | Topic |
|---|---|---|
| [1206.0782](https://arxiv.org/abs/1206.0782) | 2012 | Brouwer fixed-point for graph endomorphisms |
| [1205.0306](https://arxiv.org/abs/1205.0306) | 2012 | Index expectation curvature |
| [1301.1408](https://arxiv.org/abs/1301.1408) | 2013 | Discrete supersymmetry (McKean–Singer) |
| [1306.5597](https://arxiv.org/abs/1306.5597) | 2013 | Deformed exterior derivatives |
| [1312.4239](https://arxiv.org/abs/1312.4239) | 2013 | Zeta functions of circular graphs |
| [1708.06070](https://arxiv.org/abs/1708.06070) | 2017 | Atiyah-Singer/Atiyah-Bott in finite geometry |
| [1801.04639](https://arxiv.org/abs/1801.04639) | 2018 | Elementary dyadic Riemann hypothesis |
| [1811.10125](https://arxiv.org/abs/1811.10125) | 2018 | Cartan's magic formula (discrete) |
| [2205.14097](https://arxiv.org/abs/2205.14097) | 2022 | Graphs, groups and geometry |
| [2401.07435](https://arxiv.org/abs/2401.07435) | 2024 | Level sets and Euler characteristic |
| [2501.14611](https://arxiv.org/abs/2501.14611) | 2025 | Wave front density conjectures |

---

## 6. Knill's Recurring Code Patterns

Knill's Mathematica code is characteristically minimal. Here are the core ~20-line building
blocks that keep reappearing across his blog:

```mathematica
(* === Core Definitions === *)
L = Length;
w[x_] := -(-1)^L[x];                           (* simplex weight = (-1)^dim *)
s[x_] := Signature[x];                          (* orientation *)
s[x_, y_] := If[SubsetQ[x, y] && L[x] == L[y] + 1,
  s[Prepend[y, Complement[x, y][[1]]]] s[x], 0]; (* incidence sign *)

(* === Complex Generation === *)
CleanComplex[G_] := Union[Sort[Map[Sort, G]]];
Generate[A_] := If[A == {}, A,
  CleanComplex[Delete[Union[Sort[Flatten[Map[Subsets, A], 1]]], 1]]];
Whitney[s_] := Generate[FindClique[s, Infinity, All]];

(* === Star and Core === *)
UU[G_, x_] := Select[G, SubsetQ[#, x] &];      (* star *)
CC[G_, x_] := Select[G, SubsetQ[x, #] &];      (* core *)

(* === Characteristics === *)
Euler[G_] := Total[Map[w, G]];
Fermi[G_] := Exp[Total[Log[Map[w, G]]]];        (* = ∏ w(x) *)

(* === Super Trace === *)
Str[G_, X_] := Sum[X[[k,k]] w[G[[k]]], {k, L[G]}];

(* === Matrices === *)
d[G_] := Table[s[G[[i]], G[[j]]], {i, L[G]}, {j, L[G]}];
Dirac[G_] := d[G] + Transpose[d[G]];
Hodge[G_] := With[{D = Dirac[G]}, D.D];

(* === Connection Matrix === *)
Connection[G_] := Table[
  Euler[Intersection[CC[G, G[[i]]], CC[G, G[[j]]]]],
  {i, L[G]}, {j, L[G]}];

(* === Green Function === *)
Green[G_] := Table[
  w[G[[i]]] w[G[[j]]] Euler[Intersection[UU[G, G[[i]]], UU[G, G[[j]]]]],
  {i, L[G]}, {j, L[G]}];
```

These map almost 1-to-1 onto our Infrageometry functions, confirming we have the right
foundations. The adaptations above would extend this core into *dynamics* (wave, deformation,
geodesic gas) and *higher invariants* (Wu, zeta, torsion).
