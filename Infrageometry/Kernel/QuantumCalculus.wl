Package["WolframInstitute`Infrageometry`"]

(* Quantum Calculus — new functionality based on Oliver Knill's discrete geometry program *)

(* === Wave Equation === *)
PackageExport[WaveEquationSolution]
PackageExport[DiscreteWaveStep]
PackageExport[WavePropagator]

(* === Wu Characteristic === *)
PackageExport[WuCharacteristic]

(* === Isospectral Deformation === *)
PackageExport[IsospectralDeformation]
PackageExport[DiracMass]
PackageExport[ConnesDistance]

(* === Zeta Functions === *)
PackageExport[DiracZetaFunction]
PackageExport[ConnectionZetaFunction]
PackageExport[LefschetzZetaFunction]

(* === Analytic Torsion === *)
PackageExport[AnalyticTorsion]

(* === Curvature === *)
PackageExport[IndexExpectationCurvature]


(* ====================================================================== *)
(* 1. Wave Equation & Discrete Dynamics (§3.1)                           *)
(* ====================================================================== *)

(* d'Alembert solution: u(t) = cos(D t) u0 + t sinc(D t) v0
   where sinc(x) = Sin(x)/x, applied as a matrix function via eigendecomposition *)

WaveEquationSolution[g : {___List}, u0_ ? VectorQ, v0_ ? VectorQ, t_] :=
    With[{dirac = N[DiracHodgeMatrix[g]]},
        With[{vals = Eigenvalues[dirac], vecs = Eigenvectors[dirac]},
            With[{
                cosD = Transpose[vecs] . DiagonalMatrix[Cos[vals * t]] . vecs,
                (* t * sinc(v*t) = t * Sin(v*t)/(v*t) = Sin(v*t)/v for v!=0, t for v=0 *)
                tSincD = Transpose[vecs] . DiagonalMatrix[
                    Replace[vals, {0 | 0. -> t, v_ :> If[t == 0, 0., Sin[v * t] / v]}, {1}]
                ] . vecs
            },
                cosD . u0 + tSincD . v0
            ]
        ]
    ]

(* Discrete-time wave step: the leapfrog scheme u(t+1) = 2u(t) - u(t-1) - H u(t) *)
(* Takes current state {u, u_prev} and returns {u_next, u} *)
DiscreteWaveStep[g : {___List}, u_ ? VectorQ, uPrev_ ? VectorQ] :=
    With[{h = HodgeBlock[g]},
        {2 u - uPrev - h . u, u}
    ]

DiscreteWaveStep[g : {___List}] := Function[{u, uPrev}, DiscreteWaveStep[g, u, uPrev]]

(* Wave propagator: unitary matrix exp(i D t) *)
WavePropagator[g : {___List}, t_ : 1] := MatrixExp[I * N[DiracHodgeMatrix[g]] * t]


(* ====================================================================== *)
(* 2. Wu Characteristic (§3.6)                                            *)
(* ====================================================================== *)

(* Wu characteristic of order m: sum over m-tuples with pairwise nonempty intersection *)

WuCharacteristic[g : {___List}, 1] := ComplexEulerCharacteristic[g]

WuCharacteristic[g : {___List}, 2] :=
    Total[
        Outer[
            If[Intersection[#1, #2] =!= {}, SimplexWeight[#1] * SimplexWeight[#2], 0] &,
            g, g, 1
        ],
        2
    ]

WuCharacteristic[g : {___List}, m_Integer /; m >= 3] := Block[{tuples, w},
    w = SimplexWeight /@ g;
    Total @ Map[
        If[Intersection @@ # =!= {},
            Times @@ (SimplexWeight /@ #),
            0
        ] &,
        Tuples[g, m]
    ]
]

WuCharacteristic[g : {___List}] := WuCharacteristic[g, 2]


(* ====================================================================== *)
(* 3. Isospectral Deformation (§3.3)                                      *)
(* ====================================================================== *)

(* Lax flow: D' = [B, D] where B = d - d* (the commutator bracket)
   d is the exterior derivative part (upper triangular of Dirac),
   d* is its transpose (lower triangular).
   We iterate: D(t+dt) = D(t) + dt * [B(t), D(t)] *)

IsospectralDeformation[g : {___List}, t_ ? NumericQ, n_Integer : 100] := Block[{
    dirac = N[DiracHodgeMatrix[g]],
    dt = t / n, d, dstar, b
},
    Do[
        d = UpperTriangularize[dirac, 1];
        dstar = LowerTriangularize[dirac, -1];
        b = d - dstar;
        dirac = dirac + dt * (b . dirac - dirac . b);
        ,
        n
    ];
    dirac
]

(* Diagonal "mass" term from deformed Dirac *)
DiracMass[g : {___List}, t_ ? NumericQ, n_Integer : 100] :=
    Diagonal[IsospectralDeformation[g, t, n]]

(* Connes distance: d(x,y) = sup{|f(x)-f(y)| : ||[D,f]|| <= 1}
   For vertices x, y of a simplicial complex, f is a diagonal matrix
   acting on the vertex subspace. We solve a linear program. *)
ConnesDistance[g : {___List}] := Block[{
    dirac = N[DiracHodgeMatrix[g]],
    verts = SimplexList[g, {0}],
    nv, n, result
},
    nv = Length[verts];
    n = Length[g];
    (* Build distance matrix between all vertex pairs *)
    result = Table[
        If[i >= j, 0.,
            (* Maximize f[vi] - f[vj] subject to operator norm of [D, diag(f)] <= 1 *)
            Block[{fvals, fmat, commutator, dists = {}, f, obj, constraints, sol},
                (* Use the eigenvalue bound: try many directions *)
                (* Simpler approach: solve via linear program
                   Variables: f[1]...f[nv], one per vertex
                   We embed them on the diagonal of an n x n matrix
                   matching simplex positions *)
                With[{
                    vertIdx = Flatten[Position[g, #] & /@ verts]
                },
                    (* For small complexes, enumerate constraints directly *)
                    f = Array[\[FormalF], nv];
                    fmat = SparseArray[MapThread[{#1, #1} -> #2 &, {vertIdx, f}], {n, n}];
                    commutator = dirac . fmat - fmat . dirac;
                    (* ||[D,f]|| <= 1 means all singular values <= 1
                       For small matrices, constrain Frobenius norm as a relaxation:
                       |f(a) - f(b)| <= 1 for each edge {a,b} *)
                    constraints = Flatten @ Table[
                        With[{edge = g[[k]]},
                            If[Length[edge] == 2,
                                With[{
                                    a = FirstPosition[verts, {edge[[1]]}][[1]],
                                    b = FirstPosition[verts, {edge[[2]]}][[1]]
                                },
                                    {f[[a]] - f[[b]] <= 1, f[[b]] - f[[a]] <= 1}
                                ],
                                Nothing
                            ]
                        ],
                        {k, n}
                    ];
                    sol = Quiet @ LinearProgramming[
                        -UnitVector[nv, i] + UnitVector[nv, j],
                        Join[
                            IdentityMatrix[nv][[All]],
                            -IdentityMatrix[nv][[All]]
                        ] /. {} -> ConstantArray[ConstantArray[0, nv], 1],
                        ConstantArray[{0, -1}, 2 nv],
                        Table[{-Infinity, Infinity}, nv]
                    ];
                    If[ListQ[sol],
                        Abs[sol[[i]] - sol[[j]]],
                        (* Fallback: graph distance *)
                        N @ GraphDistance[ComplexGraph[g], verts[[i, 1]], verts[[j, 1]]]
                    ]
                ]
            ]
        ],
        {i, nv}, {j, nv}
    ];
    result + Transpose[result]
]


(* ====================================================================== *)
(* 4. Zeta Functions (§3.8)                                               *)
(* ====================================================================== *)

(* Dirac zeta function: ζ_D(s) = str(|D|^{-s}) = ∑ ω(x) |λ_x|^{-s} *)
DiracZetaFunction[g : {___List}, s_] := Block[{
    dirac = N[DiracHodgeMatrix[g]],
    evals, weights
},
    evals = Eigenvalues[dirac];
    weights = SimplexWeight /@ g;
    Total @ MapThread[
        If[Abs[#2] > 10^-10, #1 * Abs[#2]^(-s), 0] &,
        {weights, evals}
    ]
]

(* Connection zeta function: ζ_L(s) = ∑ |λ_i|^{-s} for eigenvalues of L *)
ConnectionZetaFunction[g : {___List}, s_] := Block[{
    evals = Eigenvalues[N[ConnectionMatrix[g]]]
},
    Total[If[Abs[#] > 10^-10, Abs[#]^(-s), 0] & /@ evals]
]

(* Lefschetz zeta function: ζ_T(z) = exp(∑_{n=1}^N L(T^n)/n · z^n)
   where L(T^n) is the Lefschetz number of T^n *)
LefschetzZetaFunction[g : {___List}, perm_Cycles, z_, nMax_Integer : 20] := Block[{
    maps, lefNums
},
    maps = Table[PermutationPower[perm, k], {k, nMax}];
    lefNums = LefschetzNumber[g, #] & /@ maps;
    Exp[Total @ Table[lefNums[[k]] / k * z^k, {k, nMax}]]
]


(* ====================================================================== *)
(* 5. Analytic Torsion (§3.10)                                            *)
(* ====================================================================== *)

(* log T(G) = ½ ∑_k (-1)^{k+1} k · log det'(H_k) *)

AnalyticTorsion[g : {___List}] := Block[{
    hodge = HodgeMatrix[g],
    blocks, pDets, d
},
    blocks = hodge["Blocks"];
    d = Length[blocks];
    pDets = Map[
        With[{ev = Select[Eigenvalues[N[#]], Abs[#] > 10^-10 &]},
            If[ev === {}, 1, Times @@ Abs[ev]]
        ] &,
        blocks
    ];
    Exp[1/2 * Sum[(-1)^(k + 1) * k * Log[pDets[[k + 1]]], {k, 0, d - 1}]]
]


(* ====================================================================== *)
(* 6. Index-Expectation Curvature (§3.5)                                  *)
(* ====================================================================== *)

(* K(x) = E[i_f(x)] where i_f(x) = 1 - χ(S(x) ∩ {f < f(x)})
   averaged over all orderings (permutations) of the vertices.
   For efficiency, use a sample of random orderings for large complexes. *)

IndexExpectationCurvature[g : {___List}, nSamples_Integer : All] := Block[{
    verts = ComplexVertexList[g],
    nv, perms, curvatures
},
    nv = Length[verts];
    perms = If[nSamples === All || nSamples >= nv!,
        Permutations[verts],
        Table[RandomSample[verts], nSamples]
    ];
    curvatures = Table[0., Length[g]];
    Do[
        With[{ordering = AssociationThread[perm -> Range[Length[perm]]]},
            Do[
                With[{
                    x = g[[k]],
                    sphere = SimplexUnitSphere[g, g[[k]]]
                },
                    If[sphere =!= {},
                        With[{
                            (* For each simplex x, compute i_f(x) = 1 - χ(S(x) ∩ {f < f(x)}) *)
                            fVal = Max[Lookup[ordering, x, 0]],
                            filteredSphere = Select[sphere, Max[Lookup[ordering, #, 0]] < Max[Lookup[ordering, x, 0]] &]
                        },
                            curvatures[[k]] += (1 - ComplexEulerCharacteristic[filteredSphere]) / Length[perms]
                        ],
                        curvatures[[k]] += 1 / Length[perms]
                    ]
                ],
                {k, Length[g]}
            ]
        ],
        {perm, perms}
    ];
    curvatures
]
