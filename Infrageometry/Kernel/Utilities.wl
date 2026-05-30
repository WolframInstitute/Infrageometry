Package["WolframInstitute`Infrageometry`"]

PackageScope[NestWhilePairList]



NestWhilePairList[f_, expr_, test_, m : _Integer ? Positive : 1, max : _Integer | Infinity : Infinity, n_Integer : 0, g_ : First] := 
	Block[{list = {}, args = {expr}, i = 0, stopQ = False, k = n},
		While[True,
			If[ stopQ || i > max || Length[args] == m && ! TrueQ[test @@ args],
				If[k > 0, If[! stopQ, stopQ = True]; k--, Break[]]
			];
			Replace[f @@ args,
			{
				pair : {_, next_} :> (AppendTo[list, g[pair]]; If[++i >= m, args //= Rest]; AppendTo[args, next];),
				_ :> Break[]
			}];
		];
		If[n < 0, list[[;; Max[n, - Length[list]] - 1]], list]
	]
