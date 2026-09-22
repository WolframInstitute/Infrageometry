> ⚠️ **Actively developed, experimental research code.** It undergoes frequent cleanings and refactors, and the API may change without notice.

# Infrageometry

Discrete geometry of combinatorial objects — complexes, hypergraphs, Hodge/Dirac calculus, Forman–Ricci curvature, simplicial maps, and differential forms on graphs.

Nothing here needs a distance. The **metric** layer — balls, shells, tubes, volume growth, Ollivier curvature, resistance, coordinatization, displacements, tessellations, `InfraSubstrate` — moved to [SyntheticInfrageometry](https://github.com/WolframInstitute/SyntheticInfrageometry) in **2.0.0** (2026-09-22), where it is the Riemannian branch. The two paclets are independent; neither imports the other.

## 🎨 Founding Sketch

![Infrageometry](./infra.png)

## 📄 License

- **Code**: [MIT](https://opensource.org/license/mit)
- **Research notebooks and ideas**: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)

## ✨ Usage

Install from the Wolfram Cloud:

```wolfram
PacletInstall["https://www.wolframcloud.com/obj/hajek_pavel/Infrageometry.paclet", ForceVersionInstall -> True]
Needs["WolframInstitute`Infrageometry`"]
```


## 📓 Research Notebooks — "Math from code"

> ⚠️ LLM versions generated directly from the codebase via [ClaudePluginComputationalResearch](https://github.com/WolframInstitute/ClaudePluginComputationalResearch) with no warranty of correctness. Humans are welcome to publish their own versions alongside.

| Notebook | Description | Versions |
|---|---|---|
| Forms and cochains | Vertex-anchored forms vs. clique cochains, the ordered and alternating cochain conventions, wedge/cup/cup-1, and the Steenrod primitive on a torus | [LLM](https://www.wolframcloud.com/obj/hajek_pavel/Infrageometry/FormsAndCochains.nb) |
| Homotopy transfer on graph cochains | A-infinity products, Massey products, and the transfer to cohomology from one Hodge contraction | [LLM](https://www.wolframcloud.com/obj/hajek_pavel/Infrageometry/AInfinityTransfer.nb) |
| Simplicial sets and face graphs | Simplicial sets and the face graphs of complexes |  |
