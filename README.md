> ⚠️ **Actively developed, experimental research code.** It undergoes frequent cleanings and refactors, and the API may change without notice.

# Infrageometry

Discrete geometry of combinatorial objects — complexes and their closures, hypergraphs, Hodge/Dirac calculus, Green and connection matrices, Lefschetz theory, persistence, simplicial sets and maps, meshes, quantum calculus, and Forman–Ricci curvature.

Nothing here needs a distance. Everything that does now lives in [SyntheticInfrageometry](https://github.com/WolframInstitute/SyntheticInfrageometry): the **metric** layer — balls, shells, tubes, volume growth, Ollivier curvature, resistance, coordinatization, displacements, tessellations, `InfraSubstrate` — left in **2.0.0**, and the **calculus on complexes** — differential forms and cochains, the ball-intersection complexes and their filtrations — in **3.0.0** (both 2026-09-22). The two paclets are independent; neither imports the other.

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
| Simplicial sets and face graphs | Simplicial sets and the face graphs of complexes |  |

The forms-and-cochains and homotopy-transfer notebooks describe symbols that moved; they are listed in the [SyntheticInfrageometry](https://github.com/WolframInstitute/SyntheticInfrageometry) README.
