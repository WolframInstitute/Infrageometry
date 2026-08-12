#!/usr/bin/env wolframscript
(* Rebuild Assets/ExampleGraphs.wxf, the frozen substrate table behind $ExampleGraphs.

   Run this after changing any generator in Kernel/ExampleGraphs.wl or the tier table
   exampleGraphTiers.  The named-tier clause of ExampleGraphData reads the frozen table, so the
   generators are reached here through the raw-spec clause.  The seed is fixed: DiscretizeRegion's
   triangulation and InflateGraph's fibers are random, and every documentation figure depends on
   getting the same graphs back. *)

$seed = 20260812;

paclet = ParentDirectory @ DirectoryName @ $InputFileName;
PacletDirectoryLoad[paclet];
Needs["WolframInstitute`Infrageometry`"];

tiers = WolframInstitute`Infrageometry`PackageScope`exampleGraphTiers;

data = Association @ Table[
  name -> Association @ Table[
    size -> (SeedRandom[$seed]; ExampleGraphData[name, tiers[name, size]]),
    {size, Keys @ tiers @ name}],
  {name, Keys @ tiers}];

file = FileNameJoin[{paclet, "Assets", "ExampleGraphs.wxf"}];
Export[file, data, "WXF"];

Print["wrote ", file, " (", FileByteCount[file], " bytes)"];
Print[Map[VertexCount, data, {2}]];
