#!/usr/bin/env wolframscript

root = DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{root, "Kernel", "Stochasma.wl"}]];

counts = {
  Stochasma`Private`runSchedulesTests[],
  Stochasma`Private`runForwardTests[],
  Stochasma`Private`runReverseTests[],
  Stochasma`Private`runObjectivesTests[],
  Stochasma`Private`runSamplingTests[],
  Stochasma`Private`runDDIMTests[],
  Stochasma`Private`runCategoricalTests[],
  Stochasma`Private`runEmbeddingsTests[],
  Stochasma`Private`runAdaptersTests[],
  Stochasma`Private`runTrainingTests[],
  Stochasma`Private`runLatentTests[]
};

If[AllTrue[counts, IntegerQ],
  Print["✓ all tests — ", Total[counts], " tests passed"],
  Quit[1]
];
