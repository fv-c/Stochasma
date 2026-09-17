#!/usr/bin/env wolframscript

repoRoot = DirectoryName[DirectoryName[ExpandFileName[$InputFileName]]];

Get[FileNameJoin[{repoRoot, "Kernel", "Stochasma.wl"}]];

testFiles = {
  "schedules.wlt",
  "forward.wlt",
  "reverse.wlt",
  "objectives.wlt",
  "sampling.wlt",
  "ddim.wlt",
  "categorical.wlt",
  "embeddings.wlt",
  "conditioning.wlt",
  "adapters.wlt",
  "training.wlt",
  "latent.wlt",
  "paclet_loading.wlt"
};

Scan[Get[FileNameJoin[{repoRoot, "tests", #}]] &, testFiles];

testObjects = {
  VerificationTest[
    Stochasma`Private`runSchedulesTests[],
    21,
    TestID -> "schedules"
  ],
  VerificationTest[
    Stochasma`Private`runForwardTests[],
    14,
    TestID -> "forward"
  ],
  VerificationTest[
    Stochasma`Private`runReverseTests[],
    27,
    TestID -> "reverse"
  ],
  VerificationTest[
    Stochasma`Private`runObjectivesTests[],
    16,
    TestID -> "objectives"
  ],
  VerificationTest[
    Stochasma`Private`runSamplingTests[],
    18,
    TestID -> "sampling"
  ],
  VerificationTest[
    Stochasma`Private`runDDIMTests[],
    33,
    TestID -> "ddim"
  ],
  VerificationTest[
    Stochasma`Private`runCategoricalTests[],
    70,
    TestID -> "categorical"
  ],
  VerificationTest[
    Stochasma`Private`runEmbeddingsTests[],
    18,
    TestID -> "embeddings"
  ],
  VerificationTest[
    Stochasma`Private`runConditioningTests[],
    36,
    TestID -> "conditioning"
  ],
  VerificationTest[
    Stochasma`Private`runAdaptersTests[],
    12,
    TestID -> "adapters"
  ],
  VerificationTest[
    Stochasma`Private`runTrainingTests[],
    28,
    TestID -> "training"
  ],
  VerificationTest[
    Stochasma`Private`runLatentTests[],
    28,
    TestID -> "latent"
  ],
  VerificationTest[
    Stochasma`Private`runPacletLoadingTests[repoRoot],
    7,
    TestID -> "paclet_loading"
  ]
};

report = TestReport[testObjects];

If[
  report["TestsFailedCount"] === 0 &&
    report["TestsSucceededCount"] === Length[testObjects],
  Print["✓ all tests — 328 tests passed"];
  Exit[0],
  Print[
    "✗ all tests: ",
    report["TestsFailedCount"],
    " suite checks failed"
  ];
  Exit[1]
];
