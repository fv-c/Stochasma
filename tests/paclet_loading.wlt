Begin["Stochasma`Private`"]

runPacletLoadingTests[repoRoot_String] := Module[
  {passed = 0, assert, expectedPublicSymbols, code, result, output},
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ paclet_loading: ", label];
    Quit[1]
  ];

  expectedPublicSymbols = Sort[{
    "categoricalForwardDiffuse",
    "categoricalForwardDiffuseAt",
    "categoricalPosterior",
    "categoricalReverseProbabilities",
    "categoricalReverseStep",
    "categoricalSample",
    "classifierFreeGuidance",
    "cosineBetaSchedule",
    "ddimSample",
    "ddpmSample",
    "epsilonPredictionLoss",
    "forwardDiffuse",
    "latentDiffusionSample",
    "linearBetaSchedule",
    "makeCategoricalSchedule",
    "makeClassifierFreeGuidedPredictor",
    "makeConditionedPredictor",
    "makeConditionedDiffusionTrainingBatch",
    "makeDiffusionSchedule",
    "makeDiffusionTrainingBatch",
    "makeDiffusionTrainingSample",
    "makeLatentDiffusionTrainingSample",
    "makeUniformCategoricalSchedule",
    "makeWolframNetPredictor",
    "predictCleanSample",
    "reverseDiffuseStep",
    "reverseMeanVariance",
    "sinusoidalTimeEmbedding",
    "uniformCategoricalTransitionKernel"
  }];

  code = StringRiffle[
    {
      "repoRoot = " <> ToString[repoRoot, InputForm] <> ";",
      "moduleIndexPath = FileNameJoin[{repoRoot, \"MODULE_INDEX.md\"}];",
      "expected = " <> ToString[expectedPublicSymbols, InputForm] <> ";",
      "loaded = PacletDirectoryLoad[repoRoot];",
      "pacletDirectoryQ = ListQ[loaded] && MemberQ[loaded, repoRoot];",
      "needsQ = Quiet[Check[Needs[\"Stochasma`\"]; True, False]];",
      "publicNames = Select[Names[\"Stochasma`*\"], Context[Evaluate[ToExpression[#]]] === \"Stochasma`\" &];",
      "actual = Sort[Last[StringSplit[#, \"`\"]] & /@ publicNames];",
      "apiQ = actual === expected;",
      "contextsQ = AllTrue[publicNames, Context[Evaluate[ToExpression[#]]] === \"Stochasma`\" &];",
      "helpersQ = Names[\"Stochasma`Private`run*Tests\"] === {};",
      "lines = StringSplit[Import[moduleIndexPath, \"Text\"], \"\\n\"];",
      "rows = Select[lines, Function[line, With[{parts = StringSplit[line, \"|\"]}, Length[parts] >= 4 && StringContainsQ[parts[[1]], \".wl\"]]]];",
      "documented = Sort[DeleteDuplicates[StringReplace[StringTrim[StringSplit[#, \"|\"][[2]]], \"`\" -> \"\"] & /@ rows]];",
      "docsQ = documented === expected;",
      "pacletObjectQ = MatchQ[PacletFind[\"Stochasma\"], {__PacletObject}];",
      "Print[\"PACLET_DIRECTORY=\", pacletDirectoryQ];",
      "Print[\"NEEDS=\", needsQ];",
      "Print[\"PUBLIC_API=\", apiQ];",
      "Print[\"PUBLIC_CONTEXT=\", contextsQ];",
      "Print[\"NO_TEST_HELPERS=\", helpersQ];",
      "Print[\"DOCUMENTED_API=\", docsQ];",
      "Print[\"PACLET_OBJECT=\", pacletObjectQ];",
      "Exit[If[And @@ {pacletDirectoryQ, needsQ, apiQ, contextsQ, helpersQ, docsQ, pacletObjectQ}, 0, 1]];"
    },
    " "
  ];
  result = RunProcess[{"wolframscript", "-code", code}, All];
  output = StringJoin[
    Lookup[result, "StandardOutput", ""],
    "\n",
    Lookup[result, "StandardError", ""]
  ];
  If[result["ExitCode"] =!= 0, Print[output]];

  assert["clean process exits successfully", result["ExitCode"] === 0];
  assert[
    "PacletDirectoryLoad returns a paclet object",
    StringContainsQ[output, "PACLET_DIRECTORY=True"]
  ];
  assert[
    "Needs loads every expected public symbol",
    StringContainsQ[output, "NEEDS=True"] &&
      StringContainsQ[output, "PUBLIC_API=True"]
  ];
  assert[
    "public symbols remain in the single public context",
    StringContainsQ[output, "PUBLIC_CONTEXT=True"]
  ];
  assert[
    "production loading exposes no test-suite helpers",
    StringContainsQ[output, "NO_TEST_HELPERS=True"]
  ];
  assert[
    "MODULE_INDEX exactly matches the public API",
    StringContainsQ[output, "DOCUMENTED_API=True"]
  ];
  assert[
    "PacletFind returns PacletObject metadata",
    StringContainsQ[output, "PACLET_OBJECT=True"]
  ];

  Print["✓ paclet_loading — ", passed, " tests passed"];
  passed
];

End[]
