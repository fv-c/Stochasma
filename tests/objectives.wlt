Begin["Stochasma`Private`"]

runObjectivesTests[] := Module[
  {passed = 0, assert, schedule, x0, noise, t, sample, samples,
    stochasticInput, firstDraw, secondDraw, replay1, replay2,
    expectedNext, actualNext},
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ objectives/makeDiffusionTrainingSample: ", label];
    Quit[1]
  ];

  schedule = makeDiffusionSchedule[linearBetaSchedule[8, 0.01, 0.08]];
  x0 = {1., 0., -1.};
  noise = {0.25, -0.5, 1.};
  t = 5;
  sample = makeDiffusionTrainingSample[x0, t, schedule, noise];
  assert[
    "training sample exposes the documented fields",
    Keys[sample] === {"Clean", "Noisy", "Time", "Noise"}
  ];
  assert[
    "training sample retains clean input time and explicit noise",
    sample["Clean"] === x0 && sample["Time"] === t &&
      sample["Noise"] === noise
  ];
  assert[
    "noisy field matches closed-form forward diffusion",
    sample["Noisy"] === forwardDiffuse[x0, t, schedule, noise]
  ];
  assert[
    "explicit-noise training sample is deterministic",
    sample === makeDiffusionTrainingSample[x0, t, schedule, noise]
  ];
  assert[
    "explicit training noise does not consult the current random stream",
    BlockRandom[
      SeedRandom[6420];
      makeDiffusionTrainingSample[x0, t, schedule, noise];
      RandomReal[]
    ] === BlockRandom[SeedRandom[6420]; RandomReal[]]
  ];
  samples = {
    1.5,
    {1., 2.},
    {{1., 2.}, {3., 4.}},
    ArrayReshape[N[Range[8]], {2, 2, 2}]
  };
  assert[
    "scalar vector matrix and tensor shapes are preserved",
    AllTrue[
      samples,
      Dimensions[
        makeDiffusionTrainingSample[#, t, schedule, 0. #]["Noisy"]
      ] === Dimensions[#] &
    ]
  ];
  stochasticInput = ConstantArray[0., 64];
  {firstDraw, secondDraw} = BlockRandom[
    SeedRandom[2468];
    {
      makeDiffusionTrainingSample[stochasticInput, t, schedule],
      makeDiffusionTrainingSample[stochasticInput, t, schedule]
    }
  ];
  assert[
    "consecutive stochastic training samples consume the current random stream",
    firstDraw =!= secondDraw
  ];
  replay1 = BlockRandom[
    SeedRandom[2468];
    makeDiffusionTrainingSample[stochasticInput, t, schedule]
  ];
  replay2 = BlockRandom[
    SeedRandom[2468];
    makeDiffusionTrainingSample[stochasticInput, t, schedule]
  ];
  assert[
    "resetting the current random stream reproduces training noise",
    replay1 === replay2
  ];
  expectedNext = BlockRandom[
    SeedRandom[2468];
    randomNormalLike[stochasticInput];
    RandomReal[]
  ];
  actualNext = BlockRandom[
    SeedRandom[2468];
    makeDiffusionTrainingSample[stochasticInput, t, schedule];
    RandomReal[]
  ];
  assert[
    "stochastic training sample advances the stream by one same-shape draw",
    actualNext === expectedNext
  ];
  assert[
    "invalid training times fail",
    Quiet[makeDiffusionTrainingSample[x0, 0, schedule, noise]] === $Failed &&
      Quiet[makeDiffusionTrainingSample[x0, 9, schedule, noise]] === $Failed &&
      Quiet[makeDiffusionTrainingSample[x0, 2., schedule, noise]] === $Failed
  ];
  assert[
    "invalid samples, noise, and malformed schedules fail",
    Quiet[
      makeDiffusionTrainingSample[
        {1., Infinity, -1.}, t, schedule, noise
      ]
    ] === $Failed &&
      Quiet[
        makeDiffusionTrainingSample[
          x0, t, schedule, {0., Indeterminate, 0.}
        ]
      ] === $Failed &&
    Quiet[makeDiffusionTrainingSample[x0, t, schedule, {0.}]] === $Failed &&
      Quiet[
        makeDiffusionTrainingSample[
          x0,
          t,
          ReplacePart[schedule, "Steps" -> 7],
          noise
        ]
      ] === $Failed
  ];

  assert[
    "identical predictions have zero loss",
    epsilonPredictionLoss[noise, noise] == 0
  ];
  assert[
    "vector loss is elementwise mean squared error",
    epsilonPredictionLoss[{1., 2.}, {0., 0.}] == 2.5
  ];
  assert[
    "scalar loss is squared error",
    epsilonPredictionLoss[3., 1.] == 4.
  ];
  assert[
    "matrix and tensor losses reduce over every element",
    epsilonPredictionLoss[{{1., -1.}, {1., -1.}}, ConstantArray[0., {2, 2}]] == 1. &&
      epsilonPredictionLoss[
        ArrayReshape[Range[8], {2, 2, 2}],
        ConstantArray[0, {2, 2, 2}]
      ] == Mean[Range[8]^2]
  ];
  assert[
    "mismatched shapes and non-finite values fail",
    Quiet[epsilonPredictionLoss[{1., 2.}, {1.}]] === $Failed &&
      Quiet[epsilonPredictionLoss[{1., Infinity}, {1., 2.}]] === $Failed
  ];

  Print["✓ objectives — ", passed, " tests passed"];
  passed
];

End[]
