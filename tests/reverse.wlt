Begin["Stochasma`Private`"]

runReverseTests[] := Module[
  {passed = 0, assert, schedule, t, samples, noises, noisySamples,
    reconstructions, posterior, expectedMean, firstPosterior, explicitNoise,
    reverseStep, stochasticState, stochasticPrediction, firstDraw, secondDraw,
    replay1, replay2, expectedNext, actualNext, validatedPosterior,
    validatedStep},
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ reverse/predictCleanSample: ", label];
    Quit[1]
  ];

  schedule = makeDiffusionSchedule[linearBetaSchedule[8, 0.01, 0.08]];
  t = 5;
  samples = {
    1.5,
    {1., 0., -1.},
    {{1., 2.}, {3., 4.}},
    ArrayReshape[N[Range[8]], {2, 2, 2}]
  };
  noises = {
    -0.25,
    {0.25, -0.5, 1.},
    {{0.1, -0.2}, {0.3, -0.4}},
    ArrayReshape[N[Range[-4, 3]]/10., {2, 2, 2}]
  };
  noisySamples = MapThread[forwardDiffuse[#1, t, schedule, #2] &, {samples, noises}];
  reconstructions = MapThread[
    predictCleanSample[#1, t, #2, schedule] &,
    {noisySamples, noises}
  ];
  assert[
    "reconstructs scalar vector matrix and tensor clean samples",
    And @@ MapThread[numericSamplesCloseQ, {reconstructions, samples}]
  ];
  assert[
    "reconstructed shapes match xt",
    Map[Dimensions, reconstructions] === Map[Dimensions, noisySamples]
  ];
  assert[
    "t = 0 returns xt exactly",
    predictCleanSample[samples[[2]], 0, noises[[2]], schedule] === samples[[2]]
  ];
  assert[
    "same inputs are deterministic",
    predictCleanSample[noisySamples[[2]], t, noises[[2]], schedule] ===
      predictCleanSample[noisySamples[[2]], t, noises[[2]], schedule]
  ];
  assert[
    "invalid time boundaries fail",
    Quiet[predictCleanSample[samples[[2]], -1, noises[[2]], schedule]] === $Failed &&
      Quiet[predictCleanSample[samples[[2]], 9, noises[[2]], schedule]] === $Failed
  ];
  assert[
    "invalid sample and predicted noise fail",
    Quiet[
      predictCleanSample[{1., Infinity, -1.}, t, noises[[2]], schedule]
    ] === $Failed &&
      Quiet[
        predictCleanSample[samples[[2]], t, {0., Infinity, 0.}, schedule]
      ] === $Failed &&
      Quiet[
        predictCleanSample[samples[[2]], t, {0., 0.}, schedule]
      ] === $Failed
  ];
  assert[
    "malformed schedules fail",
    Quiet[
      predictCleanSample[
        samples[[2]],
        t,
        noises[[2]],
        ReplacePart[schedule, "Steps" -> 7]
      ]
    ] === $Failed
  ];

  posterior = reverseMeanVariance[
    noisySamples[[2]],
    t,
    noises[[2]],
    schedule
  ];
  expectedMean =
    schedule["PosteriorMeanCoefficient1"][[t]] samples[[2]] +
      schedule["PosteriorMeanCoefficient2"][[t]] noisySamples[[2]];
  assert[
    "posterior result exposes the documented fields",
    Keys[posterior] === {"PredictedX0", "Mean", "Variance"}
  ];
  assert[
    "posterior uses reconstructed x0",
    numericSamplesCloseQ[posterior["PredictedX0"], samples[[2]]]
  ];
  assert[
    "posterior mean matches the direct coefficient formula",
    numericSamplesCloseQ[posterior["Mean"], expectedMean]
  ];
  assert[
    "posterior variance matches the schedule coefficient",
    posterior["Variance"] === schedule["PosteriorVariances"][[t]]
  ];
  validatedPosterior = reverseMeanVarianceValidated[
    noisySamples[[2]],
    t,
    noises[[2]],
    schedule
  ];
  assert[
    "validated posterior helper is numerically identical to the public API",
    Keys[validatedPosterior] === Keys[posterior] &&
      numericSamplesCloseQ[
        validatedPosterior["PredictedX0"],
        posterior["PredictedX0"]
      ] &&
      numericSamplesCloseQ[
        validatedPosterior["Mean"],
        posterior["Mean"]
      ] &&
      validatedPosterior["Variance"] === posterior["Variance"]
  ];
  firstPosterior = reverseMeanVariance[
    forwardDiffuse[samples[[2]], 1, schedule, noises[[2]]],
    1,
    noises[[2]],
    schedule
  ];
  assert[
    "t = 1 posterior is deterministic at predicted x0",
    firstPosterior["Variance"] == 0. &&
      numericSamplesCloseQ[firstPosterior["Mean"], samples[[2]]]
  ];
  assert[
    "posterior preserves scalar vector matrix and tensor shapes",
    And @@ MapThread[
      Dimensions[reverseMeanVariance[#1, t, #2, schedule]["Mean"]] ===
        Dimensions[#1] &,
      {noisySamples, noises}
    ]
  ];
  assert[
    "posterior rejects times outside 1 through T",
    Quiet[
      reverseMeanVariance[samples[[2]], 0, noises[[2]], schedule]
    ] === $Failed &&
      Quiet[
        reverseMeanVariance[samples[[2]], 9, noises[[2]], schedule]
      ] === $Failed
  ];

  explicitNoise = {0.4, -0.3, 0.2};
  reverseStep = reverseDiffuseStep[
    noisySamples[[2]],
    t,
    noises[[2]],
    schedule,
    explicitNoise
  ];
  assert[
    "explicit-noise step matches posterior mean plus standard deviation noise",
    numericSamplesCloseQ[
      reverseStep,
      posterior["Mean"] + Sqrt[posterior["Variance"]] explicitNoise
    ]
  ];
  assert[
    "explicit-noise reverse step is deterministic",
    reverseStep === reverseDiffuseStep[
      noisySamples[[2]], t, noises[[2]], schedule, explicitNoise
    ]
  ];
  validatedStep = reverseDiffuseStepValidated[
    noisySamples[[2]],
    t,
    noises[[2]],
    schedule,
    explicitNoise
  ];
  assert[
    "validated reverse-step helper is numerically identical to the public API",
    numericSamplesCloseQ[validatedStep, reverseStep]
  ];
  assert[
    "explicit reverse noise does not consult the current random stream",
    BlockRandom[
      SeedRandom[8765];
      reverseDiffuseStep[
        noisySamples[[2]], t, noises[[2]], schedule, explicitNoise
      ];
      RandomReal[]
    ] === BlockRandom[SeedRandom[8765]; RandomReal[]]
  ];
  assert[
    "t = 1 adds no noise",
    numericSamplesCloseQ[
      reverseDiffuseStep[
        forwardDiffuse[samples[[2]], 1, schedule, noises[[2]]],
        1,
        noises[[2]],
        schedule,
        ConstantArray[10.^6, 3]
      ],
      samples[[2]]
    ]
  ];
  assert[
    "reverse step preserves scalar vector matrix and tensor shapes",
    And @@ MapThread[
      Dimensions[
        reverseDiffuseStep[#1, t, #2, schedule, 0. #1]
      ] === Dimensions[#1] &,
      {noisySamples, noises}
    ]
  ];
  stochasticState = ConstantArray[0., 64];
  stochasticPrediction = ConstantArray[0., 64];
  {firstDraw, secondDraw} = BlockRandom[
    SeedRandom[5678];
    {
      reverseDiffuseStep[
        stochasticState, t, stochasticPrediction, schedule
      ],
      reverseDiffuseStep[
        stochasticState, t, stochasticPrediction, schedule
      ]
    }
  ];
  assert[
    "consecutive stochastic reverse steps consume the current random stream",
    firstDraw =!= secondDraw
  ];
  replay1 = BlockRandom[
    SeedRandom[5678];
    reverseDiffuseStep[stochasticState, t, stochasticPrediction, schedule]
  ];
  replay2 = BlockRandom[
    SeedRandom[5678];
    reverseDiffuseStep[stochasticState, t, stochasticPrediction, schedule]
  ];
  assert[
    "resetting the current random stream reproduces reverse noise",
    replay1 === replay2
  ];
  expectedNext = BlockRandom[
    SeedRandom[5678];
    randomNormalLike[stochasticState];
    RandomReal[]
  ];
  actualNext = BlockRandom[
    SeedRandom[5678];
    reverseDiffuseStep[stochasticState, t, stochasticPrediction, schedule];
    RandomReal[]
  ];
  assert[
    "stochastic reverse step advances the stream by one same-shape draw",
    actualNext === expectedNext
  ];
  assert[
    "t = 1 does not consume the current random stream",
    BlockRandom[
      SeedRandom[5678];
      reverseDiffuseStep[stochasticState, 1, stochasticPrediction, schedule];
      RandomReal[]
    ] === BlockRandom[SeedRandom[5678]; RandomReal[]]
  ];
  assert[
    "reverse step rejects invalid explicit-noise values and shape",
    Quiet[
      reverseDiffuseStep[noisySamples[[2]], t, noises[[2]], schedule, {0.}]
    ] === $Failed &&
      Quiet[
        reverseDiffuseStep[
          noisySamples[[2]],
          t,
          noises[[2]],
          schedule,
          {0., Indeterminate, 0.}
        ]
      ] === $Failed
  ];
  assert[
    "reverse step rejects times outside 1 through T",
    Quiet[
      reverseDiffuseStep[samples[[2]], 0, noises[[2]], schedule]
    ] === $Failed &&
      Quiet[
        reverseDiffuseStep[samples[[2]], 9, noises[[2]], schedule]
      ] === $Failed &&
      Quiet[
        reverseDiffuseStep[samples[[2]], 2., noises[[2]], schedule]
      ] === $Failed
  ];

  Print["✓ reverse — ", passed, " tests passed"];
  passed
];

End[]
