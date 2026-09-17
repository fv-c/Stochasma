Begin["Stochasma`Private`"]

runCategoricalTests[] := Module[
  {
    passed = 0, assert, kernel, expected, permutation, states,
    randomSample, expectedNoise, q1, q2, q3, schedule, uniformBetas,
    uniformSchedule, x0, noise, t, samples, expectedNext, actualNext,
    replay1, replay2, withinToleranceKernel, overToleranceKernel,
    toleranceSamples, posteriorQ1, posteriorQ2, posteriorQ3,
    posteriorSchedule, posterior, manualUnnormalized, manualPosterior,
    reverseProbabilities, oneHotReverse, qbarPrevious, priorPrevious,
    likelihood, manualReverse, posteriorComponents, posteriorMixture,
    sparseSchedule, reverseStepSchedule,
    predictedX0, reverseStepManual, reverseStepSamples, samplerSchedule,
    samplerPredictor, samplerUniforms, samplerExpected, samplerResult,
    samplerTimeTrace, samplerAutomatic1, samplerAutomatic2,
    samplerSeeded1, samplerSeeded2, samplerDifferentSeed1,
    samplerDifferentSeed2, samplerSeedSchedule, validatedPosterior,
    validatedReverse, validatedStep, validationTrace
  },
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ categorical: ", label];
    Quit[1]
  ];

  kernel = uniformCategoricalTransitionKernel[4, 0.2];
  expected = ConstantArray[0.05, {4, 4}] + 0.8 IdentityMatrix[4];
  assert[
    "matches the uniform-corruption formula",
    Max[Abs[Flatten[kernel - expected]]] < 10^-14
  ];
  assert[
    "is row-stochastic and non-negative",
    Max[Abs[Total[kernel, {2}] - ConstantArray[1., 4]]] < 10^-14 &&
      Min[kernel] >= 0
  ];
  assert[
    "preserves the uniform distribution",
    Max[Abs[ConstantArray[1./4, 4] . kernel - ConstantArray[1./4, 4]]] <
      10^-14
  ];
  assert[
    "has identity and uniform endpoint kernels",
    uniformCategoricalTransitionKernel[3, 0] === N[IdentityMatrix[3]] &&
      uniformCategoricalTransitionKernel[3, 1] ===
        ConstantArray[1./3, {3, 3}]
  ];
  permutation = IdentityMatrix[4][[{3, 1, 4, 2}]];
  assert[
    "is invariant under category relabeling",
    Max[Abs[Flatten[
      permutation . kernel . Transpose[permutation] - kernel
    ]]] < 10^-14
  ];
  assert[
    "rejects invalid category counts, beta values, and arity",
    Quiet[uniformCategoricalTransitionKernel[1, 0.2]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3.5, 0.2]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3, -0.1]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3, 1.1]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3, Infinity]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3]] === $Failed
  ];

  q1 = {{0.9, 0.1}, {0.2, 0.8}};
  q2 = {{0.6, 0.4}, {0.1, 0.9}};
  q3 = {{0.7, 0.3}, {0.4, 0.6}};
  schedule = makeCategoricalSchedule[{q1, q2, q3}];
  assert[
    "constructs the categorical schedule structure",
    schedule["Steps"] === 3 &&
      schedule["CategoryCount"] === 2 &&
      Length[schedule["TransitionKernels"]] === 3 &&
      Length[schedule["CumulativeTransitionKernels"]] === 3
  ];
  assert[
    "uses ordered non-commutative cumulative products",
    !categoricalNumericArraysCloseQ[q1 . q2, q2 . q1] &&
      categoricalNumericArraysCloseQ[
        schedule["CumulativeTransitionKernels"][[2]],
        q1 . q2
      ] &&
      categoricalNumericArraysCloseQ[
        schedule["CumulativeTransitionKernels"][[3]],
        q1 . q2 . q3
      ]
  ];
  assert[
    "keeps every cumulative transition row-stochastic",
    AllTrue[
      schedule["CumulativeTransitionKernels"],
      categoricalTransitionKernelCategoryCount[#] === 2 &
    ]
  ];
  assert[
    "validates the complete categorical schedule representation",
    categoricalScheduleQ[schedule] &&
      !categoricalScheduleQ[
        ReplacePart[
          schedule,
          "CumulativeTransitionKernels" -> {q1, q2 . q1, q3 . q2 . q1}
        ]
      ] &&
      !categoricalScheduleQ[KeyDrop[schedule, "CategoryCount"]]
  ];
  assert[
    "rejects invalid categorical schedule inputs",
    Quiet[makeCategoricalSchedule[{}]] === $Failed &&
      Quiet[makeCategoricalSchedule[{{{1., 0., 0.}, {0., 1., 0.}}}]] ===
        $Failed &&
      Quiet[makeCategoricalSchedule[{IdentityMatrix[2], IdentityMatrix[3]}]] ===
        $Failed &&
      Quiet[makeCategoricalSchedule[{{{1.1, -0.1}, {0., 1.}}}]] ===
        $Failed &&
      Quiet[makeCategoricalSchedule[{{{1., 0.}, {0., Infinity}}}]] ===
        $Failed &&
      Quiet[makeCategoricalSchedule[{{{1., 0.}, {0., Indeterminate}}}]] ===
        $Failed &&
      Quiet[makeCategoricalSchedule[{{{1., 0.}, {0.2, 0.7}}}]] ===
        $Failed &&
      Quiet[makeCategoricalSchedule[IdentityMatrix[2]]] === $Failed
  ];

  uniformBetas = {0., 0.2, 0.5, 1.};
  uniformSchedule = makeUniformCategoricalSchedule[3, uniformBetas];
  assert[
    "constructs uniform transition kernels from every beta",
    uniformSchedule["Steps"] === Length[uniformBetas] &&
      uniformSchedule["CategoryCount"] === 3 &&
      And @@ MapThread[
        categoricalNumericArraysCloseQ,
        {
          uniformSchedule["TransitionKernels"],
          uniformCategoricalTransitionKernel[3, #] & /@ uniformBetas
        }
      ]
  ];
  assert[
    "uniform schedule includes identity and uniform endpoints",
    uniformSchedule["TransitionKernels"][[1]] ===
      N[IdentityMatrix[3]] &&
      uniformSchedule["TransitionKernels"][[-1]] ===
        ConstantArray[1./3, {3, 3}]
  ];
  assert[
    "rejects invalid uniform categorical schedule inputs",
    Quiet[makeUniformCategoricalSchedule[1, {0.2}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3., {0.2}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3, {}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3, {-0.1}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3, {1.1}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3, {Infinity}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3, {Indeterminate}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3, 0.2]] === $Failed
  ];

  posteriorQ1 = {
    {0.7, 0.2, 0.1},
    {0.1, 0.6, 0.3},
    {0.2, 0.1, 0.7}
  };
  posteriorQ2 = {
    {0.5, 0.4, 0.1},
    {0.2, 0.7, 0.1},
    {0.3, 0.2, 0.5}
  };
  posteriorQ3 = {
    {0.6, 0.1, 0.3},
    {0.25, 0.5, 0.25},
    {0.15, 0.35, 0.5}
  };
  posteriorSchedule = makeCategoricalSchedule[
    {posteriorQ1, posteriorQ2, posteriorQ3}
  ];
  posterior = categoricalPosterior[2, 3, 3, posteriorSchedule];
  manualUnnormalized =
    (posteriorQ1 . posteriorQ2)[[2]] posteriorQ3[[All, 3]];
  manualPosterior = manualUnnormalized/Total[manualUnnormalized];
  assert[
    "uses the ordered non-commutative cumulative kernel in the posterior",
    !categoricalNumericArraysCloseQ[
      posteriorQ1 . posteriorQ2,
      posteriorQ2 . posteriorQ1
    ] &&
      categoricalNumericArraysCloseQ[posterior, manualPosterior]
  ];
  assert[
    "returns a normalized non-negative posterior of category length",
    Length[posterior] === 3 &&
      Min[posterior] >= 0 &&
      Abs[Total[posterior] - 1.] < 10^-14
  ];
  validatedPosterior = categoricalPosteriorValidated[
    2,
    3,
    3,
    posteriorSchedule
  ];
  assert[
    "validated posterior helper is identical to the public API",
    categoricalNumericArraysCloseQ[validatedPosterior, posterior]
  ];
  assert[
    "reduces the t = 1 posterior to the clean-state point mass",
    categoricalPosterior[2, 3, 1, posteriorSchedule] === {0., 1., 0.}
  ];
  assert[
    "rejects malformed posterior schedules",
    Quiet[categoricalPosterior[
      1,
      1,
      1,
      KeyDrop[posteriorSchedule, "CumulativeTransitionKernels"]
    ]] === $Failed &&
      Quiet[categoricalPosterior[1, 1, 1, {posteriorQ1}]] === $Failed
  ];
  assert[
    "rejects invalid posterior times",
    Quiet[categoricalPosterior[1, 1, 0, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[1, 1, 4, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[1, 1, 1.5, posteriorSchedule]] === $Failed
  ];
  assert[
    "rejects non-Integer and out-of-range posterior states",
    Quiet[categoricalPosterior[1.0, 1, 1, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[1, 1.0, 1, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[0, 1, 1, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[4, 1, 1, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[1, 0, 1, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[1, 4, 1, posteriorSchedule]] === $Failed
  ];
  assert[
    "rejects a posterior with zero normalization",
    Quiet[categoricalPosterior[
      1,
      2,
      1,
      makeCategoricalSchedule[{IdentityMatrix[3]}]
    ]] === $Failed
  ];

  reverseProbabilities = categoricalReverseProbabilities[
    3,
    3,
    {0.2, 0.5, 0.3},
    posteriorSchedule
  ];
  qbarPrevious = posteriorQ1 . posteriorQ2;
  posteriorComponents = Table[
    categoricalPosterior[index, 3, 3, posteriorSchedule],
    {index, 3}
  ];
  posteriorMixture = {0.2, 0.5, 0.3} . posteriorComponents;
  priorPrevious = {0.2, 0.5, 0.3} . qbarPrevious;
  likelihood = posteriorQ3[[All, 3]];
  manualReverse = priorPrevious likelihood;
  manualReverse = manualReverse/Total[manualReverse];
  assert[
    "uses canonical D3PM joint marginalization rather than a posterior mixture",
    !categoricalNumericArraysCloseQ[
      posteriorQ1 . posteriorQ2,
      posteriorQ2 . posteriorQ1
    ] &&
      categoricalNumericArraysCloseQ[reverseProbabilities, manualReverse] &&
      !categoricalNumericArraysCloseQ[
        reverseProbabilities,
        posteriorMixture
      ] &&
      Length[reverseProbabilities] === 3 &&
      Min[reverseProbabilities] >= 0 &&
      Abs[Total[reverseProbabilities] - 1.] < 10^-14
  ];
  validatedReverse = categoricalReverseProbabilitiesValidated[
    3,
    3,
    {0.2, 0.5, 0.3},
    posteriorSchedule
  ];
  assert[
    "validated reverse-probability helper is identical to the public API",
    categoricalNumericArraysCloseQ[
      validatedReverse,
      reverseProbabilities
    ]
  ];
  oneHotReverse = categoricalReverseProbabilities[
    3,
    3,
    {0., 1., 0.},
    posteriorSchedule
  ];
  assert[
    "reduces a one-hot x0 prediction to the exact posterior",
    categoricalNumericArraysCloseQ[
      oneHotReverse,
      categoricalPosterior[2, 3, 3, posteriorSchedule]
    ]
  ];
  assert[
    "returns predicted x0 directly at t = 1 for every observed state",
    AllTrue[
      Range[posteriorSchedule["CategoryCount"]],
      Function[observedState,
        categoricalNumericArraysCloseQ[
          categoricalReverseProbabilities[
            observedState,
            1,
            {0.2, 0.3, 0.5},
            posteriorSchedule
          ],
          {0.2, 0.3, 0.5}
        ]
      ]
    ]
  ];
  assert[
    "rejects invalid predicted x0 probability vectors",
    Quiet[categoricalReverseProbabilities[
      3,
      3,
      {0.5, 0.5},
      posteriorSchedule
    ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        3,
        {0.5, -0.1, 0.6},
        posteriorSchedule
      ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        3,
        {0.2, 0.2, 0.2},
        posteriorSchedule
      ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        3,
        {Infinity, 0., 0.},
        posteriorSchedule
      ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        3,
        1.,
        posteriorSchedule
      ]] === $Failed
  ];
  assert[
    "rejects invalid reverse-probability state time and schedule inputs",
    Quiet[categoricalReverseProbabilities[
      0,
      3,
      {0.2, 0.5, 0.3},
      posteriorSchedule
    ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        0,
        {0.2, 0.5, 0.3},
        posteriorSchedule
      ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        3.0,
        {0.2, 0.5, 0.3},
        posteriorSchedule
      ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        3,
        {0.2, 0.5, 0.3},
        KeyDrop[posteriorSchedule, "TransitionKernels"]
      ]] === $Failed
  ];
  sparseSchedule = makeCategoricalSchedule[
    {IdentityMatrix[3], IdentityMatrix[3]}
  ];
  assert[
    "returns the full predicted distribution at t = 1 without likelihood weighting",
    categoricalReverseProbabilities[
      2,
      1,
      {0.4, 0.6, 0.},
      sparseSchedule
    ] === {0.4, 0.6, 0.}
  ];
  assert[
    "returns a one-hot prediction directly at t = 1 despite zero likelihood",
    categoricalReverseProbabilities[
      2,
      1,
      {1., 0., 0.},
      sparseSchedule
    ] === {1., 0., 0.}
  ];
  assert[
    "rejects a t > 1 reverse distribution with zero joint normalization",
    Quiet[categoricalReverseProbabilities[
      2,
      2,
      {1., 0., 0.},
      sparseSchedule
    ]] === $Failed &&
      Quiet[
        Check[
          categoricalReverseProbabilities[
            2,
            2,
            {1., 0., 0.},
            sparseSchedule
          ];
          False,
          True,
          {categoricalReverseProbabilities::posterior}
        ],
        {categoricalReverseProbabilities::posterior}
      ]
  ];

  reverseStepSchedule = makeUniformCategoricalSchedule[3, {0.2}];
  predictedX0 = {0.2, 0.3, 0.5};
  reverseStepManual = predictedX0;
  assert[
    "uses the direct t = 1 prediction and correct cumulative quantiles",
    categoricalNumericArraysCloseQ[
      categoricalReverseProbabilities[
        2,
        1,
        predictedX0,
        reverseStepSchedule
      ],
      reverseStepManual
    ] &&
      categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        0.
      ] === 1 &&
      categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        0.2 - 10^-12
      ] === 1 &&
      categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        0.2
      ] === 2 &&
      categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        0.5 - 10^-12
      ] === 2 &&
      categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        0.5
      ] === 3
  ];
  validatedStep = categoricalReverseStepValidated[
    2,
    1,
    predictedX0,
    reverseStepSchedule,
    0.3
  ];
  assert[
    "validated reverse-step helper is identical to the public API",
    validatedStep === categoricalReverseStep[
      2,
      1,
      predictedX0,
      reverseStepSchedule,
      0.3
    ]
  ];
  reverseStepSamples = BlockRandom[
    SeedRandom[9127];
    {
      categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule],
      categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule]
    }
  ];
  assert[
    "automatic reverse steps return valid categories and consume RNG",
    AllTrue[reverseStepSamples, IntegerQ[#] && 1 <= # <= 3 &] &&
      (expectedNext = BlockRandom[
        SeedRandom[9127];
        RandomReal[];
        RandomReal[];
        RandomReal[]
      ];
      actualNext = BlockRandom[
        SeedRandom[9127];
        categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule];
        categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule];
        RandomReal[]
      ];
      actualNext === expectedNext)
  ];
  assert[
    "resetting the external seed reproduces reverse-step samples",
    (replay1 = BlockRandom[
      SeedRandom[2269];
      {
        categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule],
        categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule]
      }
    ];
    replay2 = BlockRandom[
      SeedRandom[2269];
      {
        categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule],
        categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule]
      }
    ];
    replay1 === replay2)
  ];
  assert[
    "explicit reverse-step noise does not consult the random stream",
    BlockRandom[
      SeedRandom[2269];
      categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        0.5
      ];
      RandomReal[]
    ] === BlockRandom[SeedRandom[2269]; RandomReal[]]
  ];
  assert[
    "rejects invalid reverse-step noise and arity",
    Quiet[categoricalReverseStep[
      2,
      1,
      predictedX0,
      reverseStepSchedule,
      -0.1
    ]] === $Failed &&
      Quiet[categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        1.
      ]] === $Failed &&
      Quiet[categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        Infinity
      ]] === $Failed &&
      Quiet[categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        {0.5}
      ]] === $Failed &&
      Quiet[categoricalReverseStep[2, 1, predictedX0]] === $Failed
  ];

  samplerSchedule = makeUniformCategoricalSchedule[
    3,
    {0.1, 0.2, 0.3, 0.4}
  ];
  samplerPredictor = Function[{state, time},
    N[
      (Range[3] + state + time)/
        Total[Range[3] + state + time]
    ]
  ];
  samplerUniforms = {0.15, 0.85, 0.35, 0.65};
  samplerExpected = Fold[
    Function[{state, time},
      categoricalReverseStep[
        state,
        time,
        samplerPredictor[state, time],
        samplerSchedule,
        samplerUniforms[[time]]
      ]
    ],
    3,
    Reverse[Range[samplerSchedule["Steps"]]]
  ];
  assert[
    "categorical sampler reproduces the direct T-to-zero reverse loop",
    categoricalSample[
      samplerPredictor,
      3,
      samplerSchedule,
      "Noises" -> samplerUniforms
    ] === samplerExpected
  ];
  validationTrace = Trace[
    categoricalSample[
      samplerPredictor,
      3,
      samplerSchedule,
      "Noises" -> samplerUniforms
    ],
    _categoricalScheduleQ,
    TraceInternal -> True
  ];
  assert[
    "validates the categorical schedule once before the sampler loop",
    Count[validationTrace, _categoricalScheduleQ, Infinity] === 1
  ];
  samplerResult = categoricalSample[
    samplerPredictor,
    3,
    samplerSchedule,
    "Noises" -> samplerUniforms,
    "ReturnTrajectory" -> True
  ];
  assert[
    "categorical sampler trajectory is ordered from xT through x0",
    Length[samplerResult["Trajectory"]] ===
        samplerSchedule["Steps"] + 1 &&
      samplerResult["Timesteps"] ===
        Append[Reverse[Range[samplerSchedule["Steps"]]], 0] &&
      Length[samplerResult["Timesteps"]] ===
        Length[samplerResult["Trajectory"]] &&
      First[samplerResult["Trajectory"]] === 3 &&
      Last[samplerResult["Trajectory"]] === samplerResult["Sample"] &&
      samplerResult["Sample"] === samplerExpected &&
      AllTrue[
        samplerResult["Trajectory"],
        IntegerQ[#] && 1 <= # <= samplerSchedule["CategoryCount"] &
      ]
  ];
  samplerTimeTrace = Reap[
    categoricalSample[
      Function[{state, time},
        Sow[time];
        samplerPredictor[state, time]
      ],
      3,
      samplerSchedule,
      "Noises" -> samplerUniforms
    ]
  ][[2, 1]];
  assert[
    "categorical sampler calls the predictor in descending time order",
    samplerTimeTrace === Reverse[Range[samplerSchedule["Steps"]]]
  ];
  samplerAutomatic1 = BlockRandom[
    SeedRandom[7193];
    categoricalSample[
      samplerPredictor,
      3,
      samplerSchedule,
      "Seed" -> Automatic,
      "ReturnTrajectory" -> True
    ]
  ];
  samplerAutomatic2 = BlockRandom[
    SeedRandom[7193];
    categoricalSample[
      samplerPredictor,
      3,
      samplerSchedule,
      "Seed" -> Automatic,
      "ReturnTrajectory" -> True
    ]
  ];
  assert[
    "resetting the caller seed reproduces categorical sampling",
    samplerAutomatic1 === samplerAutomatic2
  ];
  assert[
    "automatic categorical sampling consumes one uniform per reverse step",
    (expectedNext = BlockRandom[
      SeedRandom[7193];
      Table[RandomReal[], {samplerSchedule["Steps"]}];
      RandomReal[]
    ];
    actualNext = BlockRandom[
      SeedRandom[7193];
      categoricalSample[
        samplerPredictor,
        3,
        samplerSchedule,
        "Seed" -> Automatic
      ];
      RandomReal[]
    ];
    actualNext === expectedNext)
  ];
  samplerSeeded1 = categoricalSample[
    samplerPredictor,
    3,
    samplerSchedule,
    "Seed" -> 4816,
    "ReturnTrajectory" -> True
  ];
  samplerSeeded2 = categoricalSample[
    samplerPredictor,
    3,
    samplerSchedule,
    "Seed" -> 4816,
    "ReturnTrajectory" -> True
  ];
  assert[
    "integer seeds reproduce and isolate categorical sampling",
    samplerSeeded1 === samplerSeeded2 &&
      BlockRandom[
        SeedRandom[9021];
        categoricalSample[
          samplerPredictor,
          3,
          samplerSchedule,
          "Seed" -> 4816
        ];
        RandomReal[]
      ] === BlockRandom[SeedRandom[9021]; RandomReal[]]
  ];
  samplerSeedSchedule = makeUniformCategoricalSchedule[
    2,
    ConstantArray[1., 10]
  ];
  samplerDifferentSeed1 = categoricalSample[
    Function[{state, time}, {0.5, 0.5}],
    1,
    samplerSeedSchedule,
    "Seed" -> 1,
    "ReturnTrajectory" -> True
  ];
  samplerDifferentSeed2 = categoricalSample[
    Function[{state, time}, {0.5, 0.5}],
    1,
    samplerSeedSchedule,
    "Seed" -> 999,
    "ReturnTrajectory" -> True
  ];
  assert[
    "different integer seeds distinguish a non-degenerate categorical process",
    samplerDifferentSeed1 =!= samplerDifferentSeed2
  ];
  assert[
    "explicit categorical uniforms take precedence and preserve the RNG",
    categoricalSample[
      samplerPredictor,
      3,
      samplerSchedule,
      "Seed" -> 1,
      "Noises" -> samplerUniforms,
      "ReturnTrajectory" -> True
    ] === categoricalSample[
      samplerPredictor,
      3,
      samplerSchedule,
      "Seed" -> 999,
      "Noises" -> samplerUniforms,
      "ReturnTrajectory" -> True
    ] &&
      BlockRandom[
        SeedRandom[9021];
        categoricalSample[
          samplerPredictor,
          3,
          samplerSchedule,
          "Noises" -> samplerUniforms
        ];
        RandomReal[]
      ] === BlockRandom[SeedRandom[9021]; RandomReal[]]
  ];
  assert[
    "categorical sampler rejects invalid predictor probabilities",
    Quiet[categoricalSample[
      Function[{state, time}, {0.5, 0.5}],
      3,
      samplerSchedule,
      "Noises" -> samplerUniforms
    ]] === $Failed &&
      Quiet[categoricalSample[
        Function[{state, time}, {0.5, -0.1, 0.6}],
        3,
        samplerSchedule,
        "Noises" -> samplerUniforms
      ]] === $Failed &&
      Quiet[categoricalSample[
        Function[{state, time}, {0.2, 0.2, 0.2}],
        3,
        samplerSchedule,
        "Noises" -> samplerUniforms
      ]] === $Failed &&
      Quiet[categoricalSample[
        Function[{state, time}, {Infinity, 0., 0.}],
        3,
        samplerSchedule,
        "Noises" -> samplerUniforms
      ]] === $Failed &&
      Quiet[categoricalSample[
        Function[{state, time}, 1.],
        3,
        samplerSchedule,
        "Noises" -> samplerUniforms
      ]] === $Failed &&
      Quiet[categoricalSample[
        Function[{state, time}, $Failed],
        3,
        samplerSchedule,
        "Noises" -> samplerUniforms
      ]] === $Failed
  ];
  assert[
    "categorical sampler rejects invalid explicit uniforms",
    Quiet[categoricalSample[
      samplerPredictor,
      3,
      samplerSchedule,
      "Noises" -> Most[samplerUniforms]
    ]] === $Failed &&
      Quiet[categoricalSample[
        samplerPredictor,
        3,
        samplerSchedule,
        "Noises" -> {0.1, 0.2, 1., 0.4}
      ]] === $Failed &&
      Quiet[categoricalSample[
        samplerPredictor,
        3,
        samplerSchedule,
        "Noises" -> {0.1, 0.2, Infinity, 0.4}
      ]] === $Failed &&
      Quiet[categoricalSample[
        samplerPredictor,
        3,
        samplerSchedule,
        "Noises" -> {0.1, 0.2, Indeterminate, 0.4}
      ]] === $Failed &&
      Quiet[categoricalSample[
        samplerPredictor,
        3,
        samplerSchedule,
        "Noises" -> {0.1, 0.2, -0.1, 0.4}
      ]] === $Failed
  ];
  assert[
    "categorical sampler rejects invalid state schedule options and arity",
    Quiet[categoricalSample[
      samplerPredictor,
      0,
      samplerSchedule
    ]] === $Failed &&
      Quiet[categoricalSample[
        samplerPredictor,
        4,
        samplerSchedule
      ]] === $Failed &&
      Quiet[categoricalSample[
        samplerPredictor,
        3.,
        samplerSchedule
      ]] === $Failed &&
      Quiet[categoricalSample[
        samplerPredictor,
        3,
        KeyDrop[samplerSchedule, "TransitionKernels"]
      ]] === $Failed &&
      Quiet[categoricalSample[
        samplerPredictor,
        3,
        samplerSchedule,
        "Seed" -> 1.5
      ]] === $Failed &&
      Quiet[categoricalSample[
        samplerPredictor,
        3,
        samplerSchedule,
        "ReturnTrajectory" -> 1
      ]] === $Failed &&
      Quiet[categoricalSample[
        samplerPredictor,
        3,
        samplerSchedule,
        "Noises" -> 0.5
      ]] === $Failed &&
      Quiet[categoricalSample[
        samplerPredictor,
        3,
        samplerSchedule,
        "Unknown" -> True
      ]] === $Failed &&
      Quiet[categoricalSample[
        samplerPredictor,
        3,
        samplerSchedule,
        "Seed" -> 1,
        "Seed" -> 2
      ]] === $Failed &&
      Quiet[categoricalSample[
        samplerPredictor,
        3,
        samplerSchedule,
        "Noises" -> samplerUniforms,
        "Noises" -> samplerUniforms
      ]] === $Failed &&
      Quiet[categoricalSample[samplerPredictor, 3]] === $Failed
  ];
  assert[
    "categorical sampler propagates zero-support reverse failure",
    Quiet[categoricalSample[
      Function[{state, time}, {1., 0., 0.}],
      2,
      sparseSchedule,
      "Noises" -> {0.5, 0.5}
    ]] === $Failed
  ];

  kernel = uniformCategoricalTransitionKernel[3, 0.6];
  withinToleranceKernel = {
    {0.5, 0.5 - 10^-12},
    {0.25, 0.75}
  };
  overToleranceKernel = {
    {0.5, 0.5 - 2.*10^-12},
    {0.25, 0.75}
  };
  assert[
    "accepts row-stochastic errors within the numerical tolerance",
    Max[Abs[Total[withinToleranceKernel, {2}] - 1.]] <= 10^-12 &&
      categoricalTransitionKernelCategoryCount[withinToleranceKernel] === 2
  ];
  assert[
    "rejects row-stochastic errors beyond the numerical tolerance",
    Max[Abs[Total[overToleranceKernel, {2}] - 1.]] > 10^-12 &&
      categoricalTransitionKernelCategoryCount[overToleranceKernel] ===
        $Failed
  ];
  toleranceSamples = categoricalForwardDiffuse[
    {1, 1, 1},
    withinToleranceKernel,
    {0., 0.5, 1. - 10^-13}
  ];
  assert[
    "samples a tolerance-limit kernel with a stable last-category fallback",
    toleranceSamples === {1, 2, 2} &&
      AllTrue[toleranceSamples, IntegerQ[#] && 1 <= # <= 2 &]
  ];
  assert[
    "samples the transition row selected by each state",
    categoricalForwardDiffuse[{1, 2, 3}, kernel, {0.59, 0.19, 0.41}] ===
      {1, 1, 3}
  ];
  assert[
    "preserves scalar states and array shape",
    categoricalForwardDiffuse[2, IdentityMatrix[3], 0.25] === 2 &&
      Dimensions[categoricalForwardDiffuse[
        {{1, 2}, {3, 1}},
        kernel,
        {{0.1, 0.3}, {0.5, 0.9}}
      ]] === {2, 2} &&
      Dimensions[categoricalForwardDiffuse[
        {{{1, 2}, {3, 1}}, {{2, 3}, {1, 2}}},
        kernel,
        ConstantArray[0.5, {2, 2, 2}]
      ]] === {2, 2, 2}
  ];
  assert[
    "maps explicit quantiles through a fully uniform kernel",
    categoricalForwardDiffuse[
      {3, 1, 2},
      uniformCategoricalTransitionKernel[3, 1],
      {0., 0.34, 0.999}
    ] === {1, 2, 3}
  ];
  states = {{1, 2}, {3, 1}};
  randomSample = BlockRandom[
    SeedRandom[8128];
    categoricalForwardDiffuse[states, kernel]
  ];
  expectedNoise = BlockRandom[
    SeedRandom[8128];
    RandomReal[{0, 1}, Dimensions[states]]
  ];
  assert[
    "uses the caller's random stream",
    randomSample === categoricalForwardDiffuse[states, kernel, expectedNoise]
  ];
  assert[
    "matrix-based automatic sampling advances and replays the random stream",
    (expectedNext = BlockRandom[
      SeedRandom[4172];
      RandomReal[{0, 1}, Dimensions[states]];
      RandomReal[]
    ];
    actualNext = BlockRandom[
      SeedRandom[4172];
      categoricalForwardDiffuse[states, kernel];
      RandomReal[]
    ];
    replay1 = BlockRandom[
      SeedRandom[4172];
      categoricalForwardDiffuse[states, kernel]
    ];
    replay2 = BlockRandom[
      SeedRandom[4172];
      categoricalForwardDiffuse[states, kernel]
    ];
    actualNext === expectedNext && replay1 === replay2)
  ];
  assert[
    "matrix-based explicit noise does not consult the random stream",
    BlockRandom[
      SeedRandom[4172];
      categoricalForwardDiffuse[states, kernel, ConstantArray[0.5, {2, 2}]];
      RandomReal[]
    ] === BlockRandom[SeedRandom[4172]; RandomReal[]]
  ];
  assert[
    "rejects invalid categorical states",
    Quiet[categoricalForwardDiffuse[0, kernel, 0.5]] === $Failed &&
      Quiet[categoricalForwardDiffuse[4, kernel, 0.5]] === $Failed &&
      Quiet[categoricalForwardDiffuse[{1, 2.0}, kernel, {0.1, 0.2}]] ===
        $Failed &&
      Quiet[categoricalForwardDiffuse[{}, kernel, {}]] === $Failed
  ];
  assert[
    "rejects malformed transition kernels",
    Quiet[categoricalForwardDiffuse[1, {{1., 0.}, {0.2, 0.7}}, 0.5]] ===
      $Failed &&
      Quiet[categoricalForwardDiffuse[1, {{1., 0.}, {-0.1, 1.1}}, 0.5]] ===
        $Failed &&
      Quiet[categoricalForwardDiffuse[1, {{1., 0., 0.}, {0., 1., 0.}}, 0.5]] ===
        $Failed &&
      Quiet[categoricalForwardDiffuse[1, {{1., 0.}, {0., Infinity}}, 0.5]] ===
        $Failed
  ];
  assert[
    "rejects malformed explicit noise and arity",
    Quiet[categoricalForwardDiffuse[{1, 2}, kernel, 0.5]] === $Failed &&
      Quiet[categoricalForwardDiffuse[{1, 2}, kernel, {0.2}]] === $Failed &&
      Quiet[categoricalForwardDiffuse[1, kernel, -0.1]] === $Failed &&
      Quiet[categoricalForwardDiffuse[1, kernel, 1.]] === $Failed &&
      Quiet[categoricalForwardDiffuse[1]] === $Failed
  ];

  schedule = makeUniformCategoricalSchedule[3, {0.1, 0.2, 0.3}];
  x0 = {1, 2, 3};
  noise = {0.1, 0.5, 0.9};
  t = 2;
  assert[
    "time-indexed t = 0 returns the clean state exactly",
    categoricalForwardDiffuseAt[x0, 0, schedule] === x0 &&
      categoricalForwardDiffuseAt[x0, 0, schedule, noise] === x0
  ];
  assert[
    "time-indexed explicit sampling delegates through Qbar_t",
    categoricalForwardDiffuseAt[x0, t, schedule, noise] ===
      categoricalForwardDiffuse[
        x0,
        schedule["CumulativeTransitionKernels"][[t]],
        noise
      ]
  ];
  samples = {
    1,
    {1, 2, 3},
    {{1, 2}, {3, 1}},
    {{{1, 2}, {3, 1}}, {{2, 3}, {1, 2}}}
  };
  assert[
    "time-indexed sampling preserves scalar vector matrix and tensor shapes",
    AllTrue[
      samples,
      Function[sample,
        Dimensions[categoricalForwardDiffuseAt[
          sample,
          2,
          schedule,
          If[
            IntegerQ[sample],
            0.5,
            ConstantArray[0.5, Dimensions[sample]]
          ]
        ]] === Dimensions[sample]
      ]
    ]
  ];
  assert[
    "time-indexed automatic sampling advances and replays the random stream",
    (expectedNext = BlockRandom[
      SeedRandom[8314];
      RandomReal[{0, 1}, Dimensions[x0]];
      RandomReal[]
    ];
    actualNext = BlockRandom[
      SeedRandom[8314];
      categoricalForwardDiffuseAt[x0, t, schedule];
      RandomReal[]
    ];
    replay1 = BlockRandom[
      SeedRandom[8314];
      categoricalForwardDiffuseAt[x0, t, schedule]
    ];
    replay2 = BlockRandom[
      SeedRandom[8314];
      categoricalForwardDiffuseAt[x0, t, schedule]
    ];
    actualNext === expectedNext && replay1 === replay2)
  ];
  assert[
    "time-indexed explicit noise and t = 0 do not consult the random stream",
    BlockRandom[
      SeedRandom[8314];
      categoricalForwardDiffuseAt[x0, t, schedule, noise];
      categoricalForwardDiffuseAt[x0, 0, schedule];
      RandomReal[]
    ] === BlockRandom[SeedRandom[8314]; RandomReal[]]
  ];
  assert[
    "time-indexed sampling rejects invalid times",
    Quiet[categoricalForwardDiffuseAt[x0, -1, schedule, noise]] === $Failed &&
      Quiet[categoricalForwardDiffuseAt[x0, 4, schedule, noise]] === $Failed &&
      Quiet[categoricalForwardDiffuseAt[x0, 2., schedule, noise]] === $Failed
  ];
  assert[
    "time-indexed sampling rejects malformed schedules and states",
    Quiet[categoricalForwardDiffuseAt[
      x0,
      t,
      ReplacePart[schedule, "CategoryCount" -> 4],
      noise
    ]] === $Failed &&
      Quiet[categoricalForwardDiffuseAt[
        x0,
        t,
        KeyDrop[schedule, "CumulativeTransitionKernels"],
        noise
      ]] === $Failed &&
      Quiet[categoricalForwardDiffuseAt[{1, 4}, t, schedule, {0.1, 0.2}]] ===
        $Failed
  ];
  assert[
    "time-indexed explicit form validates noise at t = 0",
    Quiet[categoricalForwardDiffuseAt[x0, 0, schedule, {0.1}]] === $Failed &&
      Quiet[categoricalForwardDiffuseAt[x0, 0, schedule, {0.1, 0.5, 1.}]] ===
        $Failed
  ];

  Print["✓ categorical — ", passed, " tests passed"];
  passed
];

End[]
