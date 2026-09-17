Begin["Stochasma`Private`"]

runConditioningTests[] := Module[
  {
    passed = 0, assert, currentFunction = "makeConditionedPredictor",
    calls = 0, received, condition,
    conditionedPredictor, predictor, secondPredictor, sample, time, result,
    representations, failingPredictor, randomConditioned, boundRandom,
    directRandom, schedule, noises, expectedSample, actualSample,
    malformedPredictor, unconditionedPrediction, conditionedPrediction,
    guidedPrediction, expectedNext, actualNext, callLog,
    unconditionedPredictor, guidedPredictor, scaleZeroCalls,
    scaleOneCalls, baseConditionedPredictor, opaqueUnconditioned,
    opaqueConditioned, randomUnconditioned, randomConditionedPredictor,
    directGuidedRandom, wrappedGuidedRandom, conditionalCalls,
    unconditionalCalls, ddimNoises, actualDDIM, expectedDDIM,
    categoricalSchedule, categoricalNoises,
    categoricalConditionedPredictor, categoricalGuidedPredictor,
    actualCategorical, expectedCategorical
  },
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ conditioning/", currentFunction, ": ", label];
    Quit[1]
  ];

  sample = {0.5, -1., 1.5};
  time = 3;
  condition = <|"Scale" -> 2., "Metadata" -> {"opaque", 7}|>;
  conditionedPredictor = Function[{state, logicalTime, suppliedCondition},
    calls++;
    received = {state, logicalTime, suppliedCondition};
    suppliedCondition["Scale"] state + logicalTime
  ];
  predictor = makeConditionedPredictor[conditionedPredictor, condition];
  assert[
    "returns a standard two-argument predictor",
    Head[predictor] === Function
  ];
  result = predictor[sample, time];
  assert[
    "forwards sample time and conditioning unchanged",
    received === {sample, time, condition}
  ];
  assert[
    "evaluates the conditioned predictor exactly once per call",
    calls === 1
  ];
  assert[
    "returns the conditioned prediction unchanged",
    result === condition["Scale"] sample + time
  ];

  representations = {
    7,
    "label",
    {1, 2, 3},
    <|"Tokens" -> {4, 5}|>,
    HoldForm[symbolicCondition[x]]
  };
  assert[
    "accepts arbitrary conditioning representations",
    And @@ Map[
      Function[value,
        makeConditionedPredictor[
          Function[{state, logicalTime, suppliedCondition},
            suppliedCondition
          ],
          value
        ][sample, time] === value
      ],
      representations
    ]
  ];

  predictor = makeConditionedPredictor[
    Function[{state, logicalTime, suppliedCondition},
      suppliedCondition state
    ],
    2.
  ];
  secondPredictor = makeConditionedPredictor[
    Function[{state, logicalTime, suppliedCondition},
      suppliedCondition state
    ],
    3.
  ];
  assert[
    "independent closures retain independent conditioning values",
    predictor[sample, time] === 2. sample &&
      secondPredictor[sample, time] === 3. sample
  ];

  failingPredictor = makeConditionedPredictor[
    Function[{state, logicalTime, suppliedCondition}, $Failed],
    condition
  ];
  assert[
    "propagates a conditioned-predictor failure",
    failingPredictor[sample, time] === $Failed
  ];

  randomConditioned = Function[{state, logicalTime, suppliedCondition},
    state + suppliedCondition RandomReal[] + logicalTime
  ];
  boundRandom = BlockRandom[
    SeedRandom[24680];
    {
      makeConditionedPredictor[randomConditioned, 0.25][sample, time],
      RandomReal[]
    }
  ];
  directRandom = BlockRandom[
    SeedRandom[24680];
    {
      randomConditioned[sample, time, 0.25],
      RandomReal[]
    }
  ];
  assert[
    "does not alter conditioned-predictor random-stream consumption",
    boundRandom === directRandom
  ];

  schedule = makeDiffusionSchedule[linearBetaSchedule[3, 0.01, 0.03]];
  noises = ConstantArray[0. sample, schedule["Steps"]];
  predictor = makeConditionedPredictor[
    Function[{state, logicalTime, suppliedCondition},
      suppliedCondition state
    ],
    0.1
  ];
  actualSample = ddpmSample[
    predictor,
    sample,
    schedule,
    "Noises" -> noises
  ];
  expectedSample = ddpmSample[
    Function[{state, logicalTime}, 0.1 state],
    sample,
    schedule,
    "Noises" -> noises
  ];
  assert[
    "composes with the existing two-argument sampler protocol",
    actualSample === expectedSample
  ];

  malformedPredictor = makeConditionedPredictor[
    Function[{state, logicalTime, suppliedCondition}, suppliedCondition],
    {1., 2.}
  ];
  assert[
    "leaves malformed prediction rejection to the sampler boundary",
    Quiet[
      ddpmSample[
        malformedPredictor,
        sample,
        schedule,
        "Noises" -> noises
      ]
    ] === $Failed
  ];
  assert[
    "missing and extra positional arguments fail",
    Quiet[makeConditionedPredictor[]] === $Failed &&
      Quiet[
        makeConditionedPredictor[conditionedPredictor, condition, "extra"]
      ] === $Failed
  ];

  currentFunction = "classifierFreeGuidance";
  unconditionedPrediction = {1., -2., 0.5};
  conditionedPrediction = {3., 2., -0.5};
  guidedPrediction = classifierFreeGuidance[
    unconditionedPrediction,
    conditionedPrediction,
    2.
  ];
  assert[
    "implements the classifier-free guidance affine combination",
    Max[
      Abs[
        guidedPrediction -
          (unconditionedPrediction +
            2. (conditionedPrediction - unconditionedPrediction))
      ]
    ] < 10^-12
  ];
  assert[
    "scale zero returns the unconditioned prediction",
    classifierFreeGuidance[{1, -2}, {3, 2}, 0] === {1, -2}
  ];
  assert[
    "scale one returns the conditioned prediction",
    classifierFreeGuidance[{1, -2}, {3, 2}, 1] === {3, 2}
  ];
  assert[
    "supports scalar predictions",
    classifierFreeGuidance[2., 5., 0.5] === 3.5
  ];
  assert[
    "preserves matrix and tensor shapes",
    With[
      {
        matrixResult = classifierFreeGuidance[
          {{0., 1.}, {2., 3.}},
          {{1., 3.}, {5., 7.}},
          0.5
        ],
        tensorUnconditioned = ConstantArray[1., {2, 2, 2}],
        tensorConditioned = ConstantArray[3., {2, 2, 2}]
      },
      Dimensions[matrixResult] === {2, 2} &&
        Max[
          Abs[
            Flatten[
              matrixResult - {{0.5, 2.}, {3.5, 5.}}
            ]
          ]
        ] < 10^-12 &&
        Dimensions[
          classifierFreeGuidance[
            tensorUnconditioned,
            tensorConditioned,
            0.25
          ]
        ] === {2, 2, 2}
    ]
  ];
  assert[
    "identical predictions remain unchanged at extrapolating scales",
    classifierFreeGuidance[
      conditionedPrediction,
      conditionedPrediction,
      25.
    ] === conditionedPrediction
  ];

  expectedNext = BlockRandom[
    SeedRandom[13579];
    RandomReal[]
  ];
  actualNext = BlockRandom[
    SeedRandom[13579];
    classifierFreeGuidance[
      unconditionedPrediction,
      conditionedPrediction,
      1.5
    ];
    RandomReal[]
  ];
  assert[
    "does not consume the caller random stream",
    actualNext === expectedNext
  ];
  assert[
    "rejects mismatched prediction shapes",
    Quiet[classifierFreeGuidance[{1., 2.}, {1.}, 1.]] === $Failed &&
      Quiet[classifierFreeGuidance[1., {1.}, 1.]] === $Failed &&
      Quiet[
        classifierFreeGuidance[
          {{1., 2.}},
          {{1.}, {2.}},
          1.
        ]
      ] === $Failed
  ];
  assert[
    "rejects empty nonnumeric and non-finite predictions",
    Quiet[classifierFreeGuidance[{}, {}, 1.]] === $Failed &&
      Quiet[classifierFreeGuidance[{1., "x"}, {1., 2.}, 1.]] ===
        $Failed &&
      Quiet[classifierFreeGuidance[{1., Infinity}, {1., 2.}, 1.]] ===
        $Failed &&
      Quiet[classifierFreeGuidance[{1., I}, {1., 2.}, 1.]] === $Failed
  ];
  assert[
    "rejects negative non-scalar and non-finite guidance scales",
    Quiet[
      classifierFreeGuidance[
        unconditionedPrediction,
        conditionedPrediction,
        -0.1
      ]
    ] === $Failed &&
    Quiet[
      classifierFreeGuidance[
        unconditionedPrediction,
        conditionedPrediction,
        {1.}
      ]
    ] === $Failed &&
      Quiet[
        classifierFreeGuidance[
          unconditionedPrediction,
          conditionedPrediction,
          Infinity
        ]
      ] === $Failed &&
      Quiet[
        classifierFreeGuidance[
          unconditionedPrediction,
          conditionedPrediction,
          I
        ]
      ] === $Failed
  ];
  assert[
    "missing and extra positional arguments fail",
    Quiet[classifierFreeGuidance[]] === $Failed &&
      Quiet[
        classifierFreeGuidance[
          unconditionedPrediction,
          conditionedPrediction
        ]
      ] === $Failed &&
      Quiet[
        classifierFreeGuidance[
          unconditionedPrediction,
          conditionedPrediction,
          1.,
          "extra"
        ]
      ] === $Failed
  ];

  currentFunction = "makeClassifierFreeGuidedPredictor";
  callLog = {};
  unconditionedPredictor = Function[{state, logicalTime},
    AppendTo[callLog, {"Unconditioned", state, logicalTime}];
    0. state
  ];
  conditionedPredictor = Function[{state, logicalTime},
    AppendTo[callLog, {"Conditioned", state, logicalTime}];
    0.5 state
  ];
  guidedPredictor = makeClassifierFreeGuidedPredictor[
    unconditionedPredictor,
    conditionedPredictor,
    2.
  ];
  assert[
    "returns a standard two-argument predictor",
    Head[guidedPredictor] === Function
  ];
  guidedPrediction = guidedPredictor[sample, time];
  assert[
    "evaluates unconditioned then conditioned with unchanged arguments",
    callLog === {
      {"Unconditioned", sample, time},
      {"Conditioned", sample, time}
    }
  ];
  assert[
    "evaluates each predictor exactly once",
    Count[callLog, {"Unconditioned", _, _}] === 1 &&
      Count[callLog, {"Conditioned", _, _}] === 1
  ];
  assert[
    "returns the classifier-free guided prediction",
    guidedPrediction === sample
  ];

  scaleZeroCalls = {0, 0};
  guidedPredictor = makeClassifierFreeGuidedPredictor[
    Function[{state, logicalTime}, scaleZeroCalls[[1]]++; 2. state],
    Function[{state, logicalTime}, scaleZeroCalls[[2]]++; 3. state],
    0.
  ];
  guidedPrediction = guidedPredictor[sample, time];
  scaleOneCalls = {0, 0};
  predictor = makeClassifierFreeGuidedPredictor[
    Function[{state, logicalTime}, scaleOneCalls[[1]]++; 2. state],
    Function[{state, logicalTime}, scaleOneCalls[[2]]++; 3. state],
    1.
  ];
  assert[
    "evaluates both branches at boundary scales zero and one",
    guidedPrediction === 2. sample &&
      predictor[sample, time] === 3. sample &&
      scaleZeroCalls === {1, 1} &&
      scaleOneCalls === {1, 1}
  ];

  baseConditionedPredictor =
    Function[{state, logicalTime, suppliedConditioning},
      suppliedConditioning["Scale"] state +
        suppliedConditioning["Bias"] logicalTime
    ];
  opaqueUnconditioned = makeConditionedPredictor[
    baseConditionedPredictor,
    <|"Scale" -> 0., "Bias" -> 0.|>
  ];
  opaqueConditioned = makeConditionedPredictor[
    baseConditionedPredictor,
    <|"Scale" -> 0.25, "Bias" -> 0.1|>
  ];
  guidedPredictor = makeClassifierFreeGuidedPredictor[
    opaqueUnconditioned,
    opaqueConditioned,
    2.
  ];
  assert[
    "composes independently bound opaque conditioning values",
    guidedPredictor[sample, time] === 0.5 sample + 0.2 time
  ];

  randomUnconditioned = Function[{state, logicalTime},
    state RandomReal[] + logicalTime
  ];
  randomConditionedPredictor = Function[{state, logicalTime},
    state RandomReal[] - logicalTime
  ];
  wrappedGuidedRandom = BlockRandom[
    SeedRandom[86420];
    {
      makeClassifierFreeGuidedPredictor[
        randomUnconditioned,
        randomConditionedPredictor,
        1.5
      ][sample, time],
      RandomReal[]
    }
  ];
  directGuidedRandom = BlockRandom[
    SeedRandom[86420];
    {
      classifierFreeGuidance[
        randomUnconditioned[sample, time],
        randomConditionedPredictor[sample, time],
        1.5
      ],
      RandomReal[]
    }
  ];
  assert[
    "preserves sequential branch random-stream consumption",
    wrappedGuidedRandom === directGuidedRandom
  ];

  conditionalCalls = 0;
  guidedPredictor = makeClassifierFreeGuidedPredictor[
    Function[{state, logicalTime}, $Failed],
    Function[{state, logicalTime}, conditionalCalls++; 0. state],
    1.
  ];
  assert[
    "short-circuits when the unconditioned predictor fails",
    guidedPredictor[sample, time] === $Failed && conditionalCalls === 0
  ];
  unconditionalCalls = 0;
  conditionalCalls = 0;
  guidedPredictor = makeClassifierFreeGuidedPredictor[
    Function[{state, logicalTime}, unconditionalCalls++; 0. state],
    Function[{state, logicalTime}, conditionalCalls++; $Failed],
    1.
  ];
  assert[
    "propagates a conditioned predictor failure after one call per branch",
    guidedPredictor[sample, time] === $Failed &&
      unconditionalCalls === 1 && conditionalCalls === 1
  ];
  guidedPredictor = makeClassifierFreeGuidedPredictor[
    Function[{state, logicalTime}, {0., 0.}],
    Function[{state, logicalTime}, {0., 0., 0.}],
    1.
  ];
  assert[
    "rejects branch predictions with mismatched shapes",
    Quiet[guidedPredictor[sample, time]] === $Failed
  ];

  guidedPredictor = makeClassifierFreeGuidedPredictor[
    Function[{state, logicalTime}, 0. state],
    Function[{state, logicalTime}, 0.1 state],
    2.
  ];
  actualSample = ddpmSample[
    guidedPredictor,
    sample,
    schedule,
    "Noises" -> noises
  ];
  expectedSample = ddpmSample[
    Function[{state, logicalTime}, 0.2 state],
    sample,
    schedule,
    "Noises" -> noises
  ];
  assert[
    "composes with DDPM without changing sampler semantics",
    actualSample === expectedSample
  ];
  guidedPredictor = makeClassifierFreeGuidedPredictor[
    Function[{state, logicalTime}, {0., 0.}],
    Function[{state, logicalTime}, {1., 1.}],
    1.
  ];
  assert[
    "leaves sample-shape validation to the sampler boundary",
    Quiet[
      ddpmSample[
        guidedPredictor,
        sample,
        schedule,
        "Noises" -> noises
      ]
    ] === $Failed
  ];
  assert[
    "rejects invalid guidance scales at construction",
    Quiet[
      makeClassifierFreeGuidedPredictor[
        unconditionedPredictor,
        conditionedPredictor,
        -0.1
      ]
    ] === $Failed &&
      Quiet[
        makeClassifierFreeGuidedPredictor[
          unconditionedPredictor,
          conditionedPredictor,
          Infinity
        ]
      ] === $Failed &&
      Quiet[
        makeClassifierFreeGuidedPredictor[
          unconditionedPredictor,
          conditionedPredictor,
          I
        ]
      ] === $Failed
  ];
  assert[
    "missing and extra positional arguments fail",
    Quiet[makeClassifierFreeGuidedPredictor[]] === $Failed &&
      Quiet[
        makeClassifierFreeGuidedPredictor[
          unconditionedPredictor,
          conditionedPredictor
        ]
      ] === $Failed &&
      Quiet[
        makeClassifierFreeGuidedPredictor[
          unconditionedPredictor,
          conditionedPredictor,
          1.,
          "extra"
        ]
      ] === $Failed
  ];

  currentFunction = "regression";
  baseConditionedPredictor =
    Function[{state, logicalTime, suppliedConditioning},
      suppliedConditioning state
    ];
  guidedPredictor = makeClassifierFreeGuidedPredictor[
    makeConditionedPredictor[baseConditionedPredictor, 0.],
    makeConditionedPredictor[baseConditionedPredictor, 0.1],
    2.
  ];
  ddimNoises = ConstantArray[0. sample, 2];
  actualDDIM = ddimSample[
    guidedPredictor,
    sample,
    schedule,
    "Eta" -> 0.,
    "Timesteps" -> {3, 1},
    "Noises" -> ddimNoises
  ];
  expectedDDIM = ddimSample[
    Function[{state, logicalTime}, 0.2 state],
    sample,
    schedule,
    "Eta" -> 0.,
    "Timesteps" -> {3, 1},
    "Noises" -> ddimNoises
  ];
  assert[
    "condition binding and guidance preserve DDIM sampler semantics",
    Max[Abs[actualDDIM - expectedDDIM]] < 10^-12
  ];

  categoricalSchedule = makeUniformCategoricalSchedule[
    3,
    {0.1, 0.2}
  ];
  categoricalNoises = {0.2, 0.1};
  categoricalConditionedPredictor =
    Function[{state, logicalTime, suppliedConditioning},
      suppliedConditioning
    ];
  categoricalGuidedPredictor = makeClassifierFreeGuidedPredictor[
    makeConditionedPredictor[
      categoricalConditionedPredictor,
      {0.2, 0.3, 0.5}
    ],
    makeConditionedPredictor[
      categoricalConditionedPredictor,
      {0.6, 0.3, 0.1}
    ],
    0.5
  ];
  actualCategorical = categoricalSample[
    categoricalGuidedPredictor,
    2,
    categoricalSchedule,
    "Noises" -> categoricalNoises,
    "ReturnTrajectory" -> True
  ];
  expectedCategorical = categoricalSample[
    Function[{state, logicalTime}, {0.4, 0.3, 0.3}],
    2,
    categoricalSchedule,
    "Noises" -> categoricalNoises,
    "ReturnTrajectory" -> True
  ];
  assert[
    "generic guidance composes with categorical probability consumers",
    actualCategorical === expectedCategorical &&
      expectedCategorical["Trajectory"] === {2, 2, 1}
  ];

  Print["✓ conditioning — ", passed, " tests passed"];
  passed
];

End[]
