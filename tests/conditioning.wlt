Begin["Stochasma`Private`"]

runConditioningTests[] := Module[
  {
    passed = 0, assert, calls = 0, received, condition,
    conditionedPredictor, predictor, secondPredictor, sample, time, result,
    representations, failingPredictor, randomConditioned, boundRandom,
    directRandom, schedule, noises, expectedSample, actualSample,
    malformedPredictor
  },
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ conditioning/makeConditionedPredictor: ", label];
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

  Print["✓ conditioning — ", passed, " tests passed"];
  passed
];

End[]
