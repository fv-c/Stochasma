Begin["Stochasma`Private`"]

runSamplingTests[] := Module[
  {passed = 0, assert, schedule, initialNoise, predictor, stepNoises,
    expected, sample, result, timeTrace, seeded1, seeded2, differentSeed,
    stochasticInitial, automatic1, automatic2, replay1, replay2,
    expectedNext, actualNext, samples, validationTrace},
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ sampling/ddpmSample: ", label];
    Quit[1]
  ];

  schedule = makeDiffusionSchedule[linearBetaSchedule[6, 0.01, 0.06]];
  initialNoise = {0.5, -1., 1.5};
  predictor = Function[{xt, time}, 0. xt + 0.01 time];
  stepNoises = Table[ConstantArray[N[time]/10., 3], {time, 6}];
  expected = Fold[
    Function[{state, time},
      reverseDiffuseStep[
        state,
        time,
        predictor[state, time],
        schedule,
        stepNoises[[time]]
      ]
    ],
    initialNoise,
    Reverse[Range[6]]
  ];
  sample = ddpmSample[
    predictor,
    initialNoise,
    schedule,
    "Noises" -> stepNoises
  ];
  assert[
    "explicit noises reproduce the direct T-to-zero reverse loop",
    numericSamplesCloseQ[sample, expected]
  ];
  validationTrace = Trace[
    ddpmSample[
      predictor,
      initialNoise,
      schedule,
      "Noises" -> stepNoises
    ],
    _diffusionScheduleQ,
    TraceInternal -> True
  ];
  assert[
    "validates the diffusion schedule once before the DDPM loop",
    Count[validationTrace, _diffusionScheduleQ, Infinity] === 1
  ];
  result = ddpmSample[
    predictor,
    initialNoise,
    schedule,
    "Noises" -> stepNoises,
    "ReturnTrajectory" -> True
  ];
  assert[
    "trajectory is ordered from xT through x0",
    Length[result["Trajectory"]] === 7 &&
      First[result["Trajectory"]] === initialNoise &&
      Last[result["Trajectory"]] === result["Sample"] &&
      numericSamplesCloseQ[result["Sample"], sample]
  ];
  assert[
    "every trajectory state preserves sample shape",
    AllTrue[
      result["Trajectory"],
      Dimensions[#] === Dimensions[initialNoise] &
    ]
  ];
  timeTrace = Reap[
    ddpmSample[
      Function[{xt, time}, Sow[time]; 0. xt],
      initialNoise,
      schedule,
      "Noises" -> stepNoises
    ]
  ][[2, 1]];
  assert[
    "predictor is called in descending logical-time order",
    timeTrace === Reverse[Range[schedule["Steps"]]]
  ];
  stochasticInitial = ConstantArray[0., 64];
  {automatic1, automatic2} = BlockRandom[
    SeedRandom[13579];
    {
      ddpmSample[
        Function[{xt, time}, 0. xt],
        stochasticInitial,
        schedule,
        "Seed" -> Automatic
      ],
      ddpmSample[
        Function[{xt, time}, 0. xt],
        stochasticInitial,
        schedule,
        "Seed" -> Automatic
      ]
    }
  ];
  assert[
    "consecutive automatic samplers consume the current random stream",
    automatic1 =!= automatic2
  ];
  replay1 = BlockRandom[
    SeedRandom[13579];
    ddpmSample[
      Function[{xt, time}, 0. xt],
      stochasticInitial,
      schedule,
      "Seed" -> Automatic
    ]
  ];
  replay2 = BlockRandom[
    SeedRandom[13579];
    ddpmSample[
      Function[{xt, time}, 0. xt],
      stochasticInitial,
      schedule,
      "Seed" -> Automatic
    ]
  ];
  assert[
    "resetting the caller seed reproduces automatic sampling",
    replay1 === replay2
  ];
  expectedNext = BlockRandom[
    SeedRandom[13579];
    Table[
      randomNormalLike[stochasticInitial],
      {schedule["Steps"] - 1}
    ];
    RandomReal[]
  ];
  actualNext = BlockRandom[
    SeedRandom[13579];
    ddpmSample[
      Function[{xt, time}, 0. xt],
      stochasticInitial,
      schedule,
      "Seed" -> Automatic
    ];
    RandomReal[]
  ];
  assert[
    "automatic sampling advances the stream once for each noisy reverse step",
    actualNext === expectedNext
  ];
  seeded1 = ddpmSample[
    Function[{xt, time}, 0. xt],
    stochasticInitial,
    schedule,
    "Seed" -> 13579
  ];
  seeded2 = ddpmSample[
    Function[{xt, time}, 0. xt],
    stochasticInitial,
    schedule,
    "Seed" -> 13579
  ];
  assert[
    "identical seeds produce identical samples",
    seeded1 === seeded2
  ];
  assert[
    "integer-seeded sampling is isolated from the caller random stream",
    BlockRandom[
      SeedRandom[86420];
      ddpmSample[
        Function[{xt, time}, 0. xt],
        stochasticInitial,
        schedule,
        "Seed" -> 13579
      ];
      RandomReal[]
    ] === BlockRandom[SeedRandom[86420]; RandomReal[]]
  ];
  differentSeed = ddpmSample[
    Function[{xt, time}, 0. xt],
    stochasticInitial,
    schedule,
    "Seed" -> 24680
  ];
  assert[
    "different seeds produce different stochastic samples",
    seeded1 =!= differentSeed
  ];
  assert[
    "explicit noises make the seed irrelevant",
    ddpmSample[
      predictor, initialNoise, schedule,
      "Seed" -> 1, "Noises" -> stepNoises
    ] === ddpmSample[
      predictor, initialNoise, schedule,
      "Seed" -> 999, "Noises" -> stepNoises
    ]
  ];
  assert[
    "explicit noises do not consult the current random stream",
    BlockRandom[
      SeedRandom[97531];
      ddpmSample[
        predictor,
        initialNoise,
        schedule,
        "Seed" -> 1,
        "Noises" -> stepNoises
      ];
      RandomReal[]
    ] === BlockRandom[SeedRandom[97531]; RandomReal[]]
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
        ddpmSample[
          Function[{xt, time}, 0. xt],
          #,
          schedule,
          "Noises" -> ConstantArray[0. #, schedule["Steps"]]
        ]
      ] === Dimensions[#] &
    ]
  ];
  assert[
    "predictor output shape and finiteness are enforced",
    Quiet[
      ddpmSample[
        Function[{xt, time}, {0.}],
        initialNoise,
        schedule,
        "Noises" -> stepNoises
      ]
    ] === $Failed &&
      Quiet[
        ddpmSample[
          Function[{xt, time}, {0., Infinity, 0.}],
          initialNoise,
          schedule,
          "Noises" -> stepNoises
        ]
      ] === $Failed
  ];
  assert[
    "invalid explicit noise sequences fail",
    Quiet[
      ddpmSample[predictor, initialNoise, schedule, "Noises" -> Most[stepNoises]]
    ] === $Failed &&
      Quiet[
        ddpmSample[
          predictor,
          initialNoise,
          schedule,
          "Noises" -> ConstantArray[{0.}, schedule["Steps"]]
        ]
      ] === $Failed &&
      Quiet[
        ddpmSample[
          predictor,
          initialNoise,
          schedule,
          "Noises" -> ReplacePart[
            stepNoises,
            2 -> {0., Indeterminate, 0.}
          ]
        ]
      ] === $Failed
  ];
  assert[
    "non-numeric and non-finite initial samples fail",
    Quiet[ddpmSample[predictor, symbolic, schedule]] === $Failed &&
      Quiet[ddpmSample[predictor, {0., Infinity, 0.}, schedule]] === $Failed
  ];
  assert[
    "invalid options and malformed schedules fail",
    Quiet[
      ddpmSample[predictor, initialNoise, schedule, "Seed" -> 1.5]
    ] === $Failed &&
      Quiet[
      ddpmSample[predictor, initialNoise, schedule, "Unknown" -> True]
    ] === $Failed &&
      Quiet[
        ddpmSample[
          predictor,
          initialNoise,
          schedule,
          "Seed" -> 1,
          "Seed" -> 2
        ]
      ] === $Failed &&
      Quiet[
        ddpmSample[
          predictor,
          initialNoise,
          ReplacePart[schedule, "Steps" -> 5]
        ]
      ] === $Failed
  ];

  Print["✓ sampling — ", passed, " tests passed"];
  passed
];

End[]
