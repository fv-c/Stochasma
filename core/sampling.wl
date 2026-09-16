If[
  DownValues[Stochasma`reverseDiffuseStep] === {},
  Get[FileNameJoin[{DirectoryName[$InputFileName], "reverse.wl"}]]
];

BeginPackage["Stochasma`"]

ddpmSample::usage =
  "ddpmSample[predictor, initialNoise, schedule, opts] applies predictor[xt, t] from t = T down to 1 and returns x0. \"ReturnTrajectory\" -> True returns <|\"Sample\" -> x0, \"Trajectory\" -> {xT, ..., x0}|>. \"Seed\" accepts Automatic or an Integer. \"Noises\" accepts Automatic or a length-T list indexed by logical t; each entry has the sample shape and the t = 1 entry is validated but not added.";

Begin["`Private`"]

Options[ddpmSample] = {
  "ReturnTrajectory" -> False,
  "Seed" -> Automatic,
  "Noises" -> Automatic
};

ddpmSample::sample =
  "initialNoise must be a finite real numeric scalar or non-empty array.";
ddpmSample::schedule =
  "The supplied diffusion schedule is malformed or internally inconsistent.";
ddpmSample::predictor =
  "The predictor must return a finite real epsilon prediction with the same shape as xt at every time.";
ddpmSample::opts =
  "Options must use each of \"ReturnTrajectory\", \"Seed\", and \"Noises\" at most once with valid values.";
ddpmSample::noises =
  "Explicit \"Noises\" must be a length-T list whose entries have the same finite real shape as initialNoise.";
ddpmSample::args =
  "ddpmSample expects a predictor callable, initial noise, a diffusion schedule, and optional rules.";

runDDPMSampling[
  predictor_,
  initialNoise_,
  schedule_Association,
  returnTrajectory_,
  noises_
] := Module[{state, trajectory, predictedNoise, stepNoise, t, failed},
  state = initialNoise;
  trajectory = {initialNoise};
  failed = False;
  Do[
    predictedNoise = Check[predictor[state, t], $Failed];
    If[
      predictedNoise === $Failed ||
        !sameSampleShapeQ[state, predictedNoise],
      Message[ddpmSample::predictor];
      failed = True;
      Break[]
    ];
    stepNoise = Which[
      ListQ[noises], noises[[t]],
      t == 1, 0. state,
      True, randomNormalLike[state]
    ];
    state = reverseDiffuseStep[
      state,
      t,
      predictedNoise,
      schedule,
      stepNoise
    ];
    If[state === $Failed,
      failed = True;
      Break[]
    ];
    trajectory = Append[trajectory, state],
    {t, schedule["Steps"], 1, -1}
  ];
  If[failed, Return[$Failed]];
  If[
    TrueQ[returnTrajectory],
    <|"Sample" -> state, "Trajectory" -> trajectory|>,
    state
  ]
];

ddpmSample[
  predictor_,
  initialNoise_,
  schedule_Association,
  opts___
] := Module[
  {given, keys, values, returnTrajectory, seed, noises},
  given = {opts};
  If[!OptionQ[given],
    Message[ddpmSample::opts];
    Return[$Failed]
  ];
  keys = First /@ given;
  If[
    !AllTrue[
      keys,
      MemberQ[{"ReturnTrajectory", "Seed", "Noises"}, #] &
    ] || DuplicateFreeQ[keys] === False,
    Message[ddpmSample::opts];
    Return[$Failed]
  ];
  values = Join[Association[Options[ddpmSample]], Association[given]];
  returnTrajectory = values["ReturnTrajectory"];
  seed = values["Seed"];
  noises = values["Noises"];
  If[!MemberQ[{True, False}, returnTrajectory] ||
      !(seed === Automatic || IntegerQ[seed]),
    Message[ddpmSample::opts];
    Return[$Failed]
  ];
  If[!realNumericSampleQ[initialNoise],
    Message[ddpmSample::sample];
    Return[$Failed]
  ];
  If[!diffusionScheduleQ[schedule],
    Message[ddpmSample::schedule];
    Return[$Failed]
  ];
  If[
    noises =!= Automatic &&
      (!ListQ[noises] || Length[noises] =!= schedule["Steps"] ||
        !AllTrue[noises, sameSampleShapeQ[initialNoise, #] &]),
    Message[ddpmSample::noises];
    Return[$Failed]
  ];
  If[
    ListQ[noises],
    runDDPMSampling[
      predictor, initialNoise, schedule, returnTrajectory, noises
    ],
    If[
      seed === Automatic,
      BlockRandom[
        runDDPMSampling[
          predictor, initialNoise, schedule, returnTrajectory, Automatic
        ]
      ],
      BlockRandom[
        SeedRandom[seed];
        runDDPMSampling[
          predictor, initialNoise, schedule, returnTrajectory, Automatic
        ]
      ]
    ]
  ]
];

ddpmSample[___] := (
  Message[ddpmSample::args];
  $Failed
);

(* === TESTS === *)

runSamplingTests[] := Module[
  {passed = 0, assert, schedule, initialNoise, predictor, stepNoises,
    expected, sample, result, seeded1, seeded2, samples},
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
  seeded1 = ddpmSample[predictor, initialNoise, schedule, "Seed" -> 13579];
  seeded2 = ddpmSample[predictor, initialNoise, schedule, "Seed" -> 13579];
  assert[
    "identical seeds produce identical samples",
    seeded1 === seeded2
  ];
  assert[
    "explicit noises make the seed irrelevant",
    ddpmSample[
      predictor, initialNoise, schedule,
      "Seed" -> 1, "Noises" -> stepNoises
    ] === ddpmSample[
      predictor, initialNoise, schedule,
      "Seed" -> 2, "Noises" -> stepNoises
    ]
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
    "predictor output shape is enforced",
    Quiet[
      ddpmSample[
        Function[{xt, time}, {0.}],
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
      ] === $Failed
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
          ReplacePart[schedule, "Steps" -> 5]
        ]
      ] === $Failed
  ];

  Print["✓ sampling — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "sampling.wl"],
  Stochasma`Private`runSamplingTests[]
]
