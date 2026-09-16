If[
  DownValues[Stochasma`predictCleanSample] === {},
  Get[FileNameJoin[{DirectoryName[$InputFileName], "reverse.wl"}]]
];

BeginPackage["Stochasma`"]

ddimSample::usage =
  "ddimSample[predictor, initialNoise, schedule, opts] applies predictor[xt, t] at each requested logical time and returns x0. \"Timesteps\" is Automatic or a non-empty strictly decreasing list in 1..T; \"ReturnTrajectory\" -> True returns <|\"Sample\" -> x0, \"Trajectory\" -> {xStart, ..., x0}, \"Timesteps\" -> {tStart, ..., 0}|>. \"Eta\" -> 0 is deterministic. For positive eta, Automatic noises use the current stream or a locally isolated Integer \"Seed\". Explicit \"Noises\" has the same length and order as \"Timesteps\"; its final entry is validated but ignored because the transition to time 0 adds no noise.";

Begin["`Private`"]

Options[ddimSample] = {
  "Eta" -> 0.,
  "Timesteps" -> Automatic,
  "ReturnTrajectory" -> False,
  "Seed" -> Automatic,
  "Noises" -> Automatic
};

ddimSample::sample =
  "initialNoise must be a finite real numeric scalar or non-empty array.";
ddimSample::schedule =
  "The supplied diffusion schedule is malformed or internally inconsistent.";
ddimSample::predictor =
  "The predictor must return a finite real epsilon prediction with the same shape as xt at every requested time.";
ddimSample::eta =
  "\"Eta\" must be a finite non-negative real number whose DDIM direction variances are non-negative for the requested transitions.";
ddimSample::timesteps =
  "\"Timesteps\" must be Automatic or a non-empty strictly decreasing list of distinct Integers in the inclusive range 1 through `1`.";
ddimSample::opts =
  "Options must use each of \"Eta\", \"Timesteps\", \"ReturnTrajectory\", \"Seed\", and \"Noises\" at most once with valid values.";
ddimSample::noises =
  "Explicit \"Noises\" must match the resolved timestep-list length and contain entries with the same finite real shape as initialNoise.";
ddimSample::args =
  "ddimSample expects a predictor callable, initial noise, a diffusion schedule, and optional rules.";

ddimDirectionVariance[
  t_Integer,
  previousTime_Integer,
  schedule_Association,
  eta_
] := Module[{alphaBarT, alphaBarPrevious, sigmaSquared},
  alphaBarT = schedule["AlphaBars"][[t]];
  alphaBarPrevious = If[
    previousTime == 0,
    1.,
    schedule["AlphaBars"][[previousTime]]
  ];
  sigmaSquared = eta^2
    (1 - alphaBarPrevious)/(1 - alphaBarT)
    (1 - alphaBarT/alphaBarPrevious);
  1 - alphaBarPrevious - sigmaSquared
];

ddimEtaCompatibleQ[eta_, timesteps_List, schedule_Association] := Module[
  {previousTimes, directionVariances},
  previousTimes = Append[Rest[timesteps], 0];
  directionVariances = MapThread[
    ddimDirectionVariance[#1, #2, schedule, eta] &,
    {timesteps, previousTimes}
  ];
  AllTrue[
    directionVariances,
    finiteRealNumberQ[#] && TrueQ[# >= 0] &
  ]
];

ddimStep[
  xt_,
  t_Integer,
  previousTime_Integer,
  predictedNoise_,
  schedule_Association,
  eta_,
  noise_
] := Module[
  {predictedX0, alphaBarT, alphaBarPrevious, sigma,
    directionVariance},
  predictedX0 = predictCleanSample[xt, t, predictedNoise, schedule];
  If[predictedX0 === $Failed, Return[$Failed]];
  alphaBarT = schedule["AlphaBars"][[t]];
  alphaBarPrevious = If[
    previousTime == 0,
    1.,
    schedule["AlphaBars"][[previousTime]]
  ];
  sigma = eta Sqrt[
    (1 - alphaBarPrevious)/(1 - alphaBarT)
      (1 - alphaBarT/alphaBarPrevious)
  ];
  directionVariance = ddimDirectionVariance[
    t,
    previousTime,
    schedule,
    eta
  ];
  Sqrt[alphaBarPrevious] predictedX0 +
    Sqrt[directionVariance] predictedNoise +
    sigma noise
];

runDDIMSampling[
  predictor_,
  initialNoise_,
  schedule_Association,
  eta_,
  timesteps_List,
  returnTrajectory_,
  noises_
] := Module[
  {state, trajectory, previousTimes, predictedNoise, stepNoise,
    index, t, previousTime, failed},
  state = initialNoise;
  trajectory = {initialNoise};
  previousTimes = Append[Rest[timesteps], 0];
  failed = False;
  Do[
    t = timesteps[[index]];
    previousTime = previousTimes[[index]];
    predictedNoise = Check[predictor[state, t], $Failed];
    If[
      predictedNoise === $Failed ||
        !sameSampleShapeQ[state, predictedNoise],
      Message[ddimSample::predictor];
      failed = True;
      Break[]
    ];
    stepNoise = Which[
      ListQ[noises], noises[[index]],
      TrueQ[eta == 0] || previousTime == 0, 0. state,
      True, randomNormalLike[state]
    ];
    state = ddimStep[
      state,
      t,
      previousTime,
      predictedNoise,
      schedule,
      eta,
      stepNoise
    ];
    If[state === $Failed,
      failed = True;
      Break[]
    ];
    trajectory = Append[trajectory, state],
    {index, Length[timesteps]}
  ];
  If[failed, Return[$Failed]];
  If[
    TrueQ[returnTrajectory],
    <|
      "Sample" -> state,
      "Trajectory" -> trajectory,
      "Timesteps" -> Append[timesteps, 0]
    |>,
    state
  ]
];

ddimSample[
  predictor_,
  initialNoise_,
  schedule_Association,
  opts___
] := Module[
  {given, keys, values, eta, timesteps, returnTrajectory, seed, noises,
    resolvedTimesteps, run},
  given = {opts};
  If[!OptionQ[given],
    Message[ddimSample::opts];
    Return[$Failed]
  ];
  keys = First /@ given;
  If[
    !AllTrue[
      keys,
      MemberQ[
        {"Eta", "Timesteps", "ReturnTrajectory", "Seed", "Noises"},
        #
      ] &
    ] || DuplicateFreeQ[keys] === False,
    Message[ddimSample::opts];
    Return[$Failed]
  ];
  values = Join[Association[Options[ddimSample]], Association[given]];
  eta = values["Eta"];
  timesteps = values["Timesteps"];
  returnTrajectory = values["ReturnTrajectory"];
  seed = values["Seed"];
  noises = values["Noises"];
  If[
    !MemberQ[{True, False}, returnTrajectory] ||
      !(seed === Automatic || IntegerQ[seed]),
    Message[ddimSample::opts];
    Return[$Failed]
  ];
  If[!finiteRealNumberQ[eta] || !TrueQ[eta >= 0],
    Message[ddimSample::eta];
    Return[$Failed]
  ];
  If[!realNumericSampleQ[initialNoise],
    Message[ddimSample::sample];
    Return[$Failed]
  ];
  If[!diffusionScheduleQ[schedule],
    Message[ddimSample::schedule];
    Return[$Failed]
  ];
  resolvedTimesteps = If[
    timesteps === Automatic,
    Reverse[Range[schedule["Steps"]]],
    timesteps
  ];
  If[
    !ListQ[resolvedTimesteps] || resolvedTimesteps === {} ||
      !AllTrue[
        resolvedTimesteps,
        IntegerQ[#] && TrueQ[1 <= # <= schedule["Steps"]] &
      ] ||
      !AllTrue[Differences[resolvedTimesteps], TrueQ[# < 0] &],
    Message[ddimSample::timesteps, schedule["Steps"]];
    Return[$Failed]
  ];
  If[
    noises =!= Automatic &&
      (!ListQ[noises] || Length[noises] =!= Length[resolvedTimesteps] ||
        !AllTrue[noises, sameSampleShapeQ[initialNoise, #] &]),
    Message[ddimSample::noises];
    Return[$Failed]
  ];
  If[!ddimEtaCompatibleQ[eta, resolvedTimesteps, schedule],
    Message[ddimSample::eta];
    Return[$Failed]
  ];
  run = runDDIMSampling[
    predictor,
    initialNoise,
    schedule,
    eta,
    resolvedTimesteps,
    returnTrajectory,
    noises
  ] &;
  Which[
    TrueQ[eta == 0] || ListQ[noises], run[],
    IntegerQ[seed], BlockRandom[SeedRandom[seed]; run[]],
    True, run[]
  ]
];

ddimSample[___] := (
  Message[ddimSample::args];
  $Failed
);

(* === TESTS === *)

runDDIMTests[] := Module[
  {passed = 0, assert, schedule, initialNoise, predictor, fullTimesteps,
    fullNoises, fullResult, fullTrace, sparseTimesteps, sparseNoises,
    sparseTrace, manualEta, manualStep, manualExpected, sample, result,
    samples, stochasticInitial, automatic1, automatic2, seeded1, seeded2,
    differentSeed, expectedNext, actualNext, changedFinalNoise},
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ ddim/ddimSample: ", label];
    Quit[1]
  ];

  schedule = makeDiffusionSchedule[linearBetaSchedule[10, 0.01, 0.05]];
  initialNoise = {0.5, -1., 1.5};
  predictor = Function[{xt, time}, 0. xt + 0.01 time];
  fullTimesteps = Reverse[Range[schedule["Steps"]]];
  fullNoises = ConstantArray[0. initialNoise, Length[fullTimesteps]];
  fullResult = ddimSample[
    predictor,
    initialNoise,
    schedule,
    "Eta" -> 0.,
    "Noises" -> fullNoises,
    "ReturnTrajectory" -> True
  ];
  assert[
    "Automatic uses every logical timestep and appends time zero",
    fullResult["Timesteps"] === Append[fullTimesteps, 0] &&
      Length[fullResult["Trajectory"]] === Length[fullResult["Timesteps"]]
  ];
  fullTrace = Reap[
    ddimSample[
      Function[{xt, time}, Sow[time]; 0. xt],
      initialNoise,
      schedule,
      "Eta" -> 0.
    ]
  ][[2, 1]];
  assert[
    "full-step predictor calls descend from T through 1",
    fullTrace === fullTimesteps
  ];

  sparseTimesteps = {10, 7, 4, 1};
  sparseNoises = Table[
    ConstantArray[N[index]/10., Length[initialNoise]],
    {index, Length[sparseTimesteps]}
  ];
  sparseTrace = Reap[
    ddimSample[
      Function[{xt, time}, Sow[time]; 0. xt],
      initialNoise,
      schedule,
      "Eta" -> 0.,
      "Timesteps" -> sparseTimesteps
    ]
  ][[2, 1]];
  assert[
    "sparse sampling calls only the requested descending timesteps",
    sparseTrace === sparseTimesteps
  ];

  manualEta = 0.6;
  manualStep = Function[{state, transition},
    Module[
      {time, previousTime, noise, epsilon, alphaBarT,
        alphaBarPrevious, x0Hat, sigma},
      {time, previousTime, noise} = transition;
      epsilon = predictor[state, time];
      alphaBarT = schedule["AlphaBars"][[time]];
      alphaBarPrevious = If[
        previousTime == 0,
        1.,
        schedule["AlphaBars"][[previousTime]]
      ];
      x0Hat = (state - Sqrt[1 - alphaBarT] epsilon)/Sqrt[alphaBarT];
      sigma = manualEta Sqrt[
        (1 - alphaBarPrevious)/(1 - alphaBarT)
          (1 - alphaBarT/alphaBarPrevious)
      ];
      Sqrt[alphaBarPrevious] x0Hat +
        Sqrt[1 - alphaBarPrevious - sigma^2] epsilon +
        sigma noise
    ]
  ];
  manualExpected = Fold[
    manualStep,
    initialNoise,
    MapThread[
      {#1, #2, #3} &,
      {
        sparseTimesteps,
        Append[Rest[sparseTimesteps], 0],
        sparseNoises
      }
    ]
  ];
  sample = ddimSample[
    predictor,
    initialNoise,
    schedule,
    "Eta" -> manualEta,
    "Timesteps" -> sparseTimesteps,
    "Noises" -> sparseNoises
  ];
  assert[
    "sparse explicit-noise sampling matches the canonical DDIM formula",
    numericSamplesCloseQ[sample, manualExpected]
  ];
  assert[
    "sparse DDIM preserves output shape",
    Dimensions[sample] === Dimensions[initialNoise]
  ];
  result = ddimSample[
    predictor,
    initialNoise,
    schedule,
    "Eta" -> manualEta,
    "Timesteps" -> sparseTimesteps,
    "Noises" -> sparseNoises,
    "ReturnTrajectory" -> True
  ];
  assert[
    "trajectory states align with requested timesteps plus zero",
    result["Timesteps"] === {10, 7, 4, 1, 0} &&
      Length[result["Trajectory"]] === 5 &&
      First[result["Trajectory"]] === initialNoise
  ];
  assert[
    "trajectory ends at the returned x0 sample",
    Last[result["Timesteps"]] === 0 &&
      Last[result["Trajectory"]] === result["Sample"] &&
      numericSamplesCloseQ[result["Sample"], sample]
  ];
  samples = {
    1.5,
    {1., 2.},
    {{1., 2.}, {3., 4.}},
    ArrayReshape[N[Range[8]], {2, 2, 2}]
  };
  assert[
    "scalar vector matrix and tensor samples preserve shape",
    AllTrue[
      samples,
      Dimensions[
        ddimSample[
          Function[{xt, time}, 0. xt],
          #,
          schedule,
          "Eta" -> 0.,
          "Timesteps" -> sparseTimesteps
        ]
      ] === Dimensions[#] &
    ]
  ];

  assert[
    "eta zero is independent of integer Seed",
    ddimSample[
      predictor, initialNoise, schedule,
      "Eta" -> 0., "Timesteps" -> sparseTimesteps, "Seed" -> 1
    ] === ddimSample[
      predictor, initialNoise, schedule,
      "Eta" -> 0., "Timesteps" -> sparseTimesteps, "Seed" -> 999
    ]
  ];
  assert[
    "eta zero does not consult the current random stream",
    BlockRandom[
      SeedRandom[1234];
      ddimSample[
        predictor,
        initialNoise,
        schedule,
        "Eta" -> 0.,
        "Timesteps" -> sparseTimesteps
      ];
      RandomReal[]
    ] === BlockRandom[SeedRandom[1234]; RandomReal[]]
  ];

  stochasticInitial = ConstantArray[0., 64];
  {automatic1, automatic2} = BlockRandom[
    SeedRandom[13579];
    {
      ddimSample[
        Function[{xt, time}, 0. xt],
        stochasticInitial,
        schedule,
        "Eta" -> 1.,
        "Timesteps" -> sparseTimesteps
      ],
      ddimSample[
        Function[{xt, time}, 0. xt],
        stochasticInitial,
        schedule,
        "Eta" -> 1.,
        "Timesteps" -> sparseTimesteps
      ]
    }
  ];
  assert[
    "positive eta with Automatic noises consumes the current stream",
    automatic1 =!= automatic2
  ];
  expectedNext = BlockRandom[
    SeedRandom[13579];
    Table[
      randomNormalLike[stochasticInitial],
      {Length[sparseTimesteps] - 1}
    ];
    RandomReal[]
  ];
  actualNext = BlockRandom[
    SeedRandom[13579];
    ddimSample[
      Function[{xt, time}, 0. xt],
      stochasticInitial,
      schedule,
      "Eta" -> 1.,
      "Timesteps" -> sparseTimesteps
    ];
    RandomReal[]
  ];
  assert[
    "Automatic noises draw once per non-final stochastic transition",
    actualNext === expectedNext
  ];
  seeded1 = ddimSample[
    Function[{xt, time}, 0. xt],
    stochasticInitial,
    schedule,
    "Eta" -> 1.,
    "Timesteps" -> sparseTimesteps,
    "Seed" -> 2468
  ];
  seeded2 = ddimSample[
    Function[{xt, time}, 0. xt],
    stochasticInitial,
    schedule,
    "Eta" -> 1.,
    "Timesteps" -> sparseTimesteps,
    "Seed" -> 2468
  ];
  assert[
    "identical integer seeds reproduce stochastic DDIM",
    seeded1 === seeded2
  ];
  differentSeed = ddimSample[
    Function[{xt, time}, 0. xt],
    stochasticInitial,
    schedule,
    "Eta" -> 1.,
    "Timesteps" -> sparseTimesteps,
    "Seed" -> 8642
  ];
  assert[
    "different integer seeds change stochastic DDIM",
    seeded1 =!= differentSeed
  ];
  assert[
    "integer-seeded stochastic DDIM is isolated from caller RNG",
    BlockRandom[
      SeedRandom[97531];
      ddimSample[
        Function[{xt, time}, 0. xt],
        stochasticInitial,
        schedule,
        "Eta" -> 1.,
        "Timesteps" -> sparseTimesteps,
        "Seed" -> 2468
      ];
      RandomReal[]
    ] === BlockRandom[SeedRandom[97531]; RandomReal[]]
  ];
  assert[
    "explicit noises make Seed irrelevant",
    ddimSample[
      predictor, initialNoise, schedule,
      "Eta" -> manualEta, "Timesteps" -> sparseTimesteps,
      "Noises" -> sparseNoises, "Seed" -> 1
    ] === ddimSample[
      predictor, initialNoise, schedule,
      "Eta" -> manualEta, "Timesteps" -> sparseTimesteps,
      "Noises" -> sparseNoises, "Seed" -> 999
    ]
  ];
  assert[
    "explicit noises do not consult the current random stream",
    BlockRandom[
      SeedRandom[54321];
      ddimSample[
        predictor,
        initialNoise,
        schedule,
        "Eta" -> manualEta,
        "Timesteps" -> sparseTimesteps,
        "Noises" -> sparseNoises
      ];
      RandomReal[]
    ] === BlockRandom[SeedRandom[54321]; RandomReal[]]
  ];
  changedFinalNoise = ReplacePart[
    sparseNoises,
    -1 -> ConstantArray[10.^6, Length[initialNoise]]
  ];
  assert[
    "the validated final explicit noise is ignored at the x0 transition",
    sample === ddimSample[
      predictor,
      initialNoise,
      schedule,
      "Eta" -> manualEta,
      "Timesteps" -> sparseTimesteps,
      "Noises" -> changedFinalNoise
    ]
  ];

  assert[
    "wrong predictor shape fails",
    Quiet[
      ddimSample[
        Function[{xt, time}, {0.}],
        initialNoise,
        schedule,
        "Eta" -> 0.,
        "Timesteps" -> sparseTimesteps
      ]
    ] === $Failed
  ];
  assert[
    "non-finite predictor output fails",
    Quiet[
      ddimSample[
        Function[{xt, time}, {0., Infinity, 0.}],
        initialNoise,
        schedule,
        "Eta" -> 0.,
        "Timesteps" -> sparseTimesteps
      ]
    ] === $Failed
  ];
  assert[
    "non-numeric and non-finite initial samples fail",
    Quiet[ddimSample[predictor, symbolic, schedule]] === $Failed &&
      Quiet[
        ddimSample[predictor, {0., Infinity, 0.}, schedule]
      ] === $Failed
  ];
  assert[
    "negative non-numeric non-finite and incompatible eta values fail",
    Quiet[
      ddimSample[predictor, initialNoise, schedule, "Eta" -> -0.1]
    ] === $Failed &&
      Quiet[
        ddimSample[predictor, initialNoise, schedule, "Eta" -> "one"]
      ] === $Failed &&
      Quiet[
        ddimSample[predictor, initialNoise, schedule, "Eta" -> Infinity]
      ] === $Failed &&
      Quiet[
        ddimSample[
          predictor,
          initialNoise,
          schedule,
          "Eta" -> 100.,
          "Timesteps" -> sparseTimesteps
        ]
      ] === $Failed
  ];
  assert[
    "eta values above one remain valid when transition variances allow them",
    realNumericSampleQ[
      ddimSample[
        predictor,
        initialNoise,
        schedule,
        "Eta" -> 1.01,
        "Timesteps" -> sparseTimesteps,
        "Noises" -> sparseNoises
      ]
    ]
  ];
  assert[
    "empty out-of-range and non-integer timestep lists fail",
    Quiet[
      ddimSample[predictor, initialNoise, schedule, "Timesteps" -> {}]
    ] === $Failed &&
      Quiet[
        ddimSample[
          predictor, initialNoise, schedule, "Timesteps" -> {11, 1}
        ]
      ] === $Failed &&
      Quiet[
        ddimSample[
          predictor, initialNoise, schedule, "Timesteps" -> {10., 1}
        ]
      ] === $Failed
  ];
  assert[
    "non-descending and duplicate timestep lists fail",
    Quiet[
      ddimSample[
        predictor, initialNoise, schedule, "Timesteps" -> {10, 4, 7, 1}
      ]
    ] === $Failed &&
      Quiet[
        ddimSample[
          predictor, initialNoise, schedule, "Timesteps" -> {10, 7, 7, 1}
        ]
      ] === $Failed
  ];
  assert[
    "non-boolean ReturnTrajectory and invalid Seed fail",
    Quiet[
      ddimSample[
        predictor, initialNoise, schedule, "ReturnTrajectory" -> 1
      ]
    ] === $Failed &&
      Quiet[
        ddimSample[predictor, initialNoise, schedule, "Seed" -> 1.5]
      ] === $Failed
  ];
  assert[
    "wrong-length wrong-shape and non-finite explicit noises fail",
    Quiet[
      ddimSample[
        predictor,
        initialNoise,
        schedule,
        "Timesteps" -> sparseTimesteps,
        "Noises" -> Most[sparseNoises]
      ]
    ] === $Failed &&
      Quiet[
        ddimSample[
          predictor,
          initialNoise,
          schedule,
          "Timesteps" -> sparseTimesteps,
          "Noises" -> ConstantArray[{0.}, Length[sparseTimesteps]]
        ]
      ] === $Failed &&
      Quiet[
        ddimSample[
          predictor,
          initialNoise,
          schedule,
          "Timesteps" -> sparseTimesteps,
          "Noises" -> ReplacePart[
            sparseNoises,
            2 -> {0., Indeterminate, 0.}
          ]
        ]
      ] === $Failed
  ];
  assert[
    "malformed schedules fail",
    Quiet[
      ddimSample[
        predictor,
        initialNoise,
        ReplacePart[schedule, "Steps" -> 9]
      ]
    ] === $Failed
  ];
  assert[
    "unknown duplicate and non-rule options fail",
    Quiet[
      ddimSample[predictor, initialNoise, schedule, "Unknown" -> True]
    ] === $Failed &&
      Quiet[
        ddimSample[
          predictor,
          initialNoise,
          schedule,
          "Eta" -> 0.,
          "Eta" -> 1.
        ]
      ] === $Failed &&
      Quiet[ddimSample[predictor, initialNoise, schedule, 42]] === $Failed
  ];
  assert[
    "missing and extra positional arguments fail",
    Quiet[ddimSample[]] === $Failed &&
      Quiet[ddimSample[predictor, initialNoise, schedule, <||>]] === $Failed
  ];

  Print["✓ ddim — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "ddim.wl"],
  Stochasma`Private`runDDIMTests[]
]
