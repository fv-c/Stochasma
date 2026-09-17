If[
  DownValues[Stochasma`predictCleanSample] === {},
  Get[FileNameJoin[{DirectoryName[$InputFileName], "reverse.wl"}]]
];
If[
  DownValues[Stochasma`ddpmSample] === {},
  Get[FileNameJoin[{DirectoryName[$InputFileName], "sampling.wl"}]]
];

BeginPackage["Stochasma`"]

ddimSample::usage =
  "ddimSample[predictor, initialNoise, schedule, opts] applies predictor[xt, t] at each requested logical time and returns x0. initialNoise is the state at the first resolved timestep: with Automatic \"Timesteps\" it is xT, while an explicit sequence beginning below T interprets it at that first requested logical time. \"Timesteps\" is Automatic or a non-empty strictly decreasing list in 1..T; \"ReturnTrajectory\" -> True returns <|\"Sample\" -> x0, \"Trajectory\" -> {xStart, ..., x0}, \"Timesteps\" -> {tStart, ..., 0}|>. \"Eta\" -> 0 is deterministic. For positive eta, Automatic noises use the current stream or a locally isolated Integer \"Seed\". Explicit \"Noises\" has the same length and order as \"Timesteps\"; its final entry is validated but ignored because the transition to time 0 adds no noise.";

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

ddimStepValidated[
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
  predictedX0 = predictCleanSampleValidated[
    xt,
    t,
    predictedNoise,
    schedule
  ];
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
    state = ddimStepValidated[
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

End[]

EndPackage[]
