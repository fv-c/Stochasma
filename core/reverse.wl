If[
  DownValues[Stochasma`forwardDiffuse] === {},
  Get[FileNameJoin[{DirectoryName[$InputFileName], "forward.wl"}]]
];

BeginPackage["Stochasma`"]

predictCleanSample::usage =
  "predictCleanSample[xt, t, predictedNoise, schedule] reconstructs x0 from a noisy sample and a same-shape epsilon prediction. At t = 0 it returns xt exactly.";

reverseMeanVariance::usage =
  "reverseMeanVariance[xt, t, predictedNoise, schedule] returns an Association with \"PredictedX0\", the deterministic DDPM posterior \"Mean\", and scalar posterior \"Variance\" for q(x_(t-1) | x_t, x0-hat).";

reverseDiffuseStep::usage =
  "reverseDiffuseStep[xt, t, predictedNoise, schedule] samples one DDPM step from t to t-1 using the current random stream. The five-argument form uses explicit same-shape noise deterministically; at t = 1 no noise is generated or added.";

Begin["`Private`"]

numericSamplesCloseQ[left_, right_, tolerance_ : 10^-10] := Quiet[Check[
  sameSampleShapeQ[left, right] &&
    Max[Abs[Flatten[{N[left - right]}]]] <= tolerance,
  False
]];

predictCleanSample::sample =
  "xt must be a finite real numeric scalar or non-empty array.";
predictCleanSample::schedule =
  "The supplied diffusion schedule is malformed or internally inconsistent.";
predictCleanSample::time =
  "Time t must be an Integer in the inclusive range 0 through `1`.";
predictCleanSample::noise =
  "predictedNoise must be finite, real-valued, and have the same shape as xt.";
predictCleanSample::args =
  "predictCleanSample expects xt, an Integer time, predictedNoise, and a diffusion schedule.";

predictCleanSample[
  xt_,
  t_Integer,
  predictedNoise_,
  schedule_Association
] := Module[{sqrtAlphaBar, sqrtOneMinusAlphaBar},
  If[!realNumericSampleQ[xt],
    Message[predictCleanSample::sample];
    Return[$Failed]
  ];
  If[!diffusionScheduleQ[schedule],
    Message[predictCleanSample::schedule];
    Return[$Failed]
  ];
  If[!TrueQ[0 <= t <= schedule["Steps"]],
    Message[predictCleanSample::time, schedule["Steps"]];
    Return[$Failed]
  ];
  If[!sameSampleShapeQ[xt, predictedNoise],
    Message[predictCleanSample::noise];
    Return[$Failed]
  ];
  If[t == 0, Return[xt]];
  sqrtAlphaBar = schedule["SqrtAlphaBars"][[t]];
  sqrtOneMinusAlphaBar = schedule["SqrtOneMinusAlphaBars"][[t]];
  (xt - sqrtOneMinusAlphaBar predictedNoise)/sqrtAlphaBar
];

predictCleanSample[___] := (
  Message[predictCleanSample::args];
  $Failed
);

reverseMeanVariance::sample =
  "xt must be a finite real numeric scalar or non-empty array.";
reverseMeanVariance::schedule =
  "The supplied diffusion schedule is malformed or internally inconsistent.";
reverseMeanVariance::time =
  "Time t must be an Integer in the inclusive range 1 through `1`.";
reverseMeanVariance::noise =
  "predictedNoise must be finite, real-valued, and have the same shape as xt.";
reverseMeanVariance::args =
  "reverseMeanVariance expects xt, an Integer time, predictedNoise, and a diffusion schedule.";

reverseMeanVariance[
  xt_,
  t_Integer,
  predictedNoise_,
  schedule_Association
] := Module[{predictedX0, coefficient1, coefficient2},
  If[!realNumericSampleQ[xt],
    Message[reverseMeanVariance::sample];
    Return[$Failed]
  ];
  If[!diffusionScheduleQ[schedule],
    Message[reverseMeanVariance::schedule];
    Return[$Failed]
  ];
  If[!TrueQ[1 <= t <= schedule["Steps"]],
    Message[reverseMeanVariance::time, schedule["Steps"]];
    Return[$Failed]
  ];
  If[!sameSampleShapeQ[xt, predictedNoise],
    Message[reverseMeanVariance::noise];
    Return[$Failed]
  ];
  predictedX0 = predictCleanSample[xt, t, predictedNoise, schedule];
  coefficient1 = schedule["PosteriorMeanCoefficient1"][[t]];
  coefficient2 = schedule["PosteriorMeanCoefficient2"][[t]];
  <|
    "PredictedX0" -> predictedX0,
    "Mean" -> coefficient1 predictedX0 + coefficient2 xt,
    "Variance" -> schedule["PosteriorVariances"][[t]]
  |>
];

reverseMeanVariance[___] := (
  Message[reverseMeanVariance::args];
  $Failed
);

reverseDiffuseStep::noise =
  "Noise must be a finite real numeric sample with the same shape as xt.";
reverseDiffuseStep::args =
  "reverseDiffuseStep expects xt, an Integer time in 1 through T, predictedNoise, a diffusion schedule, and optional explicit noise.";

reverseDiffuseStep[
  xt_,
  t_Integer,
  predictedNoise_,
  schedule_Association
] := Module[{posterior, noise},
  posterior = reverseMeanVariance[xt, t, predictedNoise, schedule];
  If[posterior === $Failed, Return[$Failed]];
  If[t == 1, Return[posterior["Mean"]]];
  noise = randomNormalLike[xt];
  posterior["Mean"] + Sqrt[posterior["Variance"]] noise
];

reverseDiffuseStep[
  xt_,
  t_Integer,
  predictedNoise_,
  schedule_Association,
  noise_
] := Module[{posterior},
  posterior = reverseMeanVariance[xt, t, predictedNoise, schedule];
  If[posterior === $Failed, Return[$Failed]];
  If[!sameSampleShapeQ[xt, noise],
    Message[reverseDiffuseStep::noise];
    Return[$Failed]
  ];
  If[t == 1, Return[posterior["Mean"]]];
  posterior["Mean"] + Sqrt[posterior["Variance"]] noise
];

reverseDiffuseStep[___] := (
  Message[reverseDiffuseStep::args];
  $Failed
);

(* === TESTS === *)

runReverseTests[] := Module[
  {passed = 0, assert, schedule, t, samples, noises, noisySamples,
    reconstructions, posterior, expectedMean, firstPosterior, explicitNoise,
    reverseStep, stochasticState, stochasticPrediction, firstDraw, secondDraw,
    replay1, replay2, expectedNext, actualNext},
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
    "invalid predicted-noise shape fails",
    Quiet[predictCleanSample[samples[[2]], t, {0., 0.}, schedule]] === $Failed
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
    "reverse step rejects invalid explicit-noise shape",
    Quiet[
      reverseDiffuseStep[noisySamples[[2]], t, noises[[2]], schedule, {0.}]
    ] === $Failed
  ];
  assert[
    "reverse step rejects times outside 1 through T",
    Quiet[
      reverseDiffuseStep[samples[[2]], 0, noises[[2]], schedule]
    ] === $Failed &&
      Quiet[
        reverseDiffuseStep[samples[[2]], 9, noises[[2]], schedule]
      ] === $Failed
  ];

  Print["✓ reverse — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "reverse.wl"],
  Stochasma`Private`runReverseTests[]
]
