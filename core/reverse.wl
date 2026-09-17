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

predictCleanSampleValidated[
  xt_,
  t_Integer,
  predictedNoise_,
  schedule_Association
] := If[
  t == 0,
  xt,
  (xt -
      schedule["SqrtOneMinusAlphaBars"][[t]] predictedNoise)/
    schedule["SqrtAlphaBars"][[t]]
];

predictCleanSample[
  xt_,
  t_Integer,
  predictedNoise_,
  schedule_Association
] := Module[{},
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
  predictCleanSampleValidated[xt, t, predictedNoise, schedule]
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

reverseMeanVarianceValidated[
  xt_,
  t_Integer,
  predictedNoise_,
  schedule_Association
] := Module[{predictedX0, coefficient1, coefficient2},
  predictedX0 = predictCleanSampleValidated[
    xt,
    t,
    predictedNoise,
    schedule
  ];
  coefficient1 = schedule["PosteriorMeanCoefficient1"][[t]];
  coefficient2 = schedule["PosteriorMeanCoefficient2"][[t]];
  <|
    "PredictedX0" -> predictedX0,
    "Mean" -> coefficient1 predictedX0 + coefficient2 xt,
    "Variance" -> schedule["PosteriorVariances"][[t]]
  |>
];

reverseMeanVariance[
  xt_,
  t_Integer,
  predictedNoise_,
  schedule_Association
] := Module[{},
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
  reverseMeanVarianceValidated[xt, t, predictedNoise, schedule]
];

reverseMeanVariance[___] := (
  Message[reverseMeanVariance::args];
  $Failed
);

reverseDiffuseStep::noise =
  "Noise must be a finite real numeric sample with the same shape as xt.";
reverseDiffuseStep::sample =
  "xt must be a finite real numeric scalar or non-empty array.";
reverseDiffuseStep::schedule =
  "The supplied diffusion schedule is malformed or internally inconsistent.";
reverseDiffuseStep::time =
  "Time t must be an Integer in the inclusive range 1 through `1`.";
reverseDiffuseStep::prediction =
  "predictedNoise must be finite, real-valued, and have the same shape as xt.";
reverseDiffuseStep::args =
  "reverseDiffuseStep expects xt, an Integer time in 1 through T, predictedNoise, a diffusion schedule, and optional explicit noise.";

reverseDiffuseStepValidated[
  xt_,
  t_Integer,
  predictedNoise_,
  schedule_Association,
  noise_
] := Module[{posterior},
  posterior = reverseMeanVarianceValidated[
    xt,
    t,
    predictedNoise,
    schedule
  ];
  If[
    t == 1,
    posterior["Mean"],
    posterior["Mean"] + Sqrt[posterior["Variance"]] noise
  ]
];

reverseDiffuseStep[
  xt_,
  t_Integer,
  predictedNoise_,
  schedule_Association
] := Module[{noise},
  If[!realNumericSampleQ[xt],
    Message[reverseDiffuseStep::sample];
    Return[$Failed]
  ];
  If[!diffusionScheduleQ[schedule],
    Message[reverseDiffuseStep::schedule];
    Return[$Failed]
  ];
  If[!TrueQ[1 <= t <= schedule["Steps"]],
    Message[reverseDiffuseStep::time, schedule["Steps"]];
    Return[$Failed]
  ];
  If[!sameSampleShapeQ[xt, predictedNoise],
    Message[reverseDiffuseStep::prediction];
    Return[$Failed]
  ];
  If[t == 1,
    Return[
      reverseDiffuseStepValidated[
        xt,
        t,
        predictedNoise,
        schedule,
        0. xt
      ]
    ]
  ];
  noise = randomNormalLike[xt];
  reverseDiffuseStepValidated[xt, t, predictedNoise, schedule, noise]
];

reverseDiffuseStep[
  xt_,
  t_Integer,
  predictedNoise_,
  schedule_Association,
  noise_
] := Module[{},
  If[!realNumericSampleQ[xt],
    Message[reverseDiffuseStep::sample];
    Return[$Failed]
  ];
  If[!diffusionScheduleQ[schedule],
    Message[reverseDiffuseStep::schedule];
    Return[$Failed]
  ];
  If[!TrueQ[1 <= t <= schedule["Steps"]],
    Message[reverseDiffuseStep::time, schedule["Steps"]];
    Return[$Failed]
  ];
  If[!sameSampleShapeQ[xt, predictedNoise],
    Message[reverseDiffuseStep::prediction];
    Return[$Failed]
  ];
  If[!sameSampleShapeQ[xt, noise],
    Message[reverseDiffuseStep::noise];
    Return[$Failed]
  ];
  reverseDiffuseStepValidated[xt, t, predictedNoise, schedule, noise]
];

reverseDiffuseStep[___] := (
  Message[reverseDiffuseStep::args];
  $Failed
);

End[]

EndPackage[]
