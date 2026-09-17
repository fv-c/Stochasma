If[
  DownValues[Stochasma`makeDiffusionSchedule] === {},
  Get[FileNameJoin[{DirectoryName[$InputFileName], "schedules.wl"}]]
];

BeginPackage["Stochasma`"]

forwardDiffuse::usage =
  "forwardDiffuse[x0, t, schedule] samples q(x_t | x_0) for logical time t using the current random stream, while forwardDiffuse[x0, t, schedule, noise] uses explicit same-shape Gaussian noise deterministically. At t = 0 both forms return x0 exactly without generating noise.";

Begin["`Private`"]

realNumericSampleQ[value_] := finiteRealNumberQ[value] ||
  (ArrayQ[value, _, finiteRealNumberQ] && Flatten[value] =!= {});

sameSampleShapeQ[left_, right_] :=
  realNumericSampleQ[left] && realNumericSampleQ[right] &&
    Dimensions[left] === Dimensions[right];

randomNormalLike[sample_] := If[
  ArrayQ[sample],
  ArrayReshape[
    RandomVariate[NormalDistribution[0, 1], Times @@ Dimensions[sample]],
    Dimensions[sample]
  ],
  RandomVariate[NormalDistribution[0, 1]]
];

forwardDiffuse::sample =
  "The clean sample must be a finite real numeric scalar or non-empty array.";
forwardDiffuse::schedule =
  "The supplied diffusion schedule is malformed or internally inconsistent.";
forwardDiffuse::time =
  "Time t must be an Integer in the inclusive range 0 through `1`.";
forwardDiffuse::noise =
  "Noise must be a finite real numeric sample with the same shape as x0.";
forwardDiffuse::args =
  "forwardDiffuse expects x0, an Integer time, a diffusion schedule, and optional explicit noise.";

forwardDiffuse[x0_, t_Integer, schedule_Association] := Module[{noise},
  If[!realNumericSampleQ[x0],
    Message[forwardDiffuse::sample];
    Return[$Failed]
  ];
  If[!diffusionScheduleQ[schedule],
    Message[forwardDiffuse::schedule];
    Return[$Failed]
  ];
  If[!TrueQ[0 <= t <= schedule["Steps"]],
    Message[forwardDiffuse::time, schedule["Steps"]];
    Return[$Failed]
  ];
  If[t == 0, Return[x0]];
  noise = randomNormalLike[x0];
  forwardDiffuse[x0, t, schedule, noise]
];

forwardDiffuse[x0_, t_Integer, schedule_Association, noise_] := Module[
  {sqrtAlphaBar, sqrtOneMinusAlphaBar},
  If[!realNumericSampleQ[x0],
    Message[forwardDiffuse::sample];
    Return[$Failed]
  ];
  If[!diffusionScheduleQ[schedule],
    Message[forwardDiffuse::schedule];
    Return[$Failed]
  ];
  If[!TrueQ[0 <= t <= schedule["Steps"]],
    Message[forwardDiffuse::time, schedule["Steps"]];
    Return[$Failed]
  ];
  If[!sameSampleShapeQ[x0, noise],
    Message[forwardDiffuse::noise];
    Return[$Failed]
  ];
  If[t == 0, Return[x0]];
  sqrtAlphaBar = schedule["SqrtAlphaBars"][[t]];
  sqrtOneMinusAlphaBar = schedule["SqrtOneMinusAlphaBars"][[t]];
  sqrtAlphaBar x0 + sqrtOneMinusAlphaBar noise
];

forwardDiffuse[___] := (
  Message[forwardDiffuse::args];
  $Failed
);

End[]

EndPackage[]
