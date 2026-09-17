If[
  DownValues[Stochasma`forwardDiffuse] === {},
  Get[FileNameJoin[{DirectoryName[$InputFileName], "forward.wl"}]]
];

BeginPackage["Stochasma`"]

makeDiffusionTrainingSample::usage =
  "makeDiffusionTrainingSample[x0, t, schedule] creates an epsilon-prediction training example at t in 1 through T using the current random stream. The four-argument form uses explicit same-shape noise deterministically and returns keys \"Clean\", \"Noisy\", \"Time\", and \"Noise\".";

epsilonPredictionLoss::usage =
  "epsilonPredictionLoss[predicted, target] returns the mean squared error between same-shape finite real epsilon samples.";

Begin["`Private`"]

makeDiffusionTrainingSample::sample =
  "x0 must be a finite real numeric scalar or non-empty array.";
makeDiffusionTrainingSample::schedule =
  "The supplied diffusion schedule is malformed or internally inconsistent.";
makeDiffusionTrainingSample::time =
  "Training time t must be an Integer in the inclusive range 1 through `1`.";
makeDiffusionTrainingSample::noise =
  "Noise must be finite, real-valued, and have the same shape as x0.";
makeDiffusionTrainingSample::args =
  "makeDiffusionTrainingSample expects x0, an Integer time, a diffusion schedule, and optional explicit noise.";

makeDiffusionTrainingSample[
  x0_,
  t_Integer,
  schedule_Association
] := Module[{noise},
  If[!realNumericSampleQ[x0],
    Message[makeDiffusionTrainingSample::sample];
    Return[$Failed]
  ];
  If[!diffusionScheduleQ[schedule],
    Message[makeDiffusionTrainingSample::schedule];
    Return[$Failed]
  ];
  If[!TrueQ[1 <= t <= schedule["Steps"]],
    Message[makeDiffusionTrainingSample::time, schedule["Steps"]];
    Return[$Failed]
  ];
  noise = randomNormalLike[x0];
  makeDiffusionTrainingSample[x0, t, schedule, noise]
];

makeDiffusionTrainingSample[
  x0_,
  t_Integer,
  schedule_Association,
  noise_
] := Module[{noisy},
  If[!realNumericSampleQ[x0],
    Message[makeDiffusionTrainingSample::sample];
    Return[$Failed]
  ];
  If[!diffusionScheduleQ[schedule],
    Message[makeDiffusionTrainingSample::schedule];
    Return[$Failed]
  ];
  If[!TrueQ[1 <= t <= schedule["Steps"]],
    Message[makeDiffusionTrainingSample::time, schedule["Steps"]];
    Return[$Failed]
  ];
  If[!sameSampleShapeQ[x0, noise],
    Message[makeDiffusionTrainingSample::noise];
    Return[$Failed]
  ];
  noisy = forwardDiffuse[x0, t, schedule, noise];
  <|
    "Clean" -> x0,
    "Noisy" -> noisy,
    "Time" -> t,
    "Noise" -> noise
  |>
];

makeDiffusionTrainingSample[___] := (
  Message[makeDiffusionTrainingSample::args];
  $Failed
);

epsilonPredictionLoss::shape =
  "Predicted and target epsilon samples must be finite, real-valued, and have the same shape.";
epsilonPredictionLoss::args =
  "epsilonPredictionLoss expects predicted and target epsilon samples.";

epsilonPredictionLoss[predicted_, target_] := Module[{},
  If[!sameSampleShapeQ[predicted, target],
    Message[epsilonPredictionLoss::shape];
    Return[$Failed]
  ];
  Mean[Flatten[{(predicted - target)^2}]]
];

epsilonPredictionLoss[___] := (
  Message[epsilonPredictionLoss::args];
  $Failed
);

End[]

EndPackage[]
