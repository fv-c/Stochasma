If[
  DownValues[Stochasma`Private`finiteRealNumberQ] === {},
  Get[FileNameJoin[{DirectoryName[$InputFileName], "validation.wl"}]]
];

BeginPackage["Stochasma`"]

linearBetaSchedule::usage =
  "linearBetaSchedule[steps, betaStart, betaEnd] returns steps linearly spaced beta values for logical times 1 through steps. steps must be positive and 0 < betaStart <= betaEnd < 1.";

cosineBetaSchedule::usage =
  "cosineBetaSchedule[steps, opts] returns the cosine DDPM beta schedule for logical times 1 through steps. Options are \"Offset\" (default 0.008) and the documented generated-beta cap \"MaxBeta\" (default 0.999).";

makeDiffusionSchedule::usage =
  "makeDiffusionSchedule[betas] validates beta values for logical times 1 through T and returns an Association containing the canonical Gaussian DDPM coefficients.";

Begin["`Private`"]

numericListsCloseQ[left_List, right_List, tolerance_ : 10^-10] := Quiet[Check[
  Dimensions[left] === Dimensions[right] &&
    Max[Abs[Flatten[N[left - right]]]] <= tolerance,
  False
]];

linearBetaSchedule::args =
  "linearBetaSchedule expects a positive Integer step count and two finite real beta endpoints.";
linearBetaSchedule::range =
  "Beta endpoints must satisfy 0 < betaStart <= betaEnd < 1.";

linearBetaSchedule[steps_Integer, betaStart_, betaEnd_] := Module[{},
  If[steps <= 0 || !finiteRealNumberQ[betaStart] || !finiteRealNumberQ[betaEnd],
    Message[linearBetaSchedule::args];
    Return[$Failed]
  ];
  If[!TrueQ[0 < betaStart <= betaEnd < 1],
    Message[linearBetaSchedule::range];
    Return[$Failed]
  ];
  If[
    steps == 1,
    {N[betaStart]},
    N[Subdivide[betaStart, betaEnd, steps - 1]]
  ]
];

linearBetaSchedule[___] := (
  Message[linearBetaSchedule::args];
  $Failed
);

Options[cosineBetaSchedule] = {
  "Offset" -> 0.008,
  "MaxBeta" -> 0.999
};

cosineBetaSchedule::args =
  "cosineBetaSchedule expects a positive Integer step count and valid options.";
cosineBetaSchedule::offset =
  "The \"Offset\" option must be a finite non-negative real number.";
cosineBetaSchedule::maxbeta =
  "The \"MaxBeta\" option must be a finite real number strictly between 0 and 1.";

cosineBetaSchedule[steps_, opts___] := Module[
  {given, keys, values, offset, maxBeta, grid, alphaBars, betas},
  given = {opts};
  If[
    !IntegerQ[steps] || steps <= 0 || !OptionQ[given],
    Message[cosineBetaSchedule::args];
    Return[$Failed]
  ];
  keys = First /@ given;
  If[
    !AllTrue[keys, MemberQ[{"Offset", "MaxBeta"}, #] &] ||
      DuplicateFreeQ[keys] === False,
    Message[cosineBetaSchedule::args];
    Return[$Failed]
  ];
  values = Join[Association[Options[cosineBetaSchedule]], Association[given]];
  offset = values["Offset"];
  maxBeta = values["MaxBeta"];
  If[!finiteRealNumberQ[offset] || !TrueQ[offset >= 0],
    Message[cosineBetaSchedule::offset];
    Return[$Failed]
  ];
  If[!finiteRealNumberQ[maxBeta] || !TrueQ[0 < maxBeta < 1],
    Message[cosineBetaSchedule::maxbeta];
    Return[$Failed]
  ];
  grid = N[Range[0, steps]/steps];
  alphaBars = Cos[((grid + offset)/(1 + offset)) Pi/2]^2;
  alphaBars = alphaBars/First[alphaBars];
  betas = Min[#, N[maxBeta]] & /@ (1 - Rest[alphaBars]/Most[alphaBars]);
  If[!AllTrue[betas, finiteRealNumberQ[#] && 0 < # < 1 &],
    Message[cosineBetaSchedule::args];
    Return[$Failed]
  ];
  betas
];

makeDiffusionSchedule::betas =
  "Betas must be a non-empty list of finite real numbers strictly between 0 and 1.";
makeDiffusionSchedule::numeric =
  "The supplied beta values cannot produce a finite, internally consistent diffusion schedule after numerical evaluation.";

derivedScheduleValues[betas_List] := Module[
  {alphas, alphaBars, previousAlphaBars, posteriorVariances,
    posteriorMeanCoefficient1, posteriorMeanCoefficient2},
  alphas = 1 - betas;
  alphaBars = Rest[FoldList[Times, 1., alphas]];
  previousAlphaBars = Prepend[Most[alphaBars], 1.];
  posteriorVariances =
    betas (1 - previousAlphaBars)/(1 - alphaBars);
  posteriorMeanCoefficient1 =
    betas Sqrt[previousAlphaBars]/(1 - alphaBars);
  posteriorMeanCoefficient2 =
    (1 - previousAlphaBars) Sqrt[alphas]/(1 - alphaBars);
  <|
    "Alphas" -> alphas,
    "AlphaBars" -> alphaBars,
    "SqrtAlphaBars" -> Sqrt[alphaBars],
    "SqrtOneMinusAlphaBars" -> Sqrt[1 - alphaBars],
    "PosteriorVariances" -> posteriorVariances,
    "PosteriorMeanCoefficient1" -> posteriorMeanCoefficient1,
    "PosteriorMeanCoefficient2" -> posteriorMeanCoefficient2
  |>
];

diffusionScheduleQ[schedule_Association] := Module[
  {requiredKeys, steps, betas, derived},
  requiredKeys = {
    "Steps", "Betas", "Alphas", "AlphaBars", "SqrtAlphaBars",
    "SqrtOneMinusAlphaBars", "PosteriorVariances",
    "PosteriorMeanCoefficient1", "PosteriorMeanCoefficient2"
  };
  If[!AllTrue[requiredKeys, KeyExistsQ[schedule, #] &], Return[False]];
  steps = schedule["Steps"];
  betas = schedule["Betas"];
  If[
    !IntegerQ[steps] || steps <= 0 || !ListQ[betas] ||
      Length[betas] =!= steps ||
      !AllTrue[betas, finiteRealNumberQ[#] && 0 < # < 1 &],
    Return[False]
  ];
  If[
    !AllTrue[
      Rest[requiredKeys],
      ListQ[schedule[#]] && Length[schedule[#]] === steps &&
        AllTrue[schedule[#], finiteRealNumberQ] &
    ],
    Return[False]
  ];
  derived = derivedScheduleValues[N[betas]];
  And @@ (
    numericListsCloseQ[schedule[#], derived[#]] & /@
      Keys[derived]
  )
];

diffusionScheduleQ[_] := False;

makeDiffusionSchedule[betas_List] := Module[
  {numericBetas, derived, schedule},
  If[
    betas === {} ||
      !AllTrue[betas, finiteRealNumberQ[#] && TrueQ[0 < # < 1] &],
    Message[makeDiffusionSchedule::betas];
    Return[$Failed]
  ];
  numericBetas = N[betas];
  derived = Quiet[Check[derivedScheduleValues[numericBetas], <||>]];
  schedule = Join[
    <|"Steps" -> Length[numericBetas], "Betas" -> numericBetas|>,
    derived
  ];
  If[!diffusionScheduleQ[schedule],
    Message[makeDiffusionSchedule::numeric];
    Return[$Failed]
  ];
  schedule
];

makeDiffusionSchedule[___] := (
  Message[makeDiffusionSchedule::betas];
  $Failed
);

End[]

EndPackage[]
