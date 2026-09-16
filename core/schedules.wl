BeginPackage["Stochasma`"]

linearBetaSchedule::usage =
  "linearBetaSchedule[steps, betaStart, betaEnd] returns steps linearly spaced beta values for logical times 1 through steps. steps must be positive and 0 < betaStart <= betaEnd < 1.";

cosineBetaSchedule::usage =
  "cosineBetaSchedule[steps, opts] returns the cosine DDPM beta schedule for logical times 1 through steps. Options are \"Offset\" (default 0.008) and the documented generated-beta cap \"MaxBeta\" (default 0.999).";

makeDiffusionSchedule::usage =
  "makeDiffusionSchedule[betas] validates beta values for logical times 1 through T and returns an Association containing the canonical Gaussian DDPM coefficients.";

Begin["`Private`"]

finiteRealNumberQ[value_] := Quiet[Check[
  NumberQ[N[value]] &&
    TrueQ[Im[N[value]] == 0] &&
    FreeQ[N[value], Indeterminate | ComplexInfinity | DirectedInfinity],
  False
]];

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

makeDiffusionSchedule[betas_List] := Module[{numericBetas, derived},
  If[
    betas === {} ||
      !AllTrue[betas, finiteRealNumberQ[#] && TrueQ[0 < # < 1] &],
    Message[makeDiffusionSchedule::betas];
    Return[$Failed]
  ];
  numericBetas = N[betas];
  derived = derivedScheduleValues[numericBetas];
  Join[
    <|"Steps" -> Length[numericBetas], "Betas" -> numericBetas|>,
    derived
  ]
];

makeDiffusionSchedule[___] := (
  Message[makeDiffusionSchedule::betas];
  $Failed
);

(* === TESTS === *)

runSchedulesTests[] := Module[
  {passed = 0, assert, betas, cosineBetas, schedule, alphaBars,
    previousAlphaBars},
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ schedules/linearBetaSchedule: ", label];
    Quit[1]
  ];

  betas = linearBetaSchedule[5, 0.0001, 0.02];
  assert[
    "returns requested length and endpoints",
    Length[betas] == 5 && First[betas] == 0.0001 && Last[betas] == 0.02
  ];
  assert[
    "all beta values are finite and in range",
    AllTrue[betas, finiteRealNumberQ[#] && 0 < # < 1 &]
  ];
  assert[
    "values are linearly spaced",
    Max[Abs[Differences[Differences[betas]]]] < 10^-14
  ];
  assert[
    "single-step schedule uses betaStart",
    linearBetaSchedule[1, 0.1, 0.2] === {0.1}
  ];
  assert[
    "invalid counts and endpoints fail",
    Quiet[linearBetaSchedule[0, 0.1, 0.2]] === $Failed &&
      Quiet[linearBetaSchedule[3, 0., 0.2]] === $Failed &&
      Quiet[linearBetaSchedule[3, 0.3, 0.2]] === $Failed &&
      Quiet[linearBetaSchedule[3., 0.1, 0.2]] === $Failed
  ];

  cosineBetas = cosineBetaSchedule[10];
  assert[
    "cosine schedule returns requested length",
    Length[cosineBetas] == 10
  ];
  assert[
    "cosine beta values are finite and in range",
    AllTrue[cosineBetas, finiteRealNumberQ[#] && 0 < # < 1 &]
  ];
  assert[
    "cosine cumulative alpha products decrease",
    And @@ Thread[
      Differences[Rest[FoldList[Times, 1., 1 - cosineBetas]]] < 0
    ]
  ];
  assert[
    "cosine options are deterministic and MaxBeta is honored",
    cosineBetaSchedule[10, "Offset" -> 0.01, "MaxBeta" -> 0.9] ===
      cosineBetaSchedule[10, "Offset" -> 0.01, "MaxBeta" -> 0.9] &&
      Max[cosineBetaSchedule[10, "MaxBeta" -> 0.9]] <= 0.9
  ];
  assert[
    "invalid cosine arguments and options fail",
    Quiet[cosineBetaSchedule[0]] === $Failed &&
      Quiet[cosineBetaSchedule[10, "Offset" -> -0.1]] === $Failed &&
      Quiet[cosineBetaSchedule[10, "MaxBeta" -> 1.]] === $Failed &&
      Quiet[cosineBetaSchedule[10, "Unknown" -> 1]] === $Failed
  ];

  schedule = makeDiffusionSchedule[linearBetaSchedule[5, 0.01, 0.05]];
  alphaBars = schedule["AlphaBars"];
  previousAlphaBars = Prepend[Most[alphaBars], 1.];
  assert[
    "schedule contains all canonical arrays at length T",
    schedule["Steps"] === 5 &&
      AllTrue[
        {
          "Betas", "Alphas", "AlphaBars", "SqrtAlphaBars",
          "SqrtOneMinusAlphaBars", "PosteriorVariances",
          "PosteriorMeanCoefficient1", "PosteriorMeanCoefficient2"
        },
        Length[schedule[#]] === 5 &
      ]
  ];
  assert[
    "alphas equal one minus betas",
    numericListsCloseQ[schedule["Alphas"], 1 - schedule["Betas"]]
  ];
  assert[
    "alpha bars equal cumulative alpha products and decrease",
    numericListsCloseQ[
      alphaBars,
      Rest[FoldList[Times, 1., schedule["Alphas"]]]
    ] && And @@ Thread[Differences[alphaBars] < 0]
  ];
  assert[
    "square-root arrays satisfy their defining identities",
    numericListsCloseQ[schedule["SqrtAlphaBars"]^2, alphaBars] &&
      numericListsCloseQ[
        schedule["SqrtOneMinusAlphaBars"]^2,
        1 - alphaBars
      ]
  ];
  assert[
    "posterior variances match the canonical formula",
    numericListsCloseQ[
      schedule["PosteriorVariances"],
      schedule["Betas"] (1 - previousAlphaBars)/(1 - alphaBars)
    ] && First[schedule["PosteriorVariances"]] == 0.
  ];
  assert[
    "posterior mean coefficients match the canonical formulas",
    numericListsCloseQ[
      schedule["PosteriorMeanCoefficient1"],
      schedule["Betas"] Sqrt[previousAlphaBars]/(1 - alphaBars)
    ] && numericListsCloseQ[
      schedule["PosteriorMeanCoefficient2"],
      (1 - previousAlphaBars) Sqrt[schedule["Alphas"]]/(1 - alphaBars)
    ]
  ];
  assert[
    "canonical schedule passes structural coherence validation",
    diffusionScheduleQ[schedule]
  ];
  assert[
    "invalid beta lists and malformed schedules fail",
    Quiet[makeDiffusionSchedule[{}]] === $Failed &&
      Quiet[makeDiffusionSchedule[{0.1, 1.}]] === $Failed &&
      Quiet[makeDiffusionSchedule[{0.1, Indeterminate}]] === $Failed &&
      Quiet[makeDiffusionSchedule[0.1]] === $Failed &&
      !diffusionScheduleQ[ReplacePart[schedule, "Steps" -> 4]]
  ];

  Print["✓ schedules — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "schedules.wl"],
  Stochasma`Private`runSchedulesTests[]
]
