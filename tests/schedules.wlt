Begin["Stochasma`Private`"]

runSchedulesTests[] := Module[
  {passed = 0, assert, betas, cosineBetas, schedule, alphaBars,
    previousAlphaBars, standardSchedule, pathologicalResult,
    numericMessage},
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
      Quiet[cosineBetaSchedule[10, "Unknown" -> 1]] === $Failed &&
      Quiet[
        cosineBetaSchedule[10, "Offset" -> 0.01, "Offset" -> 0.02]
      ] === $Failed
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
  standardSchedule = makeDiffusionSchedule[
    linearBetaSchedule[1000, 10^-4, 0.02]
  ];
  assert[
    "standard 1000-step schedule remains numerically valid",
    AssociationQ[standardSchedule] &&
      standardSchedule["Steps"] === 1000 &&
      diffusionScheduleQ[standardSchedule]
  ];
  pathologicalResult = Quiet[
    makeDiffusionSchedule[{10^-100}],
    makeDiffusionSchedule::numeric
  ];
  assert[
    "machine-degenerate beta schedule fails instead of returning coefficients",
    pathologicalResult === $Failed
  ];
  numericMessage = Quiet[
    Check[
      makeDiffusionSchedule[{10^-100}],
      "numeric-message",
      makeDiffusionSchedule::numeric
    ],
    makeDiffusionSchedule::numeric
  ];
  assert[
    "machine-degenerate beta schedule emits the specific numeric message",
    numericMessage === "numeric-message"
  ];
  assert[
    "invalid beta lists and malformed schedules fail",
    Quiet[makeDiffusionSchedule[{}]] === $Failed &&
      Quiet[makeDiffusionSchedule[{0.1, 1.}]] === $Failed &&
      Quiet[makeDiffusionSchedule[{0.1, Indeterminate}]] === $Failed &&
      Quiet[makeDiffusionSchedule[{0.1, Infinity}]] === $Failed &&
      Quiet[makeDiffusionSchedule[{0.1, symbolicBeta}]] === $Failed &&
      Quiet[makeDiffusionSchedule[0.1]] === $Failed &&
      !diffusionScheduleQ[ReplacePart[schedule, "Steps" -> 4]]
  ];

  Print["✓ schedules — ", passed, " tests passed"];
  passed
];

End[]
