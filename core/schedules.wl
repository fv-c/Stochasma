BeginPackage["Stochasma`"]

linearBetaSchedule::usage =
  "linearBetaSchedule[steps, betaStart, betaEnd] returns steps linearly spaced beta values for logical times 1 through steps. steps must be positive and 0 < betaStart <= betaEnd < 1.";

cosineBetaSchedule::usage =
  "cosineBetaSchedule[steps, opts] returns the cosine DDPM beta schedule for logical times 1 through steps. Options are \"Offset\" (default 0.008) and the documented generated-beta cap \"MaxBeta\" (default 0.999).";

Begin["`Private`"]

finiteRealNumberQ[value_] := Quiet[Check[
  NumberQ[N[value]] &&
    TrueQ[Im[N[value]] == 0] &&
    FreeQ[N[value], Indeterminate | ComplexInfinity | DirectedInfinity],
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

(* === TESTS === *)

runSchedulesTests[] := Module[{passed = 0, assert, betas, cosineBetas},
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

  Print["✓ schedules — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "schedules.wl"],
  Stochasma`Private`runSchedulesTests[]
]
