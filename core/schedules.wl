BeginPackage["Stochasma`"]

linearBetaSchedule::usage =
  "linearBetaSchedule[steps, betaStart, betaEnd] returns steps linearly spaced beta values for logical times 1 through steps. steps must be positive and 0 < betaStart <= betaEnd < 1.";

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

(* === TESTS === *)

runSchedulesTests[] := Module[{passed = 0, assert, betas},
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

  Print["✓ schedules — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "schedules.wl"],
  Stochasma`Private`runSchedulesTests[]
]
