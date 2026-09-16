If[
  DownValues[Stochasma`makeDiffusionSchedule] === {},
  Get[FileNameJoin[{DirectoryName[$InputFileName], "schedules.wl"}]]
];

BeginPackage["Stochasma`"]

forwardDiffuse::usage =
  "forwardDiffuse[x0, t, schedule] samples q(x_t | x_0) for logical time t, while forwardDiffuse[x0, t, schedule, noise] uses explicit same-shape Gaussian noise deterministically. At t = 0 both forms return x0 exactly.";

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
  noise = BlockRandom[randomNormalLike[x0]];
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

(* === TESTS === *)

runForwardTests[] := Module[
  {passed = 0, assert, schedule, x0, noise, t, expected, samples,
    baseline, withCall},
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ forward/forwardDiffuse: ", label];
    Quit[1]
  ];

  schedule = makeDiffusionSchedule[linearBetaSchedule[8, 0.01, 0.08]];
  x0 = {1., 0., -1.};
  noise = {0.25, -0.5, 1.};
  t = 5;
  assert[
    "t = 0 returns x0 exactly",
    forwardDiffuse[x0, 0, schedule] === x0 &&
      forwardDiffuse[x0, 0, schedule, noise] === x0
  ];
  expected = schedule["SqrtAlphaBars"][[t]] x0;
  assert[
    "zero noise leaves only the scaled clean sample",
    numericListsCloseQ[forwardDiffuse[x0, t, schedule, ConstantArray[0., 3]], expected]
  ];
  expected = schedule["SqrtOneMinusAlphaBars"][[t]] noise;
  assert[
    "zero clean sample leaves only scaled noise",
    numericListsCloseQ[forwardDiffuse[ConstantArray[0., 3], t, schedule, noise], expected]
  ];
  assert[
    "explicit noise is deterministic",
    forwardDiffuse[x0, t, schedule, noise] ===
      forwardDiffuse[x0, t, schedule, noise]
  ];
  samples = {
    1.5,
    {1., 2.},
    {{1., 2.}, {3., 4.}},
    ArrayReshape[N[Range[8]], {2, 2, 2}]
  };
  assert[
    "scalar vector matrix and tensor shapes are preserved",
    AllTrue[
      samples,
      Dimensions[forwardDiffuse[#, 3, schedule, 0. #]] === Dimensions[#] &
    ]
  ];
  assert[
    "closed-form output matches the canonical formula",
    numericListsCloseQ[
      forwardDiffuse[x0, t, schedule, noise],
      schedule["SqrtAlphaBars"][[t]] x0 +
        schedule["SqrtOneMinusAlphaBars"][[t]] noise
    ]
  ];
  baseline = BlockRandom[SeedRandom[1234]; {RandomReal[], RandomReal[]}];
  withCall = BlockRandom[
    SeedRandom[1234];
    {RandomReal[], forwardDiffuse[x0, t, schedule]; RandomReal[]}
  ];
  assert[
    "generated noise does not advance the caller random state",
    baseline === withCall
  ];
  assert[
    "invalid time boundaries fail",
    Quiet[forwardDiffuse[x0, -1, schedule, noise]] === $Failed &&
      Quiet[forwardDiffuse[x0, 9, schedule, noise]] === $Failed
  ];
  assert[
    "invalid samples and noise shape fail",
    Quiet[forwardDiffuse[{1., Infinity}, 2, schedule, {0., 0.}]] === $Failed &&
      Quiet[forwardDiffuse[x0, 2, schedule, {0., 0.}]] === $Failed
  ];
  assert[
    "malformed schedule fails",
    Quiet[
      forwardDiffuse[x0, 2, ReplacePart[schedule, "Steps" -> 7], noise]
    ] === $Failed
  ];

  Print["✓ forward — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "forward.wl"],
  Stochasma`Private`runForwardTests[]
]
