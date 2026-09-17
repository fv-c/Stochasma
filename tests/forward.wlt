Begin["Stochasma`Private`"]

runForwardTests[] := Module[
  {passed = 0, assert, schedule, x0, noise, t, expected, samples,
    stochasticInput, firstDraw, secondDraw, replay1, replay2,
    expectedNext, actualNext},
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
  assert[
    "explicit noise does not consult the current random stream",
    BlockRandom[
      SeedRandom[4321];
      forwardDiffuse[x0, t, schedule, noise];
      RandomReal[]
    ] === BlockRandom[SeedRandom[4321]; RandomReal[]]
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
  stochasticInput = ConstantArray[0., 64];
  {firstDraw, secondDraw} = BlockRandom[
    SeedRandom[1234];
    {
      forwardDiffuse[stochasticInput, t, schedule],
      forwardDiffuse[stochasticInput, t, schedule]
    }
  ];
  assert[
    "consecutive stochastic calls consume the current random stream",
    firstDraw =!= secondDraw
  ];
  replay1 = BlockRandom[
    SeedRandom[1234];
    forwardDiffuse[stochasticInput, t, schedule]
  ];
  replay2 = BlockRandom[
    SeedRandom[1234];
    forwardDiffuse[stochasticInput, t, schedule]
  ];
  assert[
    "resetting the current random stream reproduces generated noise",
    replay1 === replay2
  ];
  expectedNext = BlockRandom[
    SeedRandom[1234];
    randomNormalLike[stochasticInput];
    RandomReal[]
  ];
  actualNext = BlockRandom[
    SeedRandom[1234];
    forwardDiffuse[stochasticInput, t, schedule];
    RandomReal[]
  ];
  assert[
    "stochastic diffusion advances the stream by one same-shape draw",
    actualNext === expectedNext
  ];
  assert[
    "t = 0 does not consume the current random stream",
    BlockRandom[
      SeedRandom[1234];
      forwardDiffuse[stochasticInput, 0, schedule];
      RandomReal[]
    ] === BlockRandom[SeedRandom[1234]; RandomReal[]]
  ];
  assert[
    "invalid time boundaries fail",
    Quiet[forwardDiffuse[x0, -1, schedule, noise]] === $Failed &&
      Quiet[forwardDiffuse[x0, 9, schedule, noise]] === $Failed &&
      Quiet[forwardDiffuse[x0, 2., schedule, noise]] === $Failed
  ];
  assert[
    "invalid samples, noise values, and noise shape fail",
    Quiet[forwardDiffuse[{1., Infinity}, 2, schedule, {0., 0.}]] === $Failed &&
      Quiet[forwardDiffuse[x0, 2, schedule, {0., Infinity, 0.}]] === $Failed &&
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
