BeginPackage["Stochasma`"]

uniformCategoricalTransitionKernel::usage =
  "uniformCategoricalTransitionKernel[categoryCount, beta] returns the row-stochastic transition matrix (1 - beta) I + beta U for uniform categorical corruption over categoryCount states, where U has every entry 1/categoryCount and 0 <= beta <= 1.";

makeCategoricalSchedule::usage =
  "makeCategoricalSchedule[{Q1, Q2, ..., QT}] validates one-step row-stochastic categorical transition matrices Q_t from logical time t - 1 to t and returns them together with cumulative row-vector transition matrices Qbar_t = Q1 . Q2 . ... . Qt for logical times 1 through T.";

makeUniformCategoricalSchedule::usage =
  "makeUniformCategoricalSchedule[categoryCount, betas] constructs one uniform categorical transition kernel for each finite beta in the inclusive range 0 through 1 and returns the validated categorical schedule.";

categoricalForwardDiffuse::usage =
  "categoricalForwardDiffuse[state, transitionKernel] samples a categorical state or array of states through a row-stochastic transition kernel. categoricalForwardDiffuse[state, transitionKernel, uniformNoise] uses explicit uniform variates in the half-open interval [0, 1) and is deterministic.";

Begin["`Private`"]

categoricalFiniteRealNumberQ[value_] := Quiet[Check[
  NumberQ[N[value]] &&
    TrueQ[Im[N[value]] == 0] &&
    FreeQ[N[value], Indeterminate | ComplexInfinity | DirectedInfinity],
  False
]];

uniformCategoricalTransitionKernel::args =
  "uniformCategoricalTransitionKernel expects an Integer category count of at least 2 and a finite real beta value.";
uniformCategoricalTransitionKernel::range =
  "Beta must satisfy 0 <= beta <= 1.";

uniformCategoricalTransitionKernel[categoryCount_, beta_] := Module[
  {numericBeta, uniformKernel},
  If[
    !IntegerQ[categoryCount] || categoryCount < 2 ||
      !categoricalFiniteRealNumberQ[beta],
    Message[uniformCategoricalTransitionKernel::args];
    Return[$Failed]
  ];
  If[!TrueQ[0 <= beta <= 1],
    Message[uniformCategoricalTransitionKernel::range];
    Return[$Failed]
  ];
  numericBeta = N[beta];
  uniformKernel = ConstantArray[
    1./categoryCount,
    {categoryCount, categoryCount}
  ];
  (1 - numericBeta) IdentityMatrix[categoryCount] +
    numericBeta uniformKernel
];

uniformCategoricalTransitionKernel[___] := (
  Message[uniformCategoricalTransitionKernel::args];
  $Failed
);

categoricalForwardDiffuse::args =
  "categoricalForwardDiffuse expects a categorical state or rectangular array of states, a square row-stochastic transition kernel, and optionally matching uniform noise.";
categoricalForwardDiffuse::kernel =
  "The transition kernel must be a finite, non-negative, square numeric matrix of size at least 2 whose rows sum to 1.";
categoricalForwardDiffuse::state =
  "Every state must be an Integer between 1 and the transition kernel's category count.";
categoricalForwardDiffuse::noise =
  "Explicit uniform noise must match the state shape and contain only finite real values in the half-open interval [0, 1).";

categoricalTransitionKernelCategoryCount[kernel_] := Module[
  {dimensions, numericKernel},
  If[!MatrixQ[kernel, categoricalFiniteRealNumberQ], Return[$Failed]];
  dimensions = Dimensions[kernel];
  If[
    Length[dimensions] != 2 || dimensions[[1]] < 2 ||
      dimensions[[1]] != dimensions[[2]],
    Return[$Failed]
  ];
  numericKernel = N[kernel];
  If[
    !AllTrue[Flatten[numericKernel], TrueQ[# >= 0] &] ||
      Max[Abs[Total[numericKernel, {2}] - 1.]] > 10^-12,
    Return[$Failed]
  ];
  dimensions[[1]]
];

categoricalNumericArraysCloseQ[
  left_List,
  right_List,
  tolerance_ : 10^-10
] := Quiet[Check[
  Dimensions[left] === Dimensions[right] &&
    Max[Abs[Flatten[N[left - right]]]] <= tolerance,
  False
]];

categoricalScheduleQ[schedule_Association] := Module[
  {
    requiredKeys, steps, categoryCount, transitionKernels,
    cumulativeTransitionKernels, expectedCumulative
  },
  requiredKeys = {
    "Steps", "CategoryCount", "TransitionKernels",
    "CumulativeTransitionKernels"
  };
  If[!AllTrue[requiredKeys, KeyExistsQ[schedule, #] &], Return[False]];
  steps = schedule["Steps"];
  categoryCount = schedule["CategoryCount"];
  transitionKernels = schedule["TransitionKernels"];
  cumulativeTransitionKernels = schedule["CumulativeTransitionKernels"];
  If[
    !IntegerQ[steps] || steps <= 0 ||
      !IntegerQ[categoryCount] || categoryCount < 2 ||
      !ListQ[transitionKernels] || Length[transitionKernels] =!= steps ||
      !ListQ[cumulativeTransitionKernels] ||
      Length[cumulativeTransitionKernels] =!= steps,
    Return[False]
  ];
  If[
    !AllTrue[
      Join[transitionKernels, cumulativeTransitionKernels],
      categoricalTransitionKernelCategoryCount[#] === categoryCount &
    ],
    Return[False]
  ];
  expectedCumulative = Rest@FoldList[
    Dot,
    N[IdentityMatrix[categoryCount]],
    N[transitionKernels]
  ];
  categoricalNumericArraysCloseQ[
    cumulativeTransitionKernels,
    expectedCumulative
  ]
];

categoricalScheduleQ[_] := False;

makeCategoricalSchedule::kernels =
  "Transition kernels must be a non-empty list of finite, non-negative, square numeric matrices of the same size K by K, with K at least 2 and every row summing numerically to 1.";
makeCategoricalSchedule::numeric =
  "The supplied transition kernels cannot produce a finite, internally consistent categorical schedule after numerical evaluation.";

makeCategoricalSchedule[transitionKernels_List] := Module[
  {categoryCounts, categoryCount, numericKernels, cumulativeKernels, schedule},
  If[transitionKernels === {},
    Message[makeCategoricalSchedule::kernels];
    Return[$Failed]
  ];
  categoryCounts = categoricalTransitionKernelCategoryCount /@
    transitionKernels;
  If[
    MemberQ[categoryCounts, $Failed] ||
      !SameQ @@ categoryCounts,
    Message[makeCategoricalSchedule::kernels];
    Return[$Failed]
  ];
  categoryCount = First[categoryCounts];
  numericKernels = N[transitionKernels];
  cumulativeKernels = Rest@FoldList[
    Dot,
    N[IdentityMatrix[categoryCount]],
    numericKernels
  ];
  schedule = <|
    "Steps" -> Length[numericKernels],
    "CategoryCount" -> categoryCount,
    "TransitionKernels" -> numericKernels,
    "CumulativeTransitionKernels" -> cumulativeKernels
  |>;
  If[!categoricalScheduleQ[schedule],
    Message[makeCategoricalSchedule::numeric];
    Return[$Failed]
  ];
  schedule
];

makeCategoricalSchedule[___] := (
  Message[makeCategoricalSchedule::kernels];
  $Failed
);

makeUniformCategoricalSchedule::args =
  "makeUniformCategoricalSchedule expects an Integer category count of at least 2 and a non-empty list of finite real beta values.";
makeUniformCategoricalSchedule::range =
  "Every beta must satisfy 0 <= beta <= 1.";

makeUniformCategoricalSchedule[categoryCount_, betas_List] := Module[{},
  If[
    !IntegerQ[categoryCount] || categoryCount < 2 || betas === {} ||
      !AllTrue[betas, categoricalFiniteRealNumberQ],
    Message[makeUniformCategoricalSchedule::args];
    Return[$Failed]
  ];
  If[!AllTrue[betas, TrueQ[0 <= # <= 1] &],
    Message[makeUniformCategoricalSchedule::range];
    Return[$Failed]
  ];
  makeCategoricalSchedule[
    uniformCategoricalTransitionKernel[categoryCount, #] & /@ betas
  ]
];

makeUniformCategoricalSchedule[___] := (
  Message[makeUniformCategoricalSchedule::args];
  $Failed
);

categoricalStateQ[state_, categoryCount_] := If[
  IntegerQ[state],
  1 <= state <= categoryCount,
  ArrayQ[state, _, IntegerQ] && Length[Flatten[state]] > 0 &&
    AllTrue[Flatten[state], 1 <= # <= categoryCount &]
];

categoricalStateDimensions[state_] := If[IntegerQ[state], {}, Dimensions[state]];

categoricalUniformNoiseQ[noise_, dimensions_] := If[
  dimensions === {},
  categoricalFiniteRealNumberQ[noise] && TrueQ[0 <= N[noise] < 1],
  ArrayQ[noise, Length[dimensions], categoricalFiniteRealNumberQ] &&
    Dimensions[noise] === dimensions &&
    AllTrue[Flatten[N[noise]], TrueQ[0 <= # < 1] &]
];

categoricalSampleWithUniformNoise[state_, kernel_, noise_] := Module[
  {dimensions, states, uniforms, samples},
  dimensions = categoricalStateDimensions[state];
  states = Flatten[{state}];
  uniforms = Flatten[{noise}];
  samples = MapThread[
    Function[{category, uniform},
      With[{cumulative = Accumulate[kernel[[category]]]},
        First@FirstPosition[
          cumulative,
          threshold_ /; uniform < threshold,
          {Length[cumulative]}
        ]
      ]
    ],
    {states, uniforms}
  ];
  If[dimensions === {}, First[samples], ArrayReshape[samples, dimensions]]
];

categoricalForwardDiffuse[state_, transitionKernel_List] := Module[
  {categoryCount, dimensions, noise},
  categoryCount = categoricalTransitionKernelCategoryCount[transitionKernel];
  If[categoryCount === $Failed,
    Message[categoricalForwardDiffuse::kernel];
    Return[$Failed]
  ];
  If[!TrueQ[categoricalStateQ[state, categoryCount]],
    Message[categoricalForwardDiffuse::state];
    Return[$Failed]
  ];
  dimensions = categoricalStateDimensions[state];
  noise = If[
    dimensions === {},
    RandomReal[],
    RandomReal[{0, 1}, dimensions]
  ];
  categoricalSampleWithUniformNoise[
    state,
    N[transitionKernel],
    noise
  ]
];

categoricalForwardDiffuse[state_, transitionKernel_List, uniformNoise_] := Module[
  {categoryCount, dimensions},
  categoryCount = categoricalTransitionKernelCategoryCount[transitionKernel];
  If[categoryCount === $Failed,
    Message[categoricalForwardDiffuse::kernel];
    Return[$Failed]
  ];
  If[!TrueQ[categoricalStateQ[state, categoryCount]],
    Message[categoricalForwardDiffuse::state];
    Return[$Failed]
  ];
  dimensions = categoricalStateDimensions[state];
  If[!TrueQ[categoricalUniformNoiseQ[uniformNoise, dimensions]],
    Message[categoricalForwardDiffuse::noise];
    Return[$Failed]
  ];
  categoricalSampleWithUniformNoise[
    state,
    N[transitionKernel],
    N[uniformNoise]
  ]
];

categoricalForwardDiffuse[___] := (
  Message[categoricalForwardDiffuse::args];
  $Failed
);

(* === TESTS === *)

runCategoricalTests[] := Module[
  {
    passed = 0, assert, kernel, expected, permutation, states,
    randomSample, expectedNoise, q1, q2, q3, schedule, uniformBetas,
    uniformSchedule
  },
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ categorical: ", label];
    Quit[1]
  ];

  kernel = uniformCategoricalTransitionKernel[4, 0.2];
  expected = ConstantArray[0.05, {4, 4}] + 0.8 IdentityMatrix[4];
  assert[
    "matches the uniform-corruption formula",
    Max[Abs[Flatten[kernel - expected]]] < 10^-14
  ];
  assert[
    "is row-stochastic and non-negative",
    Max[Abs[Total[kernel, {2}] - ConstantArray[1., 4]]] < 10^-14 &&
      Min[kernel] >= 0
  ];
  assert[
    "preserves the uniform distribution",
    Max[Abs[ConstantArray[1./4, 4] . kernel - ConstantArray[1./4, 4]]] <
      10^-14
  ];
  assert[
    "has identity and uniform endpoint kernels",
    uniformCategoricalTransitionKernel[3, 0] === N[IdentityMatrix[3]] &&
      uniformCategoricalTransitionKernel[3, 1] ===
        ConstantArray[1./3, {3, 3}]
  ];
  permutation = IdentityMatrix[4][[{3, 1, 4, 2}]];
  assert[
    "is invariant under category relabeling",
    Max[Abs[Flatten[
      permutation . kernel . Transpose[permutation] - kernel
    ]]] < 10^-14
  ];
  assert[
    "rejects invalid category counts, beta values, and arity",
    Quiet[uniformCategoricalTransitionKernel[1, 0.2]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3.5, 0.2]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3, -0.1]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3, 1.1]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3, Infinity]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3]] === $Failed
  ];

  q1 = {{0.9, 0.1}, {0.2, 0.8}};
  q2 = {{0.6, 0.4}, {0.1, 0.9}};
  q3 = {{0.7, 0.3}, {0.4, 0.6}};
  schedule = makeCategoricalSchedule[{q1, q2, q3}];
  assert[
    "constructs the categorical schedule structure",
    schedule["Steps"] === 3 &&
      schedule["CategoryCount"] === 2 &&
      Length[schedule["TransitionKernels"]] === 3 &&
      Length[schedule["CumulativeTransitionKernels"]] === 3
  ];
  assert[
    "uses ordered non-commutative cumulative products",
    !categoricalNumericArraysCloseQ[q1 . q2, q2 . q1] &&
      categoricalNumericArraysCloseQ[
        schedule["CumulativeTransitionKernels"][[2]],
        q1 . q2
      ] &&
      categoricalNumericArraysCloseQ[
        schedule["CumulativeTransitionKernels"][[3]],
        q1 . q2 . q3
      ]
  ];
  assert[
    "keeps every cumulative transition row-stochastic",
    AllTrue[
      schedule["CumulativeTransitionKernels"],
      categoricalTransitionKernelCategoryCount[#] === 2 &
    ]
  ];
  assert[
    "validates the complete categorical schedule representation",
    categoricalScheduleQ[schedule] &&
      !categoricalScheduleQ[
        ReplacePart[
          schedule,
          "CumulativeTransitionKernels" -> {q1, q2 . q1, q3 . q2 . q1}
        ]
      ] &&
      !categoricalScheduleQ[KeyDrop[schedule, "CategoryCount"]]
  ];
  assert[
    "rejects invalid categorical schedule inputs",
    Quiet[makeCategoricalSchedule[{}]] === $Failed &&
      Quiet[makeCategoricalSchedule[{{{1., 0., 0.}, {0., 1., 0.}}}]] ===
        $Failed &&
      Quiet[makeCategoricalSchedule[{IdentityMatrix[2], IdentityMatrix[3]}]] ===
        $Failed &&
      Quiet[makeCategoricalSchedule[{{{1.1, -0.1}, {0., 1.}}}]] ===
        $Failed &&
      Quiet[makeCategoricalSchedule[{{{1., 0.}, {0., Infinity}}}]] ===
        $Failed &&
      Quiet[makeCategoricalSchedule[{{{1., 0.}, {0., Indeterminate}}}]] ===
        $Failed &&
      Quiet[makeCategoricalSchedule[{{{1., 0.}, {0.2, 0.7}}}]] ===
        $Failed &&
      Quiet[makeCategoricalSchedule[IdentityMatrix[2]]] === $Failed
  ];

  uniformBetas = {0., 0.2, 0.5, 1.};
  uniformSchedule = makeUniformCategoricalSchedule[3, uniformBetas];
  assert[
    "constructs uniform transition kernels from every beta",
    uniformSchedule["Steps"] === Length[uniformBetas] &&
      uniformSchedule["CategoryCount"] === 3 &&
      And @@ MapThread[
        categoricalNumericArraysCloseQ,
        {
          uniformSchedule["TransitionKernels"],
          uniformCategoricalTransitionKernel[3, #] & /@ uniformBetas
        }
      ]
  ];
  assert[
    "uniform schedule includes identity and uniform endpoints",
    uniformSchedule["TransitionKernels"][[1]] ===
      N[IdentityMatrix[3]] &&
      uniformSchedule["TransitionKernels"][[-1]] ===
        ConstantArray[1./3, {3, 3}]
  ];
  assert[
    "rejects invalid uniform categorical schedule inputs",
    Quiet[makeUniformCategoricalSchedule[1, {0.2}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3., {0.2}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3, {}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3, {-0.1}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3, {1.1}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3, {Infinity}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3, {Indeterminate}]] === $Failed &&
      Quiet[makeUniformCategoricalSchedule[3, 0.2]] === $Failed
  ];

  kernel = uniformCategoricalTransitionKernel[3, 0.6];
  assert[
    "samples the transition row selected by each state",
    categoricalForwardDiffuse[{1, 2, 3}, kernel, {0.59, 0.19, 0.41}] ===
      {1, 1, 3}
  ];
  assert[
    "preserves scalar states and array shape",
    categoricalForwardDiffuse[2, IdentityMatrix[3], 0.25] === 2 &&
      Dimensions[categoricalForwardDiffuse[
        {{1, 2}, {3, 1}},
        kernel,
        {{0.1, 0.3}, {0.5, 0.9}}
      ]] === {2, 2} &&
      Dimensions[categoricalForwardDiffuse[
        {{{1, 2}, {3, 1}}, {{2, 3}, {1, 2}}},
        kernel,
        ConstantArray[0.5, {2, 2, 2}]
      ]] === {2, 2, 2}
  ];
  assert[
    "maps explicit quantiles through a fully uniform kernel",
    categoricalForwardDiffuse[
      {3, 1, 2},
      uniformCategoricalTransitionKernel[3, 1],
      {0., 0.34, 0.999}
    ] === {1, 2, 3}
  ];
  states = {{1, 2}, {3, 1}};
  randomSample = BlockRandom[
    SeedRandom[8128];
    categoricalForwardDiffuse[states, kernel]
  ];
  expectedNoise = BlockRandom[
    SeedRandom[8128];
    RandomReal[{0, 1}, Dimensions[states]]
  ];
  assert[
    "uses the caller's random stream",
    randomSample === categoricalForwardDiffuse[states, kernel, expectedNoise]
  ];
  assert[
    "rejects invalid categorical states",
    Quiet[categoricalForwardDiffuse[0, kernel, 0.5]] === $Failed &&
      Quiet[categoricalForwardDiffuse[4, kernel, 0.5]] === $Failed &&
      Quiet[categoricalForwardDiffuse[{1, 2.0}, kernel, {0.1, 0.2}]] ===
        $Failed &&
      Quiet[categoricalForwardDiffuse[{}, kernel, {}]] === $Failed
  ];
  assert[
    "rejects malformed transition kernels",
    Quiet[categoricalForwardDiffuse[1, {{1., 0.}, {0.2, 0.7}}, 0.5]] ===
      $Failed &&
      Quiet[categoricalForwardDiffuse[1, {{1., 0.}, {-0.1, 1.1}}, 0.5]] ===
        $Failed &&
      Quiet[categoricalForwardDiffuse[1, {{1., 0., 0.}, {0., 1., 0.}}, 0.5]] ===
        $Failed &&
      Quiet[categoricalForwardDiffuse[1, {{1., 0.}, {0., Infinity}}, 0.5]] ===
        $Failed
  ];
  assert[
    "rejects malformed explicit noise and arity",
    Quiet[categoricalForwardDiffuse[{1, 2}, kernel, 0.5]] === $Failed &&
      Quiet[categoricalForwardDiffuse[{1, 2}, kernel, {0.2}]] === $Failed &&
      Quiet[categoricalForwardDiffuse[1, kernel, -0.1]] === $Failed &&
      Quiet[categoricalForwardDiffuse[1, kernel, 1.]] === $Failed &&
      Quiet[categoricalForwardDiffuse[1]] === $Failed
  ];

  Print["✓ categorical — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "categorical.wl"],
  Stochasma`Private`runCategoricalTests[]
]
