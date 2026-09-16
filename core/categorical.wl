BeginPackage["Stochasma`"]

uniformCategoricalTransitionKernel::usage =
  "uniformCategoricalTransitionKernel[categoryCount, beta] returns the row-stochastic transition matrix (1 - beta) I + beta U for uniform categorical corruption over categoryCount states, where U has every entry 1/categoryCount and 0 <= beta <= 1.";

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
    randomSample, expectedNoise
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
