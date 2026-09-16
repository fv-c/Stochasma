BeginPackage["Stochasma`"]

uniformCategoricalTransitionKernel::usage =
  "uniformCategoricalTransitionKernel[categoryCount, beta] returns the row-stochastic transition matrix (1 - beta) I + beta U for uniform categorical corruption over categoryCount states, where U has every entry 1/categoryCount and 0 <= beta <= 1.";

makeCategoricalSchedule::usage =
  "makeCategoricalSchedule[{Q1, Q2, ..., QT}] validates one-step row-stochastic categorical transition matrices Q_t from logical time t - 1 to t and returns them together with cumulative row-vector transition matrices Qbar_t = Q1 . Q2 . ... . Qt for logical times 1 through T.";

makeUniformCategoricalSchedule::usage =
  "makeUniformCategoricalSchedule[categoryCount, betas] constructs one uniform categorical transition kernel for each finite beta in the inclusive range 0 through 1 and returns the validated categorical schedule.";

categoricalForwardDiffuse::usage =
  "categoricalForwardDiffuse[state, transitionKernel] samples a categorical state or array of states through any valid row-stochastic transition matrix, including a one-step Q_t or cumulative Qbar_t, without resolving logical time. categoricalForwardDiffuse[state, transitionKernel, uniformNoise] uses explicit uniform variates in the half-open interval [0, 1) and is deterministic.";

categoricalForwardDiffuseAt::usage =
  "categoricalForwardDiffuseAt[x0, t, schedule] samples q(x_t | x_0) at logical time t by applying the cumulative row-vector transition Qbar_t stored in a categorical schedule. categoricalForwardDiffuseAt[x0, t, schedule, uniformNoise] uses explicit uniform variates deterministically. At t = 0 both forms return x0 exactly without generating noise; the explicit form still validates its noise.";

categoricalPosterior::usage =
  "categoricalPosterior[x0, xt, t, schedule] returns the exact probability vector q(x_(t-1) | x_t, x_0) for scalar categorical states at logical time t.";

categoricalReverseProbabilities::usage =
  "categoricalReverseProbabilities[xt, t, predictedX0Probabilities, schedule] constructs the canonical predicted-x0 categorical reverse distribution by combining the model-predicted clean-state probabilities with the cumulative and one-step transition kernels and normalizing once.";

categoricalReverseStep::usage =
  "categoricalReverseStep[xt, t, predictedX0Probabilities, schedule] samples x_(t-1) from the predicted-x0 categorical reverse probabilities using the current random stream. categoricalReverseStep[xt, t, predictedX0Probabilities, schedule, uniformNoise] uses an explicit uniform variate in the half-open interval [0, 1) and is deterministic.";

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

categoricalProbabilityVectorQ[
  probabilities_,
  categoryCount_,
  tolerance_ : 10^-12
] := ListQ[probabilities] &&
  Length[probabilities] === categoryCount &&
  VectorQ[probabilities, categoricalFiniteRealNumberQ] &&
  AllTrue[N[probabilities], TrueQ[# >= 0] &] &&
  Abs[Total[N[probabilities]] - 1.] <= tolerance;

categoricalSampleProbabilityVector[probabilities_, uniformNoise_] := With[
  {cumulative = Accumulate[N[probabilities]]},
  First@FirstPosition[
    cumulative,
    threshold_ /; N[uniformNoise] < threshold,
    {Length[cumulative]}
  ]
];

categoricalSampleWithUniformNoise[state_, kernel_, noise_] := Module[
  {dimensions, states, uniforms, samples},
  dimensions = categoricalStateDimensions[state];
  states = Flatten[{state}];
  uniforms = Flatten[{noise}];
  samples = MapThread[
    Function[{category, uniform},
      categoricalSampleProbabilityVector[kernel[[category]], uniform]
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

categoricalForwardDiffuseAt::args =
  "categoricalForwardDiffuseAt expects a categorical state or rectangular array of states, an Integer time, a categorical schedule, and optional matching uniform noise.";
categoricalForwardDiffuseAt::schedule =
  "The supplied categorical schedule is malformed or internally inconsistent.";
categoricalForwardDiffuseAt::state =
  "Every state must be an Integer between 1 and the categorical schedule's category count.";
categoricalForwardDiffuseAt::time =
  "Time t must be an Integer in the inclusive range 0 through `1`.";
categoricalForwardDiffuseAt::noise =
  "Explicit uniform noise must match the state shape and contain only finite real values in the half-open interval [0, 1).";

categoricalForwardDiffuseAt[x0_, t_Integer, schedule_Association] := Module[
  {categoryCount},
  If[!categoricalScheduleQ[schedule],
    Message[categoricalForwardDiffuseAt::schedule];
    Return[$Failed]
  ];
  categoryCount = schedule["CategoryCount"];
  If[!TrueQ[categoricalStateQ[x0, categoryCount]],
    Message[categoricalForwardDiffuseAt::state];
    Return[$Failed]
  ];
  If[!TrueQ[0 <= t <= schedule["Steps"]],
    Message[categoricalForwardDiffuseAt::time, schedule["Steps"]];
    Return[$Failed]
  ];
  If[t == 0, Return[x0]];
  categoricalForwardDiffuse[
    x0,
    schedule["CumulativeTransitionKernels"][[t]]
  ]
];

categoricalForwardDiffuseAt[
  x0_,
  t_Integer,
  schedule_Association,
  uniformNoise_
] := Module[{categoryCount, dimensions},
  If[!categoricalScheduleQ[schedule],
    Message[categoricalForwardDiffuseAt::schedule];
    Return[$Failed]
  ];
  categoryCount = schedule["CategoryCount"];
  If[!TrueQ[categoricalStateQ[x0, categoryCount]],
    Message[categoricalForwardDiffuseAt::state];
    Return[$Failed]
  ];
  If[!TrueQ[0 <= t <= schedule["Steps"]],
    Message[categoricalForwardDiffuseAt::time, schedule["Steps"]];
    Return[$Failed]
  ];
  dimensions = categoricalStateDimensions[x0];
  If[!TrueQ[categoricalUniformNoiseQ[uniformNoise, dimensions]],
    Message[categoricalForwardDiffuseAt::noise];
    Return[$Failed]
  ];
  If[t == 0, Return[x0]];
  categoricalForwardDiffuse[
    x0,
    schedule["CumulativeTransitionKernels"][[t]],
    uniformNoise
  ]
];

categoricalForwardDiffuseAt[___] := (
  Message[categoricalForwardDiffuseAt::args];
  $Failed
);

categoricalPosterior::args =
  "categoricalPosterior expects Integer scalar states x0 and xt, an Integer time, and a categorical schedule.";
categoricalPosterior::schedule =
  "The supplied categorical schedule is malformed or internally inconsistent.";
categoricalPosterior::time =
  "Time t must be an Integer in the inclusive range 1 through `1`.";
categoricalPosterior::x0 =
  "State x0 must be an Integer between 1 and the categorical schedule's category count.";
categoricalPosterior::xt =
  "State xt must be an Integer between 1 and the categorical schedule's category count.";
categoricalPosterior::normalization =
  "The categorical posterior has zero or non-finite normalization for the supplied states and time.";

categoricalPosterior[
  x0_,
  xt_,
  t_Integer,
  schedule_Association
] := Module[
  {
    categoryCount, priorPrevious, likelihood, unnormalized,
    normalization, posterior
  },
  If[!categoricalScheduleQ[schedule],
    Message[categoricalPosterior::schedule];
    Return[$Failed]
  ];
  If[!TrueQ[1 <= t <= schedule["Steps"]],
    Message[categoricalPosterior::time, schedule["Steps"]];
    Return[$Failed]
  ];
  categoryCount = schedule["CategoryCount"];
  If[!IntegerQ[x0] || !TrueQ[1 <= x0 <= categoryCount],
    Message[categoricalPosterior::x0];
    Return[$Failed]
  ];
  If[!IntegerQ[xt] || !TrueQ[1 <= xt <= categoryCount],
    Message[categoricalPosterior::xt];
    Return[$Failed]
  ];
  priorPrevious = If[
    t == 1,
    N[IdentityMatrix[categoryCount]][[x0]],
    schedule["CumulativeTransitionKernels"][[t - 1, x0]]
  ];
  likelihood = schedule["TransitionKernels"][[t, All, xt]];
  unnormalized = N[priorPrevious likelihood];
  normalization = Total[unnormalized];
  If[
    !categoricalFiniteRealNumberQ[normalization] ||
      !TrueQ[normalization > 0],
    Message[categoricalPosterior::normalization];
    Return[$Failed]
  ];
  posterior = N[unnormalized/normalization];
  If[
    !VectorQ[posterior, categoricalFiniteRealNumberQ] ||
      !AllTrue[posterior, TrueQ[# >= 0] &],
    Message[categoricalPosterior::normalization];
    Return[$Failed]
  ];
  posterior
];

categoricalPosterior[___] := (
  Message[categoricalPosterior::args];
  $Failed
);

categoricalReverseProbabilities::args =
  "categoricalReverseProbabilities expects an Integer scalar state xt, an Integer time, a length-K list of predicted x0 probabilities, and a categorical schedule.";
categoricalReverseProbabilities::schedule =
  "The supplied categorical schedule is malformed or internally inconsistent.";
categoricalReverseProbabilities::time =
  "Time t must be an Integer in the inclusive range 1 through `1`.";
categoricalReverseProbabilities::xt =
  "State xt must be an Integer between 1 and the categorical schedule's category count.";
categoricalReverseProbabilities::probabilities =
  "Predicted x0 probabilities must be a length-K list of finite, non-negative values whose sum is numerically 1.";
categoricalReverseProbabilities::posterior =
  "The joint-marginalized reverse weights must be a finite, non-negative length-K vector with positive finite normalization.";

categoricalReverseProbabilities[
  xt_,
  t_Integer,
  predictedX0Probabilities_List,
  schedule_Association
] := Module[
  {
    categoryCount, numericPredicted, priorPrevious, likelihood,
    unnormalized, normalization, reverseProbabilities
  },
  If[!categoricalScheduleQ[schedule],
    Message[categoricalReverseProbabilities::schedule];
    Return[$Failed]
  ];
  If[!TrueQ[1 <= t <= schedule["Steps"]],
    Message[categoricalReverseProbabilities::time, schedule["Steps"]];
    Return[$Failed]
  ];
  categoryCount = schedule["CategoryCount"];
  If[!IntegerQ[xt] || !TrueQ[1 <= xt <= categoryCount],
    Message[categoricalReverseProbabilities::xt];
    Return[$Failed]
  ];
  If[
    !categoricalProbabilityVectorQ[
      predictedX0Probabilities,
      categoryCount
    ],
    Message[categoricalReverseProbabilities::probabilities];
    Return[$Failed]
  ];
  numericPredicted = N[predictedX0Probabilities];
  priorPrevious = If[
    t == 1,
    numericPredicted,
    numericPredicted .
      schedule["CumulativeTransitionKernels"][[t - 1]]
  ];
  likelihood = schedule["TransitionKernels"][[t, All, xt]];
  unnormalized = N[priorPrevious likelihood];
  If[
    !ListQ[unnormalized] || Length[unnormalized] =!= categoryCount ||
      !VectorQ[unnormalized, categoricalFiniteRealNumberQ] ||
      !AllTrue[unnormalized, TrueQ[# >= 0] &],
    Message[categoricalReverseProbabilities::posterior];
    Return[$Failed]
  ];
  normalization = Total[unnormalized];
  If[
    !categoricalFiniteRealNumberQ[normalization] ||
      !TrueQ[normalization > 0],
    Message[categoricalReverseProbabilities::posterior];
    Return[$Failed]
  ];
  reverseProbabilities = N[unnormalized/normalization];
  If[
    !categoricalProbabilityVectorQ[
      reverseProbabilities,
      categoryCount,
      10^-10
    ],
    Message[categoricalReverseProbabilities::posterior];
    Return[$Failed]
  ];
  reverseProbabilities
];

categoricalReverseProbabilities[___] := (
  Message[categoricalReverseProbabilities::args];
  $Failed
);

categoricalReverseStep::args =
  "categoricalReverseStep expects an Integer scalar state xt, an Integer time, a length-K list of predicted x0 probabilities, a categorical schedule, and optionally one uniform noise value.";
categoricalReverseStep::noise =
  "Explicit uniform noise must be a finite real value in the half-open interval [0, 1).";

categoricalReverseStep[
  xt_,
  t_Integer,
  predictedX0Probabilities_List,
  schedule_Association
] := Module[{reverseProbabilities},
  reverseProbabilities = categoricalReverseProbabilities[
    xt,
    t,
    predictedX0Probabilities,
    schedule
  ];
  If[reverseProbabilities === $Failed, Return[$Failed]];
  categoricalSampleProbabilityVector[reverseProbabilities, RandomReal[]]
];

categoricalReverseStep[
  xt_,
  t_Integer,
  predictedX0Probabilities_List,
  schedule_Association,
  uniformNoise_
] := Module[{reverseProbabilities},
  reverseProbabilities = categoricalReverseProbabilities[
    xt,
    t,
    predictedX0Probabilities,
    schedule
  ];
  If[reverseProbabilities === $Failed, Return[$Failed]];
  If[!categoricalUniformNoiseQ[uniformNoise, {}],
    Message[categoricalReverseStep::noise];
    Return[$Failed]
  ];
  categoricalSampleProbabilityVector[reverseProbabilities, uniformNoise]
];

categoricalReverseStep[___] := (
  Message[categoricalReverseStep::args];
  $Failed
);

(* === TESTS === *)

runCategoricalTests[] := Module[
  {
    passed = 0, assert, kernel, expected, permutation, states,
    randomSample, expectedNoise, q1, q2, q3, schedule, uniformBetas,
    uniformSchedule, x0, noise, t, samples, expectedNext, actualNext,
    replay1, replay2, withinToleranceKernel, overToleranceKernel,
    toleranceSamples, posteriorQ1, posteriorQ2, posteriorQ3,
    posteriorSchedule, posterior, manualUnnormalized, manualPosterior,
    reverseProbabilities, oneHotReverse, qbarPrevious, priorPrevious,
    likelihood, manualReverse, pA, pB, lambda, unnormalizedA,
    unnormalizedB, unnormalizedMix, sparseSchedule, reverseStepSchedule,
    predictedX0, reverseStepManual, reverseStepSamples
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

  posteriorQ1 = {
    {0.7, 0.2, 0.1},
    {0.1, 0.6, 0.3},
    {0.2, 0.1, 0.7}
  };
  posteriorQ2 = {
    {0.5, 0.4, 0.1},
    {0.2, 0.7, 0.1},
    {0.3, 0.2, 0.5}
  };
  posteriorQ3 = {
    {0.6, 0.1, 0.3},
    {0.25, 0.5, 0.25},
    {0.15, 0.35, 0.5}
  };
  posteriorSchedule = makeCategoricalSchedule[
    {posteriorQ1, posteriorQ2, posteriorQ3}
  ];
  posterior = categoricalPosterior[2, 3, 3, posteriorSchedule];
  manualUnnormalized =
    (posteriorQ1 . posteriorQ2)[[2]] posteriorQ3[[All, 3]];
  manualPosterior = manualUnnormalized/Total[manualUnnormalized];
  assert[
    "uses the ordered non-commutative cumulative kernel in the posterior",
    !categoricalNumericArraysCloseQ[
      posteriorQ1 . posteriorQ2,
      posteriorQ2 . posteriorQ1
    ] &&
      categoricalNumericArraysCloseQ[posterior, manualPosterior]
  ];
  assert[
    "returns a normalized non-negative posterior of category length",
    Length[posterior] === 3 &&
      Min[posterior] >= 0 &&
      Abs[Total[posterior] - 1.] < 10^-14
  ];
  assert[
    "reduces the t = 1 posterior to the clean-state point mass",
    categoricalPosterior[2, 3, 1, posteriorSchedule] === {0., 1., 0.}
  ];
  assert[
    "rejects malformed posterior schedules",
    Quiet[categoricalPosterior[
      1,
      1,
      1,
      KeyDrop[posteriorSchedule, "CumulativeTransitionKernels"]
    ]] === $Failed &&
      Quiet[categoricalPosterior[1, 1, 1, {posteriorQ1}]] === $Failed
  ];
  assert[
    "rejects invalid posterior times",
    Quiet[categoricalPosterior[1, 1, 0, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[1, 1, 4, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[1, 1, 1.5, posteriorSchedule]] === $Failed
  ];
  assert[
    "rejects non-Integer and out-of-range posterior states",
    Quiet[categoricalPosterior[1.0, 1, 1, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[1, 1.0, 1, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[0, 1, 1, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[4, 1, 1, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[1, 0, 1, posteriorSchedule]] === $Failed &&
      Quiet[categoricalPosterior[1, 4, 1, posteriorSchedule]] === $Failed
  ];
  assert[
    "rejects a posterior with zero normalization",
    Quiet[categoricalPosterior[
      1,
      2,
      1,
      makeCategoricalSchedule[{IdentityMatrix[3]}]
    ]] === $Failed
  ];

  reverseProbabilities = categoricalReverseProbabilities[
    3,
    3,
    {0.2, 0.5, 0.3},
    posteriorSchedule
  ];
  qbarPrevious = posteriorQ1 . posteriorQ2;
  priorPrevious = {0.2, 0.5, 0.3} . qbarPrevious;
  likelihood = posteriorQ3[[All, 3]];
  manualReverse = priorPrevious likelihood;
  manualReverse = manualReverse/Total[manualReverse];
  assert[
    "matches the canonical joint-marginalized reverse formula",
    !categoricalNumericArraysCloseQ[
      posteriorQ1 . posteriorQ2,
      posteriorQ2 . posteriorQ1
    ] &&
      categoricalNumericArraysCloseQ[reverseProbabilities, manualReverse] &&
      Length[reverseProbabilities] === 3 &&
      Min[reverseProbabilities] >= 0 &&
      Abs[Total[reverseProbabilities] - 1.] < 10^-14
  ];
  oneHotReverse = categoricalReverseProbabilities[
    3,
    3,
    {0., 1., 0.},
    posteriorSchedule
  ];
  assert[
    "reduces a one-hot x0 prediction to the exact posterior",
    categoricalNumericArraysCloseQ[
      oneHotReverse,
      categoricalPosterior[2, 3, 3, posteriorSchedule]
    ]
  ];
  pA = {1., 0., 0.};
  pB = {0., 0., 1.};
  lambda = 0.25;
  unnormalizedA = (pA . qbarPrevious) likelihood;
  unnormalizedB = (pB . qbarPrevious) likelihood;
  unnormalizedMix =
    ((lambda pA + (1. - lambda) pB) . qbarPrevious) likelihood;
  assert[
    "is linear before the single final normalization",
    categoricalNumericArraysCloseQ[
      unnormalizedMix,
      lambda unnormalizedA + (1. - lambda) unnormalizedB
    ] &&
      categoricalNumericArraysCloseQ[
        categoricalReverseProbabilities[
          3,
          3,
          lambda pA + (1. - lambda) pB,
          posteriorSchedule
        ],
        unnormalizedMix/Total[unnormalizedMix]
      ]
  ];
  assert[
    "rejects invalid predicted x0 probability vectors",
    Quiet[categoricalReverseProbabilities[
      3,
      3,
      {0.5, 0.5},
      posteriorSchedule
    ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        3,
        {0.5, -0.1, 0.6},
        posteriorSchedule
      ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        3,
        {0.2, 0.2, 0.2},
        posteriorSchedule
      ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        3,
        {Infinity, 0., 0.},
        posteriorSchedule
      ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        3,
        1.,
        posteriorSchedule
      ]] === $Failed
  ];
  assert[
    "rejects invalid reverse-probability state time and schedule inputs",
    Quiet[categoricalReverseProbabilities[
      0,
      3,
      {0.2, 0.5, 0.3},
      posteriorSchedule
    ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        0,
        {0.2, 0.5, 0.3},
        posteriorSchedule
      ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        3.0,
        {0.2, 0.5, 0.3},
        posteriorSchedule
      ]] === $Failed &&
      Quiet[categoricalReverseProbabilities[
        3,
        3,
        {0.2, 0.5, 0.3},
        KeyDrop[posteriorSchedule, "TransitionKernels"]
      ]] === $Failed
  ];
  sparseSchedule = makeCategoricalSchedule[{IdentityMatrix[3]}];
  assert[
    "allows incompatible predicted x0 candidates with zero joint weight",
    categoricalReverseProbabilities[
      2,
      1,
      {0.4, 0.6, 0.},
      sparseSchedule
    ] === {0., 1., 0.}
  ];
  assert[
    "rejects reverse probabilities when all predicted mass is incompatible",
    Quiet[categoricalReverseProbabilities[
      2,
      1,
      {1., 0., 0.},
      sparseSchedule
    ]] === $Failed
  ];

  reverseStepSchedule = makeUniformCategoricalSchedule[3, {0.2}];
  predictedX0 = {0.2, 0.3, 0.5};
  reverseStepManual =
    predictedX0 reverseStepSchedule["TransitionKernels"][[1, All, 2]];
  reverseStepManual = reverseStepManual/Total[reverseStepManual];
  assert[
    "uses the transition likelihood and correct quantiles at t = 1",
    categoricalNumericArraysCloseQ[
      categoricalReverseProbabilities[
        2,
        1,
        predictedX0,
        reverseStepSchedule
      ],
      reverseStepManual
    ] &&
      categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        0.
      ] === 1 &&
      categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        0.5
      ] === 2 &&
      categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        1. - 10^-12
      ] === 3
  ];
  reverseStepSamples = BlockRandom[
    SeedRandom[9127];
    {
      categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule],
      categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule]
    }
  ];
  assert[
    "automatic reverse steps return valid categories and consume RNG",
    AllTrue[reverseStepSamples, IntegerQ[#] && 1 <= # <= 3 &] &&
      (expectedNext = BlockRandom[
        SeedRandom[9127];
        RandomReal[];
        RandomReal[];
        RandomReal[]
      ];
      actualNext = BlockRandom[
        SeedRandom[9127];
        categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule];
        categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule];
        RandomReal[]
      ];
      actualNext === expectedNext)
  ];
  assert[
    "resetting the external seed reproduces reverse-step samples",
    (replay1 = BlockRandom[
      SeedRandom[2269];
      {
        categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule],
        categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule]
      }
    ];
    replay2 = BlockRandom[
      SeedRandom[2269];
      {
        categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule],
        categoricalReverseStep[2, 1, predictedX0, reverseStepSchedule]
      }
    ];
    replay1 === replay2)
  ];
  assert[
    "explicit reverse-step noise does not consult the random stream",
    BlockRandom[
      SeedRandom[2269];
      categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        0.5
      ];
      RandomReal[]
    ] === BlockRandom[SeedRandom[2269]; RandomReal[]]
  ];
  assert[
    "rejects invalid reverse-step noise and arity",
    Quiet[categoricalReverseStep[
      2,
      1,
      predictedX0,
      reverseStepSchedule,
      -0.1
    ]] === $Failed &&
      Quiet[categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        1.
      ]] === $Failed &&
      Quiet[categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        Infinity
      ]] === $Failed &&
      Quiet[categoricalReverseStep[
        2,
        1,
        predictedX0,
        reverseStepSchedule,
        {0.5}
      ]] === $Failed &&
      Quiet[categoricalReverseStep[2, 1, predictedX0]] === $Failed
  ];

  kernel = uniformCategoricalTransitionKernel[3, 0.6];
  withinToleranceKernel = {
    {0.5, 0.5 - 10^-12},
    {0.25, 0.75}
  };
  overToleranceKernel = {
    {0.5, 0.5 - 2.*10^-12},
    {0.25, 0.75}
  };
  assert[
    "accepts row-stochastic errors within the numerical tolerance",
    Max[Abs[Total[withinToleranceKernel, {2}] - 1.]] <= 10^-12 &&
      categoricalTransitionKernelCategoryCount[withinToleranceKernel] === 2
  ];
  assert[
    "rejects row-stochastic errors beyond the numerical tolerance",
    Max[Abs[Total[overToleranceKernel, {2}] - 1.]] > 10^-12 &&
      categoricalTransitionKernelCategoryCount[overToleranceKernel] ===
        $Failed
  ];
  toleranceSamples = categoricalForwardDiffuse[
    {1, 1, 1},
    withinToleranceKernel,
    {0., 0.5, 1. - 10^-13}
  ];
  assert[
    "samples a tolerance-limit kernel with a stable last-category fallback",
    toleranceSamples === {1, 2, 2} &&
      AllTrue[toleranceSamples, IntegerQ[#] && 1 <= # <= 2 &]
  ];
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
    "matrix-based automatic sampling advances and replays the random stream",
    (expectedNext = BlockRandom[
      SeedRandom[4172];
      RandomReal[{0, 1}, Dimensions[states]];
      RandomReal[]
    ];
    actualNext = BlockRandom[
      SeedRandom[4172];
      categoricalForwardDiffuse[states, kernel];
      RandomReal[]
    ];
    replay1 = BlockRandom[
      SeedRandom[4172];
      categoricalForwardDiffuse[states, kernel]
    ];
    replay2 = BlockRandom[
      SeedRandom[4172];
      categoricalForwardDiffuse[states, kernel]
    ];
    actualNext === expectedNext && replay1 === replay2)
  ];
  assert[
    "matrix-based explicit noise does not consult the random stream",
    BlockRandom[
      SeedRandom[4172];
      categoricalForwardDiffuse[states, kernel, ConstantArray[0.5, {2, 2}]];
      RandomReal[]
    ] === BlockRandom[SeedRandom[4172]; RandomReal[]]
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

  schedule = makeUniformCategoricalSchedule[3, {0.1, 0.2, 0.3}];
  x0 = {1, 2, 3};
  noise = {0.1, 0.5, 0.9};
  t = 2;
  assert[
    "time-indexed t = 0 returns the clean state exactly",
    categoricalForwardDiffuseAt[x0, 0, schedule] === x0 &&
      categoricalForwardDiffuseAt[x0, 0, schedule, noise] === x0
  ];
  assert[
    "time-indexed explicit sampling delegates through Qbar_t",
    categoricalForwardDiffuseAt[x0, t, schedule, noise] ===
      categoricalForwardDiffuse[
        x0,
        schedule["CumulativeTransitionKernels"][[t]],
        noise
      ]
  ];
  samples = {
    1,
    {1, 2, 3},
    {{1, 2}, {3, 1}},
    {{{1, 2}, {3, 1}}, {{2, 3}, {1, 2}}}
  };
  assert[
    "time-indexed sampling preserves scalar vector matrix and tensor shapes",
    AllTrue[
      samples,
      Function[sample,
        Dimensions[categoricalForwardDiffuseAt[
          sample,
          2,
          schedule,
          If[
            IntegerQ[sample],
            0.5,
            ConstantArray[0.5, Dimensions[sample]]
          ]
        ]] === Dimensions[sample]
      ]
    ]
  ];
  assert[
    "time-indexed automatic sampling advances and replays the random stream",
    (expectedNext = BlockRandom[
      SeedRandom[8314];
      RandomReal[{0, 1}, Dimensions[x0]];
      RandomReal[]
    ];
    actualNext = BlockRandom[
      SeedRandom[8314];
      categoricalForwardDiffuseAt[x0, t, schedule];
      RandomReal[]
    ];
    replay1 = BlockRandom[
      SeedRandom[8314];
      categoricalForwardDiffuseAt[x0, t, schedule]
    ];
    replay2 = BlockRandom[
      SeedRandom[8314];
      categoricalForwardDiffuseAt[x0, t, schedule]
    ];
    actualNext === expectedNext && replay1 === replay2)
  ];
  assert[
    "time-indexed explicit noise and t = 0 do not consult the random stream",
    BlockRandom[
      SeedRandom[8314];
      categoricalForwardDiffuseAt[x0, t, schedule, noise];
      categoricalForwardDiffuseAt[x0, 0, schedule];
      RandomReal[]
    ] === BlockRandom[SeedRandom[8314]; RandomReal[]]
  ];
  assert[
    "time-indexed sampling rejects invalid times",
    Quiet[categoricalForwardDiffuseAt[x0, -1, schedule, noise]] === $Failed &&
      Quiet[categoricalForwardDiffuseAt[x0, 4, schedule, noise]] === $Failed &&
      Quiet[categoricalForwardDiffuseAt[x0, 2., schedule, noise]] === $Failed
  ];
  assert[
    "time-indexed sampling rejects malformed schedules and states",
    Quiet[categoricalForwardDiffuseAt[
      x0,
      t,
      ReplacePart[schedule, "CategoryCount" -> 4],
      noise
    ]] === $Failed &&
      Quiet[categoricalForwardDiffuseAt[
        x0,
        t,
        KeyDrop[schedule, "CumulativeTransitionKernels"],
        noise
      ]] === $Failed &&
      Quiet[categoricalForwardDiffuseAt[{1, 4}, t, schedule, {0.1, 0.2}]] ===
        $Failed
  ];
  assert[
    "time-indexed explicit form validates noise at t = 0",
    Quiet[categoricalForwardDiffuseAt[x0, 0, schedule, {0.1}]] === $Failed &&
      Quiet[categoricalForwardDiffuseAt[x0, 0, schedule, {0.1, 0.5, 1.}]] ===
        $Failed
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
