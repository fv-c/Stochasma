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
  "categoricalReverseProbabilities[xt, t, predictedX0Probabilities, schedule] constructs the predicted-x0 categorical reverse distribution by marginalizing the joint q(x_(t-1), x_t | x_0) under the predicted clean-state probabilities and normalizing once; at t = 1 it returns the predicted clean-state distribution directly.";

categoricalReverseStep::usage =
  "categoricalReverseStep[xt, t, predictedX0Probabilities, schedule] samples x_(t-1) from the predicted-x0 categorical reverse probabilities using the current random stream. categoricalReverseStep[xt, t, predictedX0Probabilities, schedule, uniformNoise] uses an explicit uniform variate in the half-open interval [0, 1) and is deterministic.";

categoricalSample::usage =
  "categoricalSample[predictor, initialState, schedule, opts] runs the full scalar categorical reverse process from caller-supplied initialState = xT through x_(T-1), ..., x0. It never generates a terminal prior. At each logical time t = T down to 1 it calls predictor[xt, t] for a length-K predicted-x0 probability vector and samples one categoricalReverseStep. \"Noises\" -> Automatic uses one current-stream uniform variate per step; an explicit length-T \"Noises\" list is deterministic, indexed by logical time so Noises[[t]] drives t to t - 1, and takes precedence over \"Seed\". \"Seed\" -> Automatic uses the caller stream; an Integer seed is reproducible and locally isolated. \"ReturnTrajectory\" -> True returns <|\"Sample\" -> x0, \"Trajectory\" -> {xT, ..., x0}, \"Timesteps\" -> {T, ..., 0}|>.";

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

normalizeCategoricalWeights[weights_] := Module[
  {numericWeights, scale, scaledWeights, normalization, probabilities},
  If[
    !ListQ[weights] || weights === {} ||
      !VectorQ[weights, categoricalFiniteRealNumberQ],
    Return[$Failed]
  ];
  numericWeights = N[weights];
  If[!AllTrue[numericWeights, TrueQ[# >= 0] &], Return[$Failed]];
  scale = Max[numericWeights];
  If[
    !categoricalFiniteRealNumberQ[scale] || !TrueQ[scale > 0],
    Return[$Failed]
  ];
  scaledWeights = numericWeights/scale;
  normalization = Total[scaledWeights];
  If[
    !categoricalFiniteRealNumberQ[normalization] ||
      !TrueQ[normalization > 0],
    Return[$Failed]
  ];
  probabilities = N[scaledWeights/normalization];
  If[
    !VectorQ[probabilities, categoricalFiniteRealNumberQ] ||
      !AllTrue[probabilities, TrueQ[# >= 0] &],
    $Failed,
    probabilities
  ]
];

categoricalPosteriorValidated[
  x0_,
  xt_,
  t_Integer,
  schedule_Association
] := Module[{categoryCount, priorPrevious, likelihood, unnormalized},
  categoryCount = schedule["CategoryCount"];
  priorPrevious = If[
    t == 1,
    N[IdentityMatrix[categoryCount]][[x0]],
    schedule["CumulativeTransitionKernels"][[t - 1, x0]]
  ];
  likelihood = schedule["TransitionKernels"][[t, All, xt]];
  unnormalized = N[priorPrevious likelihood];
  normalizeCategoricalWeights[unnormalized]
];

categoricalPosterior[
  x0_,
  xt_,
  t_Integer,
  schedule_Association
] := Module[
  {categoryCount, posterior},
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
  posterior = categoricalPosteriorValidated[x0, xt, t, schedule];
  If[posterior === $Failed,
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
  "The predicted-x0 categorical reverse distribution has zero or non-finite normalization for the supplied state and time.";

categoricalReverseProbabilitiesValidated[
  xt_,
  t_Integer,
  predictedX0Probabilities_List,
  schedule_Association
] := Module[
  {categoryCount, priorPrevious, likelihood, reverseProbabilities},
  categoryCount = schedule["CategoryCount"];
  If[t == 1, Return[N[predictedX0Probabilities]]];
  priorPrevious =
    N[predictedX0Probabilities] .
      schedule["CumulativeTransitionKernels"][[t - 1]];
  likelihood = schedule["TransitionKernels"][[t, All, xt]];
  reverseProbabilities = normalizeCategoricalWeights[
    N[priorPrevious likelihood]
  ];
  If[
    reverseProbabilities === $Failed ||
      !categoricalProbabilityVectorQ[
        reverseProbabilities,
        categoryCount,
        10^-10
      ],
    $Failed,
    reverseProbabilities
  ]
];

categoricalReverseProbabilities[
  xt_,
  t_Integer,
  predictedX0Probabilities_List,
  schedule_Association
] := Module[
  {categoryCount, reverseProbabilities},
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
  reverseProbabilities = categoricalReverseProbabilitiesValidated[
    xt,
    t,
    predictedX0Probabilities,
    schedule
  ];
  If[reverseProbabilities === $Failed,
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
categoricalReverseStep::schedule =
  "The supplied categorical schedule is malformed or internally inconsistent.";
categoricalReverseStep::time =
  "Time t must be an Integer in the inclusive range 1 through `1`.";
categoricalReverseStep::xt =
  "State xt must be an Integer between 1 and the categorical schedule's category count.";
categoricalReverseStep::probabilities =
  "Predicted x0 probabilities must be a length-K list of finite, non-negative values whose sum is numerically 1.";
categoricalReverseStep::posterior =
  "The predicted-x0 categorical reverse distribution has zero or non-finite normalization for the supplied state and time.";

categoricalReverseStepValidated[
  xt_,
  t_Integer,
  predictedX0Probabilities_List,
  schedule_Association,
  uniformNoise_
] := Module[{reverseProbabilities},
  reverseProbabilities = categoricalReverseProbabilitiesValidated[
    xt,
    t,
    predictedX0Probabilities,
    schedule
  ];
  If[reverseProbabilities === $Failed, Return[$Failed]];
  categoricalSampleProbabilityVector[reverseProbabilities, uniformNoise]
];

categoricalReverseStep[
  xt_,
  t_Integer,
  predictedX0Probabilities_List,
  schedule_Association
] := Module[{reverseProbabilities},
  If[!categoricalScheduleQ[schedule],
    Message[categoricalReverseStep::schedule];
    Return[$Failed]
  ];
  If[!TrueQ[1 <= t <= schedule["Steps"]],
    Message[categoricalReverseStep::time, schedule["Steps"]];
    Return[$Failed]
  ];
  If[
    !IntegerQ[xt] ||
      !TrueQ[1 <= xt <= schedule["CategoryCount"]],
    Message[categoricalReverseStep::xt];
    Return[$Failed]
  ];
  If[
    !categoricalProbabilityVectorQ[
      predictedX0Probabilities,
      schedule["CategoryCount"]
    ],
    Message[categoricalReverseStep::probabilities];
    Return[$Failed]
  ];
  reverseProbabilities = categoricalReverseProbabilitiesValidated[
    xt,
    t,
    predictedX0Probabilities,
    schedule
  ];
  If[reverseProbabilities === $Failed,
    Message[categoricalReverseStep::posterior];
    Return[$Failed]
  ];
  categoricalSampleProbabilityVector[reverseProbabilities, RandomReal[]]
];

categoricalReverseStep[
  xt_,
  t_Integer,
  predictedX0Probabilities_List,
  schedule_Association,
  uniformNoise_
] := Module[{reverseProbabilities},
  If[!categoricalScheduleQ[schedule],
    Message[categoricalReverseStep::schedule];
    Return[$Failed]
  ];
  If[!TrueQ[1 <= t <= schedule["Steps"]],
    Message[categoricalReverseStep::time, schedule["Steps"]];
    Return[$Failed]
  ];
  If[
    !IntegerQ[xt] ||
      !TrueQ[1 <= xt <= schedule["CategoryCount"]],
    Message[categoricalReverseStep::xt];
    Return[$Failed]
  ];
  If[
    !categoricalProbabilityVectorQ[
      predictedX0Probabilities,
      schedule["CategoryCount"]
    ],
    Message[categoricalReverseStep::probabilities];
    Return[$Failed]
  ];
  If[!categoricalUniformNoiseQ[uniformNoise, {}],
    Message[categoricalReverseStep::noise];
    Return[$Failed]
  ];
  reverseProbabilities = categoricalReverseStepValidated[
    xt,
    t,
    predictedX0Probabilities,
    schedule,
    uniformNoise
  ];
  If[reverseProbabilities === $Failed,
    Message[categoricalReverseStep::posterior];
    Return[$Failed]
  ];
  reverseProbabilities
];

categoricalReverseStep[___] := (
  Message[categoricalReverseStep::args];
  $Failed
);

Options[categoricalSample] = {
  "Seed" -> Automatic,
  "Noises" -> Automatic,
  "ReturnTrajectory" -> False
};

categoricalSample::state =
  "initialState must be an Integer between 1 and the categorical schedule's category count.";
categoricalSample::schedule =
  "The supplied categorical schedule is malformed or internally inconsistent.";
categoricalSample::predictor =
  "The predictor must return a length-K list of finite, non-negative predicted x0 probabilities whose sum is numerically 1 at every time.";
categoricalSample::opts =
  "Options must use each of \"Seed\", \"Noises\", and \"ReturnTrajectory\" at most once with valid values.";
categoricalSample::noises =
  "Explicit \"Noises\" must be a length-T list of finite real values in the half-open interval [0, 1), indexed by logical time.";
categoricalSample::posterior =
  "No finite normalized reverse distribution can be constructed from the predictor output at the current state and time.";
categoricalSample::args =
  "categoricalSample expects a predictor callable, an Integer initial state, a categorical schedule, and optional rules.";

runCategoricalSampling[
  predictor_,
  initialState_,
  schedule_Association,
  returnTrajectory_,
  noises_
] := Module[
  {
    state, trajectory, predictedX0Probabilities, noise,
    categoryCount, t, failed
  },
  state = initialState;
  trajectory = {initialState};
  categoryCount = schedule["CategoryCount"];
  failed = False;
  Do[
    predictedX0Probabilities = Check[predictor[state, t], $Failed];
    If[
      predictedX0Probabilities === $Failed ||
        !categoricalProbabilityVectorQ[
          predictedX0Probabilities,
          categoryCount
        ],
      Message[categoricalSample::predictor];
      failed = True;
      Break[]
    ];
    noise = If[
      ListQ[noises],
      noises[[t]],
      RandomReal[]
    ];
    state = categoricalReverseStepValidated[
      state,
      t,
      predictedX0Probabilities,
      schedule,
      noise
    ];
    If[state === $Failed,
      Message[categoricalSample::posterior];
      failed = True;
      Break[]
    ];
    trajectory = Append[trajectory, state],
    {t, schedule["Steps"], 1, -1}
  ];
  If[failed, Return[$Failed]];
  If[
    TrueQ[returnTrajectory],
    <|
      "Sample" -> state,
      "Trajectory" -> trajectory,
      "Timesteps" -> Append[Reverse[Range[schedule["Steps"]]], 0]
    |>,
    state
  ]
];

categoricalSample[
  predictor_,
  initialState_,
  schedule_Association,
  opts___
] := Module[
  {
    given, keys, values, returnTrajectory, seed, noises,
    categoryCount
  },
  given = {opts};
  If[!OptionQ[given],
    Message[categoricalSample::opts];
    Return[$Failed]
  ];
  keys = First /@ given;
  If[
    !AllTrue[
      keys,
      MemberQ[{"Seed", "Noises", "ReturnTrajectory"}, #] &
    ] || DuplicateFreeQ[keys] === False,
    Message[categoricalSample::opts];
    Return[$Failed]
  ];
  values = Join[Association[Options[categoricalSample]], Association[given]];
  returnTrajectory = values["ReturnTrajectory"];
  seed = values["Seed"];
  noises = values["Noises"];
  If[
    !MemberQ[{True, False}, returnTrajectory] ||
      !(seed === Automatic || IntegerQ[seed]),
    Message[categoricalSample::opts];
    Return[$Failed]
  ];
  If[!categoricalScheduleQ[schedule],
    Message[categoricalSample::schedule];
    Return[$Failed]
  ];
  categoryCount = schedule["CategoryCount"];
  If[
    !IntegerQ[initialState] ||
      !TrueQ[1 <= initialState <= categoryCount],
    Message[categoricalSample::state];
    Return[$Failed]
  ];
  If[
    noises =!= Automatic &&
      (!ListQ[noises] ||
        Length[noises] =!= schedule["Steps"] ||
        !AllTrue[
          noises,
          categoricalUniformNoiseQ[#, {}] &
        ]),
    Message[categoricalSample::noises];
    Return[$Failed]
  ];
  Which[
    ListQ[noises],
      runCategoricalSampling[
        predictor,
        initialState,
        schedule,
        returnTrajectory,
        noises
      ],
    IntegerQ[seed],
      BlockRandom[
        SeedRandom[seed];
        runCategoricalSampling[
          predictor,
          initialState,
          schedule,
          returnTrajectory,
          Automatic
        ]
      ],
    True,
      runCategoricalSampling[
        predictor,
        initialState,
        schedule,
        returnTrajectory,
        Automatic
      ]
  ]
];

categoricalSample[___] := (
  Message[categoricalSample::args];
  $Failed
);

End[]

EndPackage[]
