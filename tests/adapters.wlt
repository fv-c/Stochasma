Begin["Stochasma`Private`"]

runAdaptersTests[] := Module[
  {
    passed = 0, assert, twoInputNetwork, defaultPredictor, sample,
    prediction, namedInputNetwork, namedInputPredictor,
    namedInputPrediction, expectedNamedInputPrediction, singleInputNetwork,
    singleInputPredictor, before, after, failingPredictor
  },
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ adapters/makeWolframNetPredictor: ", label];
    Quit[1]
  ];

  twoInputNetwork = NetGraph[
    {
      "ExpandTime" -> ReplicateLayer[3],
      "FlattenTime" -> FlattenLayer[],
      "Sum" -> ThreadingLayer[Plus]
    },
    {
      NetPort["Sample"] -> NetPort["Sum", 1],
      NetPort["Time"] -> "ExpandTime" -> "FlattenTime" ->
        NetPort["Sum", 2]
    },
    "Sample" -> 3,
    "Time" -> "Scalar"
  ];
  defaultPredictor = makeWolframNetPredictor[twoInputNetwork];
  sample = {1., 2., 3.};
  prediction = defaultPredictor[sample, 2];
  assert[
    "returns a callable predictor",
    Head[defaultPredictor] === Function
  ];
  assert[
    "the default adapter supplies Sample and numeric Time ports",
    Dimensions[prediction] === Dimensions[sample] &&
      Max[Abs[prediction - {3., 4., 5.}]] < 10^-6
  ];

  namedInputNetwork = NetGraph[
    {"Sum" -> ThreadingLayer[Plus]},
    {
      NetPort["State"] -> NetPort["Sum", 1],
      NetPort["Embedding"] -> NetPort["Sum", 2]
    },
    "State" -> 4,
    "Embedding" -> 4
  ];
  namedInputPredictor = makeWolframNetPredictor[
    namedInputNetwork,
    Function[{xt, time},
      <|
        "State" -> xt,
        "Embedding" -> ConstantArray[N[time], 4]
      |>
    ]
  ];
  expectedNamedInputPrediction = {0.5, -0.5, 1., -1.} + {3., 3., 3., 3.};
  namedInputPrediction = namedInputPredictor[{0.5, -0.5, 1., -1.}, 3];
  assert[
    "a custom adapter can supply named deterministic inputs",
    Dimensions[namedInputPrediction] === {4} &&
      Max[Abs[namedInputPrediction - expectedNamedInputPrediction]] < 10^-6
  ];

  singleInputNetwork = NetChain[
    {ElementwiseLayer[2 # &]},
    "Input" -> 3
  ];
  singleInputPredictor = makeWolframNetPredictor[
    singleInputNetwork,
    Function[{xt, time}, xt + N[time]]
  ];
  assert[
    "a custom adapter can feed a single-input NetChain",
    Max[
      Abs[singleInputPredictor[{1., 2., 3.}, 2] - {6., 8., 10.}]
    ] < 10^-6
  ];
  assert[
    "repeated evaluation is deterministic",
    defaultPredictor[sample, 2] === defaultPredictor[sample, 2]
  ];
  BlockRandom[
    SeedRandom[314159];
    before = RandomReal[];
    defaultPredictor[sample, 2];
    after = RandomReal[];
  ];
  assert[
    "deterministic inference does not consume random state",
    BlockRandom[
      SeedRandom[314159];
      {before, after} === {RandomReal[], RandomReal[]}
    ]
  ];

  failingPredictor = makeWolframNetPredictor[
    twoInputNetwork,
    Function[{xt, time}, $Failed]
  ];
  assert[
    "an input-adapter failure is propagated",
    Quiet[failingPredictor[sample, 2]] === $Failed
  ];
  failingPredictor = makeWolframNetPredictor[
    twoInputNetwork,
    Function[{xt, time}, <|"WrongPort" -> xt|>]
  ];
  assert[
    "a network-evaluation failure is propagated",
    Quiet[failingPredictor[sample, 2]] === $Failed
  ];
  assert[
    "a clearly non-callable input adapter is rejected",
    Quiet[makeWolframNetPredictor[twoInputNetwork, 42]] === $Failed
  ];
  assert[
    "non-network callables are rejected",
    Quiet[makeWolframNetPredictor[Function[input, input]]] === $Failed
  ];
  assert[
    "missing arguments are rejected",
    Quiet[makeWolframNetPredictor[]] === $Failed
  ];
  assert[
    "extra arguments are rejected",
    Quiet[
      makeWolframNetPredictor[twoInputNetwork, Identity, "extra"]
    ] === $Failed
  ];

  Print["✓ adapters — ", passed, " tests passed"];
  passed
];

End[]
