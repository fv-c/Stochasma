If[
  DownValues[Stochasma`sinusoidalTimeEmbedding] === {},
  Get[FileNameJoin[{DirectoryName[$InputFileName], "embeddings.wl"}]]
];

BeginPackage["Stochasma`"]

makeWolframNetPredictor::usage =
  "makeWolframNetPredictor[network, inputAdapter] returns a predictor function with the contract predictor[xt, t] for a Wolfram NetChain or NetGraph. By default, the network receives <|\"Sample\" -> xt, \"Time\" -> N[t]|>. An explicit inputAdapter[xt, t] may construct any input accepted by the network, including sinusoidal time features or a single tensor.";

Begin["`Private`"]

makeWolframNetPredictor::args =
  "makeWolframNetPredictor expects a NetChain or NetGraph and, optionally, a callable input adapter.";
makeWolframNetPredictor::input =
  "The input adapter failed to construct a network input from xt and t.";
makeWolframNetPredictor::network =
  "The Wolfram neural network failed to evaluate the adapted input.";

makeWolframNetPredictor[
  network : (_NetChain | _NetGraph),
  inputAdapter_ : Automatic
] := Module[{adapter},
  adapter = If[
    inputAdapter === Automatic,
    Function[{xt, time}, <|"Sample" -> xt, "Time" -> N[time]|>],
    inputAdapter
  ];
  Function[{xt, time},
    Module[{networkInput, prediction},
      networkInput = Quiet[Check[adapter[xt, time], $Failed]];
      If[networkInput === $Failed,
        Message[makeWolframNetPredictor::input];
        $Failed,
        prediction = Quiet[Check[network[networkInput], $Failed]];
        If[prediction === $Failed,
          Message[makeWolframNetPredictor::network];
          $Failed,
          prediction
        ]
      ]
    ]
  ]
];

makeWolframNetPredictor[___] := (
  Message[makeWolframNetPredictor::args];
  $Failed
);

(* === TESTS === *)

runAdaptersTests[] := Module[
  {
    passed = 0, assert, twoInputNetwork, defaultPredictor, sample,
    prediction, embeddingNetwork, embeddingPredictor,
    embeddingPrediction, expectedEmbeddingPrediction, singleInputNetwork,
    singleInputPredictor, before, after, failingPredictor
  },
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ adapters/makeWolframNetPredictor: ", label];
    Quit[1]
  ];

  twoInputNetwork = NetGraph[
    {"Sum" -> ThreadingLayer[Plus]},
    {
      NetPort["Sample"] -> NetPort["Sum", 1],
      NetPort["Time"] -> NetPort["Sum", 2]
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

  embeddingNetwork = NetGraph[
    {"Sum" -> ThreadingLayer[Plus]},
    {
      NetPort["State"] -> NetPort["Sum", 1],
      NetPort["Embedding"] -> NetPort["Sum", 2]
    },
    "State" -> 4,
    "Embedding" -> 4
  ];
  embeddingPredictor = makeWolframNetPredictor[
    embeddingNetwork,
    Function[{xt, time},
      <|
        "State" -> xt,
        "Embedding" -> sinusoidalTimeEmbedding[time, 4]
      |>
    ]
  ];
  expectedEmbeddingPrediction =
    {0.5, -0.5, 1., -1.} + sinusoidalTimeEmbedding[3, 4];
  embeddingPrediction = embeddingPredictor[{0.5, -0.5, 1., -1.}, 3];
  assert[
    "a custom adapter can supply named sinusoidal inputs",
    Dimensions[embeddingPrediction] === {4} &&
      Max[Abs[embeddingPrediction - expectedEmbeddingPrediction]] < 10^-6
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

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "adapters.wl"],
  Stochasma`Private`runAdaptersTests[]
]
