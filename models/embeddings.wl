BeginPackage["Stochasma`"]

sinusoidalTimeEmbedding::usage =
  "sinusoidalTimeEmbedding[time, dimensions, opts] returns a deterministic sinusoidal embedding of a finite non-negative diffusion time. dimensions must be an Integer greater than or equal to 2. The option \"MaxPeriod\" (default 10000.) controls the lowest generated frequency; odd dimensions are padded with one trailing zero.";

Begin["`Private`"]

finiteRealTimeQ[value_] := Quiet[Check[
  NumberQ[N[value]] &&
    TrueQ[Im[N[value]] == 0] &&
    FreeQ[N[value], Indeterminate | ComplexInfinity | DirectedInfinity],
  False
]];

Options[sinusoidalTimeEmbedding] = {
  "MaxPeriod" -> 10000.
};

sinusoidalTimeEmbedding::args =
  "sinusoidalTimeEmbedding expects a finite non-negative real time, an Integer dimension count greater than or equal to 2, and valid options.";
sinusoidalTimeEmbedding::maxperiod =
  "The \"MaxPeriod\" option must be a finite real number greater than or equal to 1.";

sinusoidalTimeEmbedding[time_, dimensions_, opts___] := Module[
  {given, keys, values, maxPeriod, halfDimensions, frequencies, angles,
    embedding},
  given = {opts};
  If[
    !finiteRealTimeQ[time] || !TrueQ[time >= 0] ||
      !IntegerQ[dimensions] || dimensions < 2 || !OptionQ[given],
    Message[sinusoidalTimeEmbedding::args];
    Return[$Failed]
  ];
  keys = First /@ given;
  If[
    !AllTrue[keys, MemberQ[{"MaxPeriod"}, #] &] ||
      DuplicateFreeQ[keys] === False,
    Message[sinusoidalTimeEmbedding::args];
    Return[$Failed]
  ];
  values = Join[
    Association[Options[sinusoidalTimeEmbedding]],
    Association[given]
  ];
  maxPeriod = values["MaxPeriod"];
  If[
    !finiteRealTimeQ[maxPeriod] || !TrueQ[maxPeriod >= 1],
    Message[sinusoidalTimeEmbedding::maxperiod];
    Return[$Failed]
  ];
  halfDimensions = Quotient[dimensions, 2];
  frequencies = Exp[
    -Log[N[maxPeriod]] Range[0, halfDimensions - 1]/halfDimensions
  ];
  angles = N[time] frequencies;
  embedding = Join[Cos[angles], Sin[angles]];
  If[OddQ[dimensions], Append[embedding, 0.], embedding]
];

(* === TESTS === *)

runEmbeddingsTests[] := Module[
  {
    passed = 0, assert, embeddingAtZero2, embeddingAtZero6,
    embeddingAtThree6, oddEmbedding, customEmbedding, before, after
  },
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ embeddings/sinusoidalTimeEmbedding: ", label];
    Quit[1]
  ];

  embeddingAtZero2 = sinusoidalTimeEmbedding[0, 2];
  assert[
    "two-dimensional time-zero embedding has the requested finite values",
    embeddingAtZero2 === {1., 0.} &&
      AllTrue[embeddingAtZero2, finiteRealTimeQ]
  ];
  embeddingAtZero6 = sinusoidalTimeEmbedding[0, 6];
  assert[
    "six-dimensional time-zero embedding has the requested dimensions",
    Dimensions[embeddingAtZero6] === {6} &&
      AllTrue[embeddingAtZero6, finiteRealTimeQ]
  ];
  embeddingAtThree6 = sinusoidalTimeEmbedding[3, 6];
  assert[
    "nonzero-time embedding has the requested finite dimensions",
    Dimensions[embeddingAtThree6] === {6} &&
      AllTrue[embeddingAtThree6, finiteRealTimeQ]
  ];
  assert[
    "evaluation is deterministic",
    embeddingAtThree6 === sinusoidalTimeEmbedding[3, 6]
  ];
  assert[
    "each sine-cosine frequency pair has unit squared norm",
    Max[
      Abs[
        Take[embeddingAtThree6, 3]^2 +
          Take[embeddingAtThree6, -3]^2 - 1
      ]
    ] < 10^-12
  ];
  assert[
    "time zero has cosine components equal to one and sine components zero",
    embeddingAtZero6 === {1., 1., 1., 0., 0., 0.}
  ];
  customEmbedding = sinusoidalTimeEmbedding[
    2,
    4,
    "MaxPeriod" -> 100.
  ];
  assert[
    "custom maximum period follows the documented frequency formula",
    Max[
      Abs[
        customEmbedding - {Cos[2.], Cos[0.2], Sin[2.], Sin[0.2]}
      ]
    ] < 10^-12
  ];
  oddEmbedding = sinusoidalTimeEmbedding[2, 5];
  assert[
    "odd dimensions receive one trailing zero",
    Dimensions[oddEmbedding] === {5} &&
      oddEmbedding === Append[sinusoidalTimeEmbedding[2, 4], 0.]
  ];
  BlockRandom[
    SeedRandom[314159];
    before = RandomReal[];
    sinusoidalTimeEmbedding[7, 8];
    after = RandomReal[];
  ];
  assert[
    "evaluation does not consume random state",
    BlockRandom[
      SeedRandom[314159];
      {before, after} === {RandomReal[], RandomReal[]}
    ]
  ];
  assert[
    "one dimension fails",
    Quiet[sinusoidalTimeEmbedding[1, 1]] === $Failed
  ];
  assert[
    "zero dimensions fail",
    Quiet[sinusoidalTimeEmbedding[1, 0]] === $Failed
  ];
  assert[
    "non-integer dimensions fail",
    Quiet[sinusoidalTimeEmbedding[1, 4.]] === $Failed
  ];
  assert[
    "negative time fails",
    Quiet[sinusoidalTimeEmbedding[-1, 4]] === $Failed
  ];
  assert[
    "non-finite time fails",
    Quiet[sinusoidalTimeEmbedding[Infinity, 4]] === $Failed
  ];
  assert[
    "symbolic time fails",
    Quiet[sinusoidalTimeEmbedding[symbolicTime, 4]] === $Failed
  ];
  assert[
    "maximum period below one fails",
    Quiet[
      sinusoidalTimeEmbedding[1, 4, "MaxPeriod" -> 0.5]
    ] === $Failed
  ];
  assert[
    "unknown options fail",
    Quiet[
      sinusoidalTimeEmbedding[1, 4, "Unknown" -> 1]
    ] === $Failed
  ];
  assert[
    "duplicate options fail",
    Quiet[
      sinusoidalTimeEmbedding[
        1,
        4,
        "MaxPeriod" -> 100.,
        "MaxPeriod" -> 1000.
      ]
    ] === $Failed
  ];

  Print["✓ embeddings — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "embeddings.wl"],
  Stochasma`Private`runEmbeddingsTests[]
]
