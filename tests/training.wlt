Begin["Stochasma`Private`"]

runTrainingTests[] := Module[
  {
    passed = 0, assert, currentFunction = "makeDiffusionTrainingBatch",
    schedule, cleanSamples, times, noises, batch,
    seeded1, seeded2, differentSeed, automatic1, automatic2,
    replay1, replay2, seed, expectedNext, actualNext,
    conditioningValues, conditionedBatch, expectedConditionedBatch,
    opaqueConditioning, opaqueSamples, opaqueTimes, opaqueNoises,
    opaqueBatch, seededBase, seededConditioned, wrapperRun, baseRun,
    mismatchNext
  },
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ training/", currentFunction, ": ", label];
    Quit[1]
  ];

  schedule = makeDiffusionSchedule[linearBetaSchedule[6, 0.01, 0.06]];
  cleanSamples = {
    {1., 0., -1.},
    {0.5, -0.5, 2.}
  };
  times = {2, 5};
  noises = {
    {0.25, -0.5, 1.},
    {-1., 0.75, 0.}
  };
  batch = makeDiffusionTrainingBatch[
    cleanSamples,
    schedule,
    "Times" -> times,
    "Noises" -> noises
  ];
  assert[
    "returns one documented training association per clean sample",
    Length[batch] === Length[cleanSamples] &&
      AllTrue[
        batch,
        Keys[#] === {"Clean", "Noisy", "Time", "Noise"} &
      ]
  ];
  assert[
    "explicit times and noises are retained in input order",
    Lookup[batch, "Clean"] === cleanSamples &&
      Lookup[batch, "Time"] === times &&
      Lookup[batch, "Noise"] === noises
  ];
  assert[
    "every noisy sample matches the closed-form forward process",
    And @@ MapThread[
      #1["Noisy"] === forwardDiffuse[#2, #3, schedule, #4] &,
      {batch, cleanSamples, times, noises}
    ]
  ];
  assert[
    "fully explicit batches are deterministic and ignore Seed",
    batch === makeDiffusionTrainingBatch[
      cleanSamples,
      schedule,
      "Seed" -> 999,
      "Times" -> times,
      "Noises" -> noises
    ]
  ];
  assert[
    "fully explicit batches do not consult the current random stream",
    BlockRandom[
      SeedRandom[31415];
      makeDiffusionTrainingBatch[
        cleanSamples,
        schedule,
        "Times" -> times,
        "Noises" -> noises
      ];
      RandomReal[]
    ] === BlockRandom[SeedRandom[31415]; RandomReal[]]
  ];

  seeded1 = makeDiffusionTrainingBatch[
    cleanSamples,
    schedule,
    "Seed" -> 2468
  ];
  seeded2 = makeDiffusionTrainingBatch[
    cleanSamples,
    schedule,
    "Seed" -> 2468
  ];
  differentSeed = makeDiffusionTrainingBatch[
    cleanSamples,
    schedule,
    "Seed" -> 1357
  ];
  assert[
    "identical integer seeds reproduce automatic times and noises",
    seeded1 === seeded2
  ];
  assert[
    "different integer seeds change an automatic batch",
    seeded1 =!= differentSeed
  ];
  assert[
    "integer-seeded batches are isolated from the caller random stream",
    BlockRandom[
      SeedRandom[86420];
      makeDiffusionTrainingBatch[
        cleanSamples,
        schedule,
        "Seed" -> 2468
      ];
      RandomReal[]
    ] === BlockRandom[SeedRandom[86420]; RandomReal[]]
  ];

  {automatic1, automatic2} = BlockRandom[
    SeedRandom[97531];
    {
      makeDiffusionTrainingBatch[cleanSamples, schedule],
      makeDiffusionTrainingBatch[cleanSamples, schedule]
    }
  ];
  assert[
    "consecutive automatic batches consume the current random stream",
    automatic1 =!= automatic2
  ];
  replay1 = BlockRandom[
    SeedRandom[97531];
    makeDiffusionTrainingBatch[cleanSamples, schedule]
  ];
  replay2 = BlockRandom[
    SeedRandom[97531];
    makeDiffusionTrainingBatch[cleanSamples, schedule]
  ];
  assert[
    "resetting the current random stream reproduces an automatic batch",
    replay1 === replay2
  ];
  seed = 112233;
  expectedNext = BlockRandom[
    SeedRandom[seed];
    RandomInteger[
      {1, schedule["Steps"]},
      Length[cleanSamples]
    ];
    Scan[randomNormalLike, cleanSamples];
    RandomReal[]
  ];
  actualNext = BlockRandom[
    SeedRandom[seed];
    makeDiffusionTrainingBatch[cleanSamples, schedule];
    RandomReal[]
  ];
  assert[
    "fully automatic batches draw all times before same-shape noises",
    actualNext === expectedNext
  ];

  assert[
    "scalar vector matrix and tensor samples preserve their shapes",
    With[
      {
        shapedSamples = {
          1.5,
          {1., 2.},
          {{1., 2.}, {3., 4.}},
          ArrayReshape[N[Range[8]], {2, 2, 2}]
        }
      },
      Map[
        Dimensions,
        Lookup[
          makeDiffusionTrainingBatch[
            shapedSamples,
            schedule,
            "Times" -> {1, 2, 3, 4},
            "Noises" -> (0. # & /@ shapedSamples)
          ],
          "Noisy"
        ]
      ] === (Dimensions /@ shapedSamples)
    ]
  ];
  assert[
    "empty and non-finite clean-sample lists fail",
    Quiet[makeDiffusionTrainingBatch[{}, schedule]] === $Failed &&
      Quiet[
        makeDiffusionTrainingBatch[{{1., Infinity}}, schedule]
      ] === $Failed
  ];
  assert[
    "invalid explicit times fail",
    Quiet[
      makeDiffusionTrainingBatch[
        cleanSamples,
        schedule,
        "Times" -> {1}
      ]
    ] === $Failed &&
      Quiet[
        makeDiffusionTrainingBatch[
          cleanSamples,
          schedule,
          "Times" -> {0, 2}
        ]
      ] === $Failed &&
      Quiet[
        makeDiffusionTrainingBatch[
          cleanSamples,
          schedule,
          "Times" -> {1., 2}
        ]
      ] === $Failed
  ];
  assert[
    "invalid explicit noises fail",
    Quiet[
      makeDiffusionTrainingBatch[
        cleanSamples,
        schedule,
        "Noises" -> {First[noises]}
      ]
    ] === $Failed &&
      Quiet[
        makeDiffusionTrainingBatch[
          cleanSamples,
          schedule,
          "Noises" -> {{0.}, Last[noises]}
        ]
      ] === $Failed &&
      Quiet[
        makeDiffusionTrainingBatch[
          cleanSamples,
          schedule,
          "Noises" -> {{0., Infinity, 0.}, Last[noises]}
        ]
      ] === $Failed
  ];
  assert[
    "malformed schedules fail",
    Quiet[
      makeDiffusionTrainingBatch[
        cleanSamples,
        ReplacePart[schedule, "Steps" -> 5]
      ]
    ] === $Failed
  ];
  assert[
    "unknown duplicate and invalid options fail",
    Quiet[
      makeDiffusionTrainingBatch[
        cleanSamples,
        schedule,
        "Unknown" -> True
      ]
    ] === $Failed &&
      Quiet[
        makeDiffusionTrainingBatch[
          cleanSamples,
          schedule,
          "Seed" -> 1,
          "Seed" -> 2
        ]
      ] === $Failed &&
      Quiet[
        makeDiffusionTrainingBatch[
          cleanSamples,
          schedule,
          "Seed" -> 1.5
        ]
      ] === $Failed
  ];
  assert[
    "missing and extra positional arguments fail",
    Quiet[makeDiffusionTrainingBatch[]] === $Failed &&
      Quiet[
        makeDiffusionTrainingBatch[cleanSamples, schedule, 42]
      ] === $Failed
  ];

  currentFunction = "makeConditionedDiffusionTrainingBatch";
  conditioningValues = {
    <|"Label" -> "first"|>,
    <|"Label" -> "second"|>
  };
  conditionedBatch = makeConditionedDiffusionTrainingBatch[
    cleanSamples,
    conditioningValues,
    schedule,
    "Times" -> times,
    "Noises" -> noises
  ];
  assert[
    "appends one Conditioning field in documented key order",
    Length[conditionedBatch] === Length[cleanSamples] &&
      AllTrue[
        conditionedBatch,
        Keys[#] === {
          "Clean", "Noisy", "Time", "Noise", "Conditioning"
        } &
      ]
  ];
  expectedConditionedBatch = MapThread[
    Append[#1, "Conditioning" -> #2] &,
    {batch, conditioningValues}
  ];
  assert[
    "delegates diffusion fields unchanged and preserves alignment",
    conditionedBatch === expectedConditionedBatch
  ];

  opaqueConditioning = {
    7,
    {"token", 2},
    <|"Embedding" -> {0.1, 0.2}|>,
    HoldForm[symbolicCondition[x]],
    Automatic
  };
  opaqueSamples = ConstantArray[
    First[cleanSamples],
    Length[opaqueConditioning]
  ];
  opaqueTimes = Range[Length[opaqueConditioning]];
  opaqueNoises = ConstantArray[
    0. First[cleanSamples],
    Length[opaqueConditioning]
  ];
  opaqueBatch = makeConditionedDiffusionTrainingBatch[
    opaqueSamples,
    opaqueConditioning,
    schedule,
    "Times" -> opaqueTimes,
    "Noises" -> opaqueNoises
  ];
  assert[
    "preserves arbitrary conditioning values without interpretation",
    Lookup[opaqueBatch, "Conditioning"] === opaqueConditioning
  ];
  assert[
    "fully explicit batches are deterministic and ignore Seed",
    conditionedBatch === makeConditionedDiffusionTrainingBatch[
      cleanSamples,
      conditioningValues,
      schedule,
      "Seed" -> 999,
      "Times" -> times,
      "Noises" -> noises
    ]
  ];
  assert[
    "fully explicit batches do not consume the caller random stream",
    BlockRandom[
      SeedRandom[24680];
      makeConditionedDiffusionTrainingBatch[
        cleanSamples,
        conditioningValues,
        schedule,
        "Times" -> times,
        "Noises" -> noises
      ];
      RandomReal[]
    ] === BlockRandom[SeedRandom[24680]; RandomReal[]]
  ];

  seededBase = makeDiffusionTrainingBatch[
    cleanSamples,
    schedule,
    "Seed" -> 97531
  ];
  seededConditioned = makeConditionedDiffusionTrainingBatch[
    cleanSamples,
    conditioningValues,
    schedule,
    "Seed" -> 97531
  ];
  assert[
    "integer seeds govern only the delegated diffusion batch",
    (KeyDrop[#, "Conditioning"] & /@ seededConditioned) === seededBase &&
      Lookup[seededConditioned, "Conditioning"] === conditioningValues
  ];

  wrapperRun = BlockRandom[
    SeedRandom[112358];
    {
      makeConditionedDiffusionTrainingBatch[
        cleanSamples,
        conditioningValues,
        schedule
      ],
      RandomReal[]
    }
  ];
  baseRun = BlockRandom[
    SeedRandom[112358];
    {
      makeDiffusionTrainingBatch[cleanSamples, schedule],
      RandomReal[]
    }
  ];
  assert[
    "automatic batches preserve base random-stream consumption",
    (KeyDrop[#, "Conditioning"] & /@ First[wrapperRun]) ===
        First[baseRun] &&
      Last[wrapperRun] === Last[baseRun]
  ];

  expectedNext = BlockRandom[SeedRandom[271828]; RandomReal[]];
  mismatchNext = BlockRandom[
    SeedRandom[271828];
    Quiet[
      makeConditionedDiffusionTrainingBatch[
        cleanSamples,
        {First[conditioningValues]},
        schedule
      ]
    ];
    RandomReal[]
  ];
  assert[
    "rejects misaligned conditioning before consuming randomness",
    mismatchNext === expectedNext
  ];
  assert[
    "propagates base batch validation failures",
    Quiet[
      makeConditionedDiffusionTrainingBatch[{}, {}, schedule]
    ] === $Failed &&
      Quiet[
        makeConditionedDiffusionTrainingBatch[
          cleanSamples,
          conditioningValues,
          ReplacePart[schedule, "Steps" -> 5]
        ]
      ] === $Failed &&
      Quiet[
        makeConditionedDiffusionTrainingBatch[
          cleanSamples,
          conditioningValues,
          schedule,
          "Unknown" -> True
        ]
      ] === $Failed
  ];
  assert[
    "rejects non-list conditioning and invalid arity",
    Quiet[
      makeConditionedDiffusionTrainingBatch[
        cleanSamples,
        "conditioning",
        schedule
      ]
    ] === $Failed &&
      Quiet[makeConditionedDiffusionTrainingBatch[]] === $Failed &&
      Quiet[
        makeConditionedDiffusionTrainingBatch[
          cleanSamples,
          conditioningValues,
          schedule,
          "Seed" -> 1,
          "extra"
        ]
      ] === $Failed
  ];

  Print["✓ training — ", passed, " tests passed"];
  passed
];

End[]
