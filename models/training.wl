If[
  DownValues[Stochasma`makeDiffusionTrainingSample] === {},
  Get[
    FileNameJoin[{
      DirectoryName[$InputFileName], "..", "core", "objectives.wl"
    }]
  ]
];

BeginPackage["Stochasma`"]

makeDiffusionTrainingBatch::usage =
  "makeDiffusionTrainingBatch[cleanSamples, schedule, opts] creates one epsilon-prediction training association per clean sample. \"Times\" and \"Noises\" may be Automatic or explicit lists matching cleanSamples. With automatic values, \"Seed\" -> Automatic uses the current random stream and an Integer seed is reproducible and locally isolated.";

Begin["`Private`"]

Options[makeDiffusionTrainingBatch] = {
  "Seed" -> Automatic,
  "Times" -> Automatic,
  "Noises" -> Automatic
};

makeDiffusionTrainingBatch::samples =
  "cleanSamples must be a non-empty list of finite real numeric scalar or array samples.";
makeDiffusionTrainingBatch::schedule =
  "The supplied diffusion schedule is malformed or internally inconsistent.";
makeDiffusionTrainingBatch::opts =
  "Options must use each of \"Seed\", \"Times\", and \"Noises\" at most once with valid values.";
makeDiffusionTrainingBatch::times =
  "Explicit \"Times\" must be a list matching cleanSamples with Integer entries in the inclusive range 1 through `1`.";
makeDiffusionTrainingBatch::noises =
  "Explicit \"Noises\" must be a list matching cleanSamples whose entries have the same finite real shapes as their clean samples.";
makeDiffusionTrainingBatch::args =
  "makeDiffusionTrainingBatch expects a non-empty list of clean samples, a diffusion schedule, and optional rules.";

runDiffusionTrainingBatch[
  cleanSamples_List,
  schedule_Association,
  times_,
  noises_
] := Module[{resolvedTimes, resolvedNoises},
  resolvedTimes = If[
    times === Automatic,
    RandomInteger[{1, schedule["Steps"]}, Length[cleanSamples]],
    times
  ];
  resolvedNoises = If[
    noises === Automatic,
    randomNormalLike /@ cleanSamples,
    noises
  ];
  MapThread[
    makeDiffusionTrainingSample[#1, #2, schedule, #3] &,
    {cleanSamples, resolvedTimes, resolvedNoises}
  ]
];

makeDiffusionTrainingBatch[
  cleanSamples_List,
  schedule_Association,
  opts___
] := Module[{given, keys, values, seed, times, noises, run},
  given = {opts};
  If[!OptionQ[given],
    Message[makeDiffusionTrainingBatch::opts];
    Return[$Failed]
  ];
  keys = First /@ given;
  If[
    !AllTrue[keys, MemberQ[{"Seed", "Times", "Noises"}, #] &] ||
      DuplicateFreeQ[keys] === False,
    Message[makeDiffusionTrainingBatch::opts];
    Return[$Failed]
  ];
  values = Join[
    Association[Options[makeDiffusionTrainingBatch]],
    Association[given]
  ];
  seed = values["Seed"];
  times = values["Times"];
  noises = values["Noises"];
  If[
    !(seed === Automatic || IntegerQ[seed]) ||
      !(times === Automatic || ListQ[times]) ||
      !(noises === Automatic || ListQ[noises]),
    Message[makeDiffusionTrainingBatch::opts];
    Return[$Failed]
  ];
  If[cleanSamples === {} || !AllTrue[cleanSamples, realNumericSampleQ],
    Message[makeDiffusionTrainingBatch::samples];
    Return[$Failed]
  ];
  If[!diffusionScheduleQ[schedule],
    Message[makeDiffusionTrainingBatch::schedule];
    Return[$Failed]
  ];
  If[
    ListQ[times] &&
      (Length[times] =!= Length[cleanSamples] ||
        !AllTrue[
          times,
          IntegerQ[#] && TrueQ[1 <= # <= schedule["Steps"]] &
        ]),
    Message[makeDiffusionTrainingBatch::times, schedule["Steps"]];
    Return[$Failed]
  ];
  If[
    ListQ[noises] &&
      (Length[noises] =!= Length[cleanSamples] ||
        !And @@ MapThread[sameSampleShapeQ, {cleanSamples, noises}]),
    Message[makeDiffusionTrainingBatch::noises];
    Return[$Failed]
  ];
  run = runDiffusionTrainingBatch[
    cleanSamples,
    schedule,
    times,
    noises
  ] &;
  If[IntegerQ[seed], BlockRandom[SeedRandom[seed]; run[]], run[]]
];

makeDiffusionTrainingBatch[___] := (
  Message[makeDiffusionTrainingBatch::args];
  $Failed
);

(* === TESTS === *)

runTrainingTests[] := Module[
  {
    passed = 0, assert, schedule, cleanSamples, times, noises, batch,
    seeded1, seeded2, differentSeed, automatic1, automatic2,
    replay1, replay2
  },
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ training/makeDiffusionTrainingBatch: ", label];
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

  Print["✓ training — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "training.wl"],
  Stochasma`Private`runTrainingTests[]
]
