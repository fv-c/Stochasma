If[
  DownValues[Stochasma`makeDiffusionTrainingSample] === {},
  Get[
    FileNameJoin[{
      DirectoryName[$InputFileName], "..", "core", "objectives.wl"
    }]
  ]
];

BeginPackage["Stochasma`"]

makeLatentDiffusionTrainingSample::usage =
  "makeLatentDiffusionTrainingSample[encoder, input, t, schedule] evaluates encoder[input] once and creates an epsilon-prediction training example in the resulting latent space. The five-argument form uses explicit noise matching the encoded latent. The returned \"Clean\" field is the encoded latent.";

Begin["`Private`"]

makeLatentDiffusionTrainingSample::schedule =
  "The supplied diffusion schedule is malformed or internally inconsistent.";
makeLatentDiffusionTrainingSample::time =
  "Training time t must be an Integer in the inclusive range 1 through `1`.";
makeLatentDiffusionTrainingSample::latent =
  "encoder[input] must evaluate to a finite real numeric scalar or non-empty array.";
makeLatentDiffusionTrainingSample::noise =
  "Noise must be finite, real-valued, and have the same shape as the encoded latent.";
makeLatentDiffusionTrainingSample::args =
  "makeLatentDiffusionTrainingSample expects an encoder, an input, an Integer time, a diffusion schedule, and optional explicit latent noise.";

validateLatentTrainingRequest[t_Integer, schedule_Association] := Module[{},
  If[!diffusionScheduleQ[schedule],
    Message[makeLatentDiffusionTrainingSample::schedule];
    Return[False]
  ];
  If[!TrueQ[1 <= t <= schedule["Steps"]],
    Message[makeLatentDiffusionTrainingSample::time, schedule["Steps"]];
    Return[False]
  ];
  True
];

encodeLatentTrainingInput[encoder_, input_] := Module[{latent},
  latent = encoder[input];
  If[!realNumericSampleQ[latent],
    Message[makeLatentDiffusionTrainingSample::latent];
    Return[$Failed]
  ];
  latent
];

makeLatentDiffusionTrainingSample[
  encoder_,
  input_,
  t_Integer,
  schedule_Association
] := Module[{latent},
  If[!validateLatentTrainingRequest[t, schedule], Return[$Failed]];
  latent = encodeLatentTrainingInput[encoder, input];
  If[latent === $Failed, Return[$Failed]];
  makeDiffusionTrainingSample[latent, t, schedule]
];

makeLatentDiffusionTrainingSample[
  encoder_,
  input_,
  t_Integer,
  schedule_Association,
  noise_
] := Module[{latent},
  If[!validateLatentTrainingRequest[t, schedule], Return[$Failed]];
  latent = encodeLatentTrainingInput[encoder, input];
  If[latent === $Failed, Return[$Failed]];
  If[!sameSampleShapeQ[latent, noise],
    Message[makeLatentDiffusionTrainingSample::noise];
    Return[$Failed]
  ];
  makeDiffusionTrainingSample[latent, t, schedule, noise]
];

makeLatentDiffusionTrainingSample[___] := (
  Message[makeLatentDiffusionTrainingSample::args];
  $Failed
);

(* === TESTS === *)

runLatentTests[] := Module[
  {
    passed = 0, assert, schedule, input, encoder, latent, noise, t, sample,
    calls, countingEncoder, firstDraw, secondDraw, replay1, replay2,
    expectedNext, actualNext
  },
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ latent/makeLatentDiffusionTrainingSample: ", label];
    Quit[1]
  ];

  schedule = makeDiffusionSchedule[linearBetaSchedule[6, 0.01, 0.06]];
  input = <|"Values" -> {2., -4., 6.}, "Metadata" -> "source"|>;
  encoder = Function[value, value["Values"]/2.];
  latent = {1., -2., 3.};
  noise = {0.25, -0.5, 1.};
  t = 4;
  sample = makeLatentDiffusionTrainingSample[
    encoder,
    input,
    t,
    schedule,
    noise
  ];
  assert[
    "returns the documented latent training fields",
    Keys[sample] === {"Clean", "Noisy", "Time", "Noise"}
  ];
  assert[
    "stores the encoded latent rather than the source input",
    sample["Clean"] === latent && sample["Time"] === t &&
      sample["Noise"] === noise
  ];
  assert[
    "delegates latent diffusion mathematics to the Gaussian objective",
    sample === makeDiffusionTrainingSample[latent, t, schedule, noise]
  ];
  calls = 0;
  countingEncoder = Function[value, calls++; value["Values"]];
  makeLatentDiffusionTrainingSample[
    countingEncoder,
    input,
    t,
    schedule,
    ConstantArray[0., 3]
  ];
  assert[
    "evaluates the encoder exactly once",
    calls === 1
  ];
  assert[
    "explicit latent noise is deterministic and leaves the random stream untouched",
    sample === makeLatentDiffusionTrainingSample[
      encoder,
      input,
      t,
      schedule,
      noise
    ] &&
      BlockRandom[
        SeedRandom[31415];
        makeLatentDiffusionTrainingSample[
          encoder,
          input,
          t,
          schedule,
          noise
        ];
        RandomReal[]
      ] === BlockRandom[SeedRandom[31415]; RandomReal[]]
  ];
  {firstDraw, secondDraw} = BlockRandom[
    SeedRandom[2468];
    {
      makeLatentDiffusionTrainingSample[encoder, input, t, schedule],
      makeLatentDiffusionTrainingSample[encoder, input, t, schedule]
    }
  ];
  assert[
    "automatic latent noises consume the current random stream",
    firstDraw =!= secondDraw
  ];
  replay1 = BlockRandom[
    SeedRandom[2468];
    makeLatentDiffusionTrainingSample[encoder, input, t, schedule]
  ];
  replay2 = BlockRandom[
    SeedRandom[2468];
    makeLatentDiffusionTrainingSample[encoder, input, t, schedule]
  ];
  assert[
    "resetting the current stream reproduces automatic latent noise",
    replay1 === replay2
  ];
  expectedNext = BlockRandom[
    SeedRandom[86420];
    randomNormalLike[latent];
    RandomReal[]
  ];
  actualNext = BlockRandom[
    SeedRandom[86420];
    makeLatentDiffusionTrainingSample[encoder, input, t, schedule];
    RandomReal[]
  ];
  assert[
    "automatic sampling advances the stream by one latent-shaped draw",
    actualNext === expectedNext
  ];
  assert[
    "scalar and tensor encoder outputs are accepted",
    Dimensions[
      makeLatentDiffusionTrainingSample[
        Function[value, value],
        1.5,
        t,
        schedule,
        0.
      ]["Noisy"]
    ] === {} &&
      Dimensions[
        makeLatentDiffusionTrainingSample[
          Function[value, value],
          ArrayReshape[N[Range[8]], {2, 2, 2}],
          t,
          schedule,
          ConstantArray[0., {2, 2, 2}]
        ]["Noisy"]
      ] === {2, 2, 2}
  ];
  assert[
    "invalid encoder outputs fail",
    Quiet[
      makeLatentDiffusionTrainingSample[
        Function[value, {1., Infinity}],
        input,
        t,
        schedule,
        noise
      ]
    ] === $Failed &&
      Quiet[
        makeLatentDiffusionTrainingSample[
          Function[value, {}],
          input,
          t,
          schedule,
          noise
        ]
      ] === $Failed &&
      Quiet[
        makeLatentDiffusionTrainingSample[
          Function[value, $Failed],
          input,
          t,
          schedule,
          noise
        ]
      ] === $Failed
  ];
  assert[
    "invalid times schedules and latent noises fail",
    Quiet[
      makeLatentDiffusionTrainingSample[encoder, input, 0, schedule, noise]
    ] === $Failed &&
      Quiet[
        makeLatentDiffusionTrainingSample[encoder, input, 7, schedule, noise]
      ] === $Failed &&
      Quiet[
        makeLatentDiffusionTrainingSample[encoder, input, 2., schedule, noise]
      ] === $Failed &&
      Quiet[
        makeLatentDiffusionTrainingSample[
          encoder,
          input,
          t,
          ReplacePart[schedule, "Steps" -> 5],
          noise
        ]
      ] === $Failed &&
      Quiet[
        makeLatentDiffusionTrainingSample[
          encoder,
          input,
          t,
          schedule,
          {0.}
        ]
      ] === $Failed
  ];
  assert[
    "missing and extra positional arguments fail",
    Quiet[makeLatentDiffusionTrainingSample[]] === $Failed &&
      Quiet[
        makeLatentDiffusionTrainingSample[
          encoder,
          input,
          t,
          schedule,
          noise,
          "extra"
        ]
      ] === $Failed
  ];

  Print["✓ latent — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "latent.wl"],
  Stochasma`Private`runLatentTests[]
]
