Begin["Stochasma`Private`"]

runLatentTests[] := Module[
  {
    passed = 0, assert, schedule, input, encoder, latent, noise, t, sample,
    calls, countingEncoder, firstDraw, secondDraw, replay1, replay2,
    expectedNext, actualNext, initialLatentNoise, predictor, stepNoises,
    decoder, directLatent, decodedSample, trajectoryResult, decodeCalls,
    seenLatent, countingDecoder, seeded1, seeded2, directNext, wrappedNext,
    ddimTimesteps, ddimNoises, directDDIM, decodedDDIM, ddimTrajectory,
    ddimDecodeCalls, ddimCountingDecoder, ddimFullTimesteps,
    directFullDDIM, decodedFullDDIM
  },
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ latent: ", label];
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

  initialLatentNoise = {0.5, -1., 1.5};
  predictor = Function[{state, time}, 0. state + 0.01 time];
  stepNoises = Table[
    ConstantArray[N[time]/10., 3],
    {time, schedule["Steps"]}
  ];
  decoder = Function[value, <|"Decoded" -> (2. value)|>];
  directLatent = ddpmSample[
    predictor,
    initialLatentNoise,
    schedule,
    "Noises" -> stepNoises
  ];
  decodedSample = latentDiffusionSample[
    decoder,
    predictor,
    initialLatentNoise,
    schedule,
    "SamplerOptions" -> {"Noises" -> stepNoises}
  ];
  assert[
    "decodes the final DDPM latent sample",
    decodedSample === decoder[directLatent]
  ];
  trajectoryResult = latentDiffusionSample[
    decoder,
    predictor,
    initialLatentNoise,
    schedule,
    "SamplerOptions" -> {
      "Noises" -> stepNoises,
      "ReturnTrajectory" -> True
    }
  ];
  assert[
    "trajectory mode separates decoded output from latent state",
    Keys[trajectoryResult] === {
      "Sample", "LatentSample", "LatentTrajectory"
    } &&
      trajectoryResult["Sample"] === decoder[directLatent] &&
      trajectoryResult["LatentSample"] === directLatent &&
      First[trajectoryResult["LatentTrajectory"]] === initialLatentNoise &&
      Last[trajectoryResult["LatentTrajectory"]] === directLatent &&
      Length[trajectoryResult["LatentTrajectory"]] ===
        schedule["Steps"] + 1
  ];
  decodeCalls = 0;
  seenLatent = None;
  countingDecoder = Function[value,
    decodeCalls++;
    seenLatent = value;
    <|"Decoded" -> value|>
  ];
  latentDiffusionSample[
    countingDecoder,
    predictor,
    initialLatentNoise,
    schedule,
    "SamplerOptions" -> {
      "Noises" -> stepNoises,
      "ReturnTrajectory" -> True
    }
  ];
  assert[
    "evaluates the decoder exactly once on z0 even in trajectory mode",
    decodeCalls === 1 && seenLatent === directLatent
  ];
  assert[
    "separates wrapper options from selected-sampler options",
    Options[latentDiffusionSample] === {
      "Sampler" -> ddpmSample,
      "SamplerOptions" -> {}
    }
  ];
  assert[
    "explicit reverse noises remain deterministic and preserve the random stream",
    decodedSample === latentDiffusionSample[
      decoder,
      predictor,
      initialLatentNoise,
      schedule,
      "SamplerOptions" -> {
        "Seed" -> 999,
        "Noises" -> stepNoises
      }
    ] &&
      BlockRandom[
        SeedRandom[97531];
        latentDiffusionSample[
          decoder,
          predictor,
          initialLatentNoise,
          schedule,
          "SamplerOptions" -> {"Noises" -> stepNoises}
        ];
        RandomReal[]
      ] === BlockRandom[SeedRandom[97531]; RandomReal[]]
  ];
  seeded1 = latentDiffusionSample[
    decoder,
    predictor,
    initialLatentNoise,
    schedule,
    "SamplerOptions" -> {"Seed" -> 13579}
  ];
  seeded2 = latentDiffusionSample[
    decoder,
    predictor,
    initialLatentNoise,
    schedule,
    "SamplerOptions" -> {"Seed" -> 13579}
  ];
  assert[
    "integer seeds reproduce decoded samples and remain locally isolated",
    seeded1 === seeded2 &&
      BlockRandom[
        SeedRandom[86420];
        latentDiffusionSample[
          decoder,
          predictor,
          initialLatentNoise,
          schedule,
          "SamplerOptions" -> {"Seed" -> 13579}
        ];
        RandomReal[]
      ] === BlockRandom[SeedRandom[86420]; RandomReal[]]
  ];
  directNext = BlockRandom[
    SeedRandom[112233];
    ddpmSample[predictor, initialLatentNoise, schedule];
    RandomReal[]
  ];
  wrappedNext = BlockRandom[
    SeedRandom[112233];
    latentDiffusionSample[
      decoder,
      predictor,
      initialLatentNoise,
      schedule
    ];
    RandomReal[]
  ];
  assert[
    "automatic sampling consumes exactly the DDPM random stream",
    wrappedNext === directNext
  ];

  ddimTimesteps = {6, 4, 2};
  ddimNoises = ConstantArray[0. initialLatentNoise, Length[ddimTimesteps]];
  directDDIM = ddimSample[
    predictor,
    initialLatentNoise,
    schedule,
    "Eta" -> 0.,
    "Timesteps" -> ddimTimesteps,
    "Noises" -> ddimNoises,
    "ReturnTrajectory" -> True
  ];
  decodedDDIM = latentDiffusionSample[
    decoder,
    predictor,
    initialLatentNoise,
    schedule,
    "Sampler" -> ddimSample,
    "SamplerOptions" -> {
      "Eta" -> 0.,
      "Timesteps" -> ddimTimesteps,
      "Noises" -> ddimNoises,
      "ReturnTrajectory" -> True
    }
  ];
  assert[
    "DDIM subsampling decodes the direct final latent and preserves trajectory metadata",
    Keys[decodedDDIM] === {
      "Sample", "LatentSample", "LatentTrajectory", "Timesteps"
    } &&
      decodedDDIM["Sample"] === decoder[directDDIM["Sample"]] &&
      decodedDDIM["LatentSample"] === directDDIM["Sample"] &&
      decodedDDIM["LatentTrajectory"] === directDDIM["Trajectory"] &&
      decodedDDIM["Timesteps"] === directDDIM["Timesteps"] &&
      decodedDDIM["Timesteps"] === Append[ddimTimesteps, 0]
  ];
  ddimFullTimesteps = Reverse[Range[schedule["Steps"]]];
  directFullDDIM = ddimSample[
    predictor,
    initialLatentNoise,
    schedule,
    "Eta" -> 0.,
    "ReturnTrajectory" -> True
  ];
  decodedFullDDIM = latentDiffusionSample[
    decoder,
    predictor,
    initialLatentNoise,
    schedule,
    "Sampler" -> ddimSample,
    "SamplerOptions" -> {
      "Eta" -> 0.,
      "ReturnTrajectory" -> True
    }
  ];
  assert[
    "DDIM full timesteps are retained without inventing wrapper metadata",
    decodedFullDDIM["Sample"] === decoder[directFullDDIM["Sample"]] &&
      decodedFullDDIM["LatentTrajectory"] ===
        directFullDDIM["Trajectory"] &&
      decodedFullDDIM["Timesteps"] ===
        Append[ddimFullTimesteps, 0]
  ];
  ddimDecodeCalls = 0;
  ddimCountingDecoder = Function[value,
    ddimDecodeCalls++;
    <|"Decoded" -> value|>
  ];
  latentDiffusionSample[
    ddimCountingDecoder,
    predictor,
    initialLatentNoise,
    schedule,
    "Sampler" -> ddimSample,
    "SamplerOptions" -> {
      "Eta" -> 0.,
      "Timesteps" -> ddimTimesteps,
      "ReturnTrajectory" -> True
    }
  ];
  assert[
    "DDIM trajectory mode evaluates the decoder exactly once",
    ddimDecodeCalls === 1
  ];
  assert[
    "deterministic DDIM wrapping leaves the caller random stream unchanged",
    BlockRandom[
      SeedRandom[556677];
      latentDiffusionSample[
        decoder,
        predictor,
        initialLatentNoise,
        schedule,
        "Sampler" -> ddimSample,
        "SamplerOptions" -> {
          "Eta" -> 0.,
          "Timesteps" -> ddimTimesteps
        }
      ];
      RandomReal[]
    ] === BlockRandom[SeedRandom[556677]; RandomReal[]]
  ];

  decodeCalls = 0;
  assert[
    "sampling failures do not invoke the decoder",
    Quiet[
      latentDiffusionSample[
        Function[value, decodeCalls++; value],
        Function[{state, time}, {0.}],
        initialLatentNoise,
        schedule,
        "SamplerOptions" -> {"Noises" -> stepNoises}
      ]
    ] === $Failed && decodeCalls === 0
  ];
  assert[
    "decoder failures return Failed",
    Quiet[
      latentDiffusionSample[
        Function[value, $Failed],
        predictor,
        initialLatentNoise,
        schedule,
        "SamplerOptions" -> {"Noises" -> stepNoises}
      ]
    ] === $Failed
  ];
  assert[
    "invalid sampler inputs and wrapper options fail",
    Quiet[
      latentDiffusionSample[
        decoder,
        predictor,
        {0., Infinity, 0.},
        schedule
      ]
    ] === $Failed &&
      Quiet[
        latentDiffusionSample[
          decoder,
          predictor,
          initialLatentNoise,
          ReplacePart[schedule, "Steps" -> 5]
        ]
      ] === $Failed &&
      Quiet[
        latentDiffusionSample[
          decoder,
          predictor,
          initialLatentNoise,
          schedule,
          "Unknown" -> True
        ]
      ] === $Failed &&
      Quiet[
        latentDiffusionSample[
          decoder,
          predictor,
          initialLatentNoise,
          schedule,
          "Seed" -> 1
        ]
      ] === $Failed &&
      Quiet[
        latentDiffusionSample[
          decoder,
          predictor,
          initialLatentNoise,
          schedule,
          "Sampler" -> ddpmSample,
          "Sampler" -> ddimSample
        ]
      ] === $Failed
  ];
  assert[
    "invalid sampler selections and sampler-option sets fail",
    Quiet[
      latentDiffusionSample[
        decoder,
        predictor,
        initialLatentNoise,
        schedule,
        "Sampler" -> Identity
      ]
    ] === $Failed &&
      Quiet[
        latentDiffusionSample[
          decoder,
          predictor,
          initialLatentNoise,
          schedule,
          "SamplerOptions" -> "invalid"
        ]
      ] === $Failed &&
      Quiet[
        latentDiffusionSample[
          decoder,
          predictor,
          initialLatentNoise,
          schedule,
          "SamplerOptions" -> {"Unknown" -> True}
        ]
      ] === $Failed &&
      Quiet[
        latentDiffusionSample[
          decoder,
          predictor,
          initialLatentNoise,
          schedule,
          "SamplerOptions" -> {"Seed" -> 1, "Seed" -> 2}
        ]
      ] === $Failed
  ];
  assert[
    "missing sampler arguments fail",
    Quiet[latentDiffusionSample[]] === $Failed &&
      Quiet[
        latentDiffusionSample[
          decoder,
          predictor,
          initialLatentNoise
        ]
      ] === $Failed
  ];

  Print["✓ latent — ", passed, " tests passed"];
  passed
];

End[]
