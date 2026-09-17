If[
  DownValues[Stochasma`makeDiffusionTrainingSample] === {},
  Get[
    FileNameJoin[{
      DirectoryName[$InputFileName], "..", "core", "objectives.wl"
    }]
  ]
];
If[
  DownValues[Stochasma`ddpmSample] === {},
  Get[
    FileNameJoin[{
      DirectoryName[$InputFileName], "..", "core", "sampling.wl"
    }]
  ]
];
If[
  DownValues[Stochasma`ddimSample] === {},
  Get[
    FileNameJoin[{
      DirectoryName[$InputFileName], "..", "core", "ddim.wl"
    }]
  ]
];

BeginPackage["Stochasma`"]

makeLatentDiffusionTrainingSample::usage =
  "makeLatentDiffusionTrainingSample[encoder, input, t, schedule] evaluates encoder[input] once and creates an epsilon-prediction training example in the resulting latent space. The five-argument form uses explicit noise matching the encoded latent. The returned \"Clean\" field is the encoded latent.";

latentDiffusionSample::usage =
  "latentDiffusionSample[decoder, predictor, initialLatent, schedule, opts] runs a selected Gaussian sampler in latent space and evaluates decoder[z0] exactly once after successful sampling. \"Sampler\" is ddpmSample by default and may also be ddimSample; \"SamplerOptions\" is a list of rules passed only to that sampler. When the sampler returns a trajectory, the wrapper returns \"Sample\", \"LatentSample\", and \"LatentTrajectory\", and preserves \"Timesteps\" when the sampler supplies them.";

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

Options[latentDiffusionSample] = {
  "Sampler" -> ddpmSample,
  "SamplerOptions" -> {}
};

latentDiffusionSample::decoder =
  "The decoder failed while evaluating the final latent sample.";
latentDiffusionSample::sampler =
  "The \"Sampler\" option must be ddpmSample or ddimSample.";
latentDiffusionSample::opts =
  "Options must use each of \"Sampler\" and \"SamplerOptions\" at most once; \"SamplerOptions\" must be a list of rules accepted by the selected sampler.";
latentDiffusionSample::args =
  "latentDiffusionSample expects a decoder callable, a predictor callable, an initial latent sample, a diffusion schedule, and optional wrapper rules.";

latentSamplerQ[sampler_] := sampler === ddpmSample || sampler === ddimSample;

latentDiffusionSample[
  decoder_,
  predictor_,
  initialLatentNoise_,
  schedule_Association,
  opts___
] := Module[
  {
    given, keys, values, sampler, samplerOptions, latentResult,
    latentSample, decodedSample
  },
  given = {opts};
  If[!OptionQ[given],
    Message[latentDiffusionSample::opts];
    Return[$Failed]
  ];
  keys = First /@ given;
  If[
    !AllTrue[keys, MemberQ[{"Sampler", "SamplerOptions"}, #] &] ||
      DuplicateFreeQ[keys] === False,
    Message[latentDiffusionSample::opts];
    Return[$Failed]
  ];
  values = Join[
    Association[Options[latentDiffusionSample]],
    Association[given]
  ];
  sampler = values["Sampler"];
  samplerOptions = values["SamplerOptions"];
  If[!latentSamplerQ[sampler],
    Message[latentDiffusionSample::sampler];
    Return[$Failed]
  ];
  If[!ListQ[samplerOptions] || !OptionQ[samplerOptions],
    Message[latentDiffusionSample::opts];
    Return[$Failed]
  ];
  latentResult = sampler[
    predictor,
    initialLatentNoise,
    schedule,
    Sequence @@ samplerOptions
  ];
  If[latentResult === $Failed, Return[$Failed]];
  latentSample = If[
    AssociationQ[latentResult],
    latentResult["Sample"],
    latentResult
  ];
  decodedSample = Check[decoder[latentSample], $Failed];
  If[MemberQ[{$Failed, $Aborted}, decodedSample],
    Message[latentDiffusionSample::decoder];
    Return[$Failed]
  ];
  If[
    AssociationQ[latentResult],
    Join[
      <|
        "Sample" -> decodedSample,
        "LatentSample" -> latentSample,
        "LatentTrajectory" -> latentResult["Trajectory"]
      |>,
      If[
        KeyExistsQ[latentResult, "Timesteps"],
        <|"Timesteps" -> latentResult["Timesteps"]|>,
        <||>
      ]
    ],
    decodedSample
  ]
];

latentDiffusionSample[___] := (
  Message[latentDiffusionSample::args];
  $Failed
);

End[]

EndPackage[]
