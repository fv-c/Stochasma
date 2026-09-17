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

makeConditionedDiffusionTrainingBatch::usage =
  "makeConditionedDiffusionTrainingBatch[cleanSamples, conditioningValues, schedule, opts] creates a diffusion training batch and appends one opaque \"Conditioning\" value to each training association. conditioningValues must be a list matching cleanSamples. The function accepts the same \"Seed\", \"Times\", and \"Noises\" options as makeDiffusionTrainingBatch and introduces no additional randomness or conditioning sentinel.";

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

Options[makeConditionedDiffusionTrainingBatch] =
  Options[makeDiffusionTrainingBatch];

makeConditionedDiffusionTrainingBatch::conditioning =
  "conditioningValues must be a list with the same length as cleanSamples.";
makeConditionedDiffusionTrainingBatch::args =
  "makeConditionedDiffusionTrainingBatch expects a non-empty list of clean samples, a matching list of opaque conditioning values, a diffusion schedule, and optional rules.";

makeConditionedDiffusionTrainingBatch[
  cleanSamples_List,
  conditioningValues_List,
  schedule_Association,
  opts___
] := Module[{batch},
  If[Length[conditioningValues] =!= Length[cleanSamples],
    Message[makeConditionedDiffusionTrainingBatch::conditioning];
    Return[$Failed]
  ];
  batch = makeDiffusionTrainingBatch[cleanSamples, schedule, opts];
  If[batch === $Failed, Return[$Failed]];
  MapThread[
    Append[#1, "Conditioning" -> #2] &,
    {batch, conditioningValues}
  ]
];

makeConditionedDiffusionTrainingBatch[___] := (
  Message[makeConditionedDiffusionTrainingBatch::args];
  $Failed
);

End[]

EndPackage[]
