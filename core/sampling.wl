If[
  DownValues[Stochasma`reverseDiffuseStep] === {},
  Get[FileNameJoin[{DirectoryName[$InputFileName], "reverse.wl"}]]
];

BeginPackage["Stochasma`"]

ddpmSample::usage =
  "ddpmSample[predictor, initialNoise, schedule, opts] applies predictor[xt, t] from t = T down to 1 and returns x0. \"ReturnTrajectory\" -> True returns <|\"Sample\" -> x0, \"Trajectory\" -> {xT, ..., x0}|>. With \"Noises\" -> Automatic, \"Seed\" -> Automatic uses the current random stream and an Integer seed is reproducible and locally isolated. An explicit length-T \"Noises\" list is deterministic and takes precedence over \"Seed\"; its t = 1 entry is validated but not added.";

Begin["`Private`"]

Options[ddpmSample] = {
  "ReturnTrajectory" -> False,
  "Seed" -> Automatic,
  "Noises" -> Automatic
};

ddpmSample::sample =
  "initialNoise must be a finite real numeric scalar or non-empty array.";
ddpmSample::schedule =
  "The supplied diffusion schedule is malformed or internally inconsistent.";
ddpmSample::predictor =
  "The predictor must return a finite real epsilon prediction with the same shape as xt at every time.";
ddpmSample::opts =
  "Options must use each of \"ReturnTrajectory\", \"Seed\", and \"Noises\" at most once with valid values.";
ddpmSample::noises =
  "Explicit \"Noises\" must be a length-T list whose entries have the same finite real shape as initialNoise.";
ddpmSample::args =
  "ddpmSample expects a predictor callable, initial noise, a diffusion schedule, and optional rules.";

runDDPMSampling[
  predictor_,
  initialNoise_,
  schedule_Association,
  returnTrajectory_,
  noises_
] := Module[{state, trajectory, predictedNoise, stepNoise, t, failed},
  state = initialNoise;
  trajectory = {initialNoise};
  failed = False;
  Do[
    predictedNoise = Check[predictor[state, t], $Failed];
    If[
      predictedNoise === $Failed ||
        !sameSampleShapeQ[state, predictedNoise],
      Message[ddpmSample::predictor];
      failed = True;
      Break[]
    ];
    stepNoise = Which[
      ListQ[noises], noises[[t]],
      t == 1, 0. state,
      True, randomNormalLike[state]
    ];
    state = reverseDiffuseStepValidated[
      state,
      t,
      predictedNoise,
      schedule,
      stepNoise
    ];
    If[state === $Failed,
      failed = True;
      Break[]
    ];
    trajectory = Append[trajectory, state],
    {t, schedule["Steps"], 1, -1}
  ];
  If[failed, Return[$Failed]];
  If[
    TrueQ[returnTrajectory],
    <|"Sample" -> state, "Trajectory" -> trajectory|>,
    state
  ]
];

ddpmSample[
  predictor_,
  initialNoise_,
  schedule_Association,
  opts___
] := Module[
  {given, keys, values, returnTrajectory, seed, noises},
  given = {opts};
  If[!OptionQ[given],
    Message[ddpmSample::opts];
    Return[$Failed]
  ];
  keys = First /@ given;
  If[
    !AllTrue[
      keys,
      MemberQ[{"ReturnTrajectory", "Seed", "Noises"}, #] &
    ] || DuplicateFreeQ[keys] === False,
    Message[ddpmSample::opts];
    Return[$Failed]
  ];
  values = Join[Association[Options[ddpmSample]], Association[given]];
  returnTrajectory = values["ReturnTrajectory"];
  seed = values["Seed"];
  noises = values["Noises"];
  If[!MemberQ[{True, False}, returnTrajectory] ||
      !(seed === Automatic || IntegerQ[seed]),
    Message[ddpmSample::opts];
    Return[$Failed]
  ];
  If[!realNumericSampleQ[initialNoise],
    Message[ddpmSample::sample];
    Return[$Failed]
  ];
  If[!diffusionScheduleQ[schedule],
    Message[ddpmSample::schedule];
    Return[$Failed]
  ];
  If[
    noises =!= Automatic &&
      (!ListQ[noises] || Length[noises] =!= schedule["Steps"] ||
        !AllTrue[noises, sameSampleShapeQ[initialNoise, #] &]),
    Message[ddpmSample::noises];
    Return[$Failed]
  ];
  Which[
    ListQ[noises],
      runDDPMSampling[
        predictor, initialNoise, schedule, returnTrajectory, noises
      ],
    IntegerQ[seed],
      BlockRandom[
        SeedRandom[seed];
        runDDPMSampling[
          predictor, initialNoise, schedule, returnTrajectory, Automatic
        ]
      ],
    True,
      runDDPMSampling[
        predictor, initialNoise, schedule, returnTrajectory, Automatic
      ]
  ]
];

ddpmSample[___] := (
  Message[ddpmSample::args];
  $Failed
);

End[]

EndPackage[]
