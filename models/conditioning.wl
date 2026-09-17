If[
  DownValues[Stochasma`forwardDiffuse] === {},
  Get[
    FileNameJoin[{
      DirectoryName[$InputFileName], "..", "core", "forward.wl"
    }]
  ]
];

BeginPackage["Stochasma`"]

makeConditionedPredictor::usage =
  "makeConditionedPredictor[conditionedPredictor, conditioning] binds an opaque conditioning value to a callable conditionedPredictor[xt, t, conditioning] and returns a standard predictor[xt, t]. The conditioning value and prediction are passed through without representation-specific validation.";

classifierFreeGuidance::usage =
  "classifierFreeGuidance[unconditionedPrediction, conditionedPrediction, guidanceScale] returns unconditionedPrediction + guidanceScale (conditionedPrediction - unconditionedPrediction). Predictions must be same-shape finite real numeric scalars or non-empty arrays, and guidanceScale must be a finite non-negative real scalar.";

makeClassifierFreeGuidedPredictor::usage =
  "makeClassifierFreeGuidedPredictor[unconditionedPredictor, conditionedPredictor, guidanceScale] returns a standard predictor[xt, t] that evaluates the unconditioned predictor followed by the conditioned predictor exactly once each and combines their outputs with classifierFreeGuidance. guidanceScale must be a finite non-negative real scalar.";

Begin["`Private`"]

makeConditionedPredictor::args =
  "makeConditionedPredictor expects a conditioned predictor callable and one conditioning value.";

makeConditionedPredictor[conditionedPredictor_, conditioning_] := With[
  {
    predictor = conditionedPredictor,
    boundConditioning = conditioning
  },
  Function[{xt, time}, predictor[xt, time, boundConditioning]]
];

makeConditionedPredictor[___] := (
  Message[makeConditionedPredictor::args];
  $Failed
);

classifierFreeGuidance::predictions =
  "Unconditioned and conditioned predictions must be finite real numeric scalars or non-empty arrays with the same shape.";
classifierFreeGuidance::scale =
  "guidanceScale must be a finite non-negative real numeric scalar.";
classifierFreeGuidance::result =
  "The guided prediction is not a finite real numeric scalar or non-empty array.";
classifierFreeGuidance::args =
  "classifierFreeGuidance expects unconditioned and conditioned predictions followed by a finite non-negative real guidance scale.";

classifierFreeGuidance[
  unconditionedPrediction_,
  conditionedPrediction_,
  guidanceScale_
] := Module[{guidedPrediction},
  If[
    !sameSampleShapeQ[unconditionedPrediction, conditionedPrediction],
    Message[classifierFreeGuidance::predictions];
    Return[$Failed]
  ];
  If[
    !finiteRealNumberQ[guidanceScale] || !TrueQ[guidanceScale >= 0],
    Message[classifierFreeGuidance::scale];
    Return[$Failed]
  ];
  guidedPrediction = Quiet[Check[
    unconditionedPrediction +
      guidanceScale (conditionedPrediction - unconditionedPrediction),
    $Failed
  ]];
  If[!realNumericSampleQ[guidedPrediction],
    Message[classifierFreeGuidance::result];
    Return[$Failed]
  ];
  guidedPrediction
];

classifierFreeGuidance[___] := (
  Message[classifierFreeGuidance::args];
  $Failed
);

makeClassifierFreeGuidedPredictor::scale =
  "guidanceScale must be a finite non-negative real numeric scalar.";
makeClassifierFreeGuidedPredictor::args =
  "makeClassifierFreeGuidedPredictor expects unconditioned and conditioned predictor callables followed by a finite non-negative real guidance scale.";

makeClassifierFreeGuidedPredictor[
  unconditionedPredictor_,
  conditionedPredictor_,
  guidanceScale_
] := Module[{},
  If[
    !finiteRealNumberQ[guidanceScale] || !TrueQ[guidanceScale >= 0],
    Message[makeClassifierFreeGuidedPredictor::scale];
    Return[$Failed]
  ];
  With[
    {
      unconditioned = unconditionedPredictor,
      conditioned = conditionedPredictor,
      scale = guidanceScale
    },
    Function[{xt, time},
      Module[{unconditionedPrediction, conditionedPrediction},
        unconditionedPrediction = unconditioned[xt, time];
        If[
          MemberQ[{$Failed, $Aborted}, unconditionedPrediction],
          $Failed,
          conditionedPrediction = conditioned[xt, time];
          If[
            MemberQ[{$Failed, $Aborted}, conditionedPrediction],
            $Failed,
            classifierFreeGuidance[
              unconditionedPrediction,
              conditionedPrediction,
              scale
            ]
          ]
        ]
      ]
    ]
  ]
];

makeClassifierFreeGuidedPredictor[___] := (
  Message[makeClassifierFreeGuidedPredictor::args];
  $Failed
);

End[]

EndPackage[]
