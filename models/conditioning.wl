BeginPackage["Stochasma`"]

makeConditionedPredictor::usage =
  "makeConditionedPredictor[conditionedPredictor, conditioning] binds an opaque conditioning value to a callable conditionedPredictor[xt, t, conditioning] and returns a standard predictor[xt, t]. The conditioning value and prediction are passed through without representation-specific validation.";

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

End[]

EndPackage[]
