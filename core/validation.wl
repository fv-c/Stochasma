BeginPackage["Stochasma`"]

Begin["`Private`"]

finiteRealNumberQ[value_] := Quiet[Check[
  NumberQ[N[value]] &&
    TrueQ[Im[N[value]] == 0] &&
    FreeQ[N[value], Indeterminate | ComplexInfinity | DirectedInfinity],
  False
]];

realNumericSampleQ[value_] := finiteRealNumberQ[value] ||
  (ArrayQ[value, _, finiteRealNumberQ] && Flatten[value] =!= {});

sameSampleShapeQ[left_, right_] :=
  realNumericSampleQ[left] && realNumericSampleQ[right] &&
    Dimensions[left] === Dimensions[right];

End[]

EndPackage[]
