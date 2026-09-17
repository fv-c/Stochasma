BeginPackage["Stochasma`"]

Begin["`Private`"]

randomNormalLike[sample_] := If[
  ArrayQ[sample],
  ArrayReshape[
    RandomVariate[NormalDistribution[0, 1], Times @@ Dimensions[sample]],
    Dimensions[sample]
  ],
  RandomVariate[NormalDistribution[0, 1]]
];

End[]

EndPackage[]
