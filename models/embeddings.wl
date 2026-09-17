BeginPackage["Stochasma`"]

sinusoidalTimeEmbedding::usage =
  "sinusoidalTimeEmbedding[time, dimensions, opts] returns a deterministic sinusoidal embedding of a finite non-negative diffusion time. dimensions must be an Integer greater than or equal to 2. The option \"MaxPeriod\" (default 10000.) controls the lowest generated frequency; odd dimensions are padded with one trailing zero.";

Begin["`Private`"]

finiteRealTimeQ[value_] := Quiet[Check[
  NumberQ[N[value]] &&
    TrueQ[Im[N[value]] == 0] &&
    FreeQ[N[value], Indeterminate | ComplexInfinity | DirectedInfinity],
  False
]];

Options[sinusoidalTimeEmbedding] = {
  "MaxPeriod" -> 10000.
};

sinusoidalTimeEmbedding::args =
  "sinusoidalTimeEmbedding expects a finite non-negative real time, an Integer dimension count greater than or equal to 2, and valid options.";
sinusoidalTimeEmbedding::maxperiod =
  "The \"MaxPeriod\" option must be a finite real number greater than or equal to 1.";

sinusoidalTimeEmbedding[time_, dimensions_, opts___] := Module[
  {given, keys, values, maxPeriod, halfDimensions, frequencies, angles,
    embedding},
  given = {opts};
  If[
    !finiteRealTimeQ[time] || !TrueQ[time >= 0] ||
      !IntegerQ[dimensions] || dimensions < 2 || !OptionQ[given],
    Message[sinusoidalTimeEmbedding::args];
    Return[$Failed]
  ];
  keys = First /@ given;
  If[
    !AllTrue[keys, MemberQ[{"MaxPeriod"}, #] &] ||
      DuplicateFreeQ[keys] === False,
    Message[sinusoidalTimeEmbedding::args];
    Return[$Failed]
  ];
  values = Join[
    Association[Options[sinusoidalTimeEmbedding]],
    Association[given]
  ];
  maxPeriod = values["MaxPeriod"];
  If[
    !finiteRealTimeQ[maxPeriod] || !TrueQ[maxPeriod >= 1],
    Message[sinusoidalTimeEmbedding::maxperiod];
    Return[$Failed]
  ];
  halfDimensions = Quotient[dimensions, 2];
  frequencies = Exp[
    -Log[N[maxPeriod]] Range[0, halfDimensions - 1]/halfDimensions
  ];
  angles = N[time] frequencies;
  embedding = Join[Cos[angles], Sin[angles]];
  If[OddQ[dimensions], Append[embedding, 0.], embedding]
];

End[]

EndPackage[]
