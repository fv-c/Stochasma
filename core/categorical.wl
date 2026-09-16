BeginPackage["Stochasma`"]

uniformCategoricalTransitionKernel::usage =
  "uniformCategoricalTransitionKernel[categoryCount, beta] returns the row-stochastic transition matrix (1 - beta) I + beta U for uniform categorical corruption over categoryCount states, where U has every entry 1/categoryCount and 0 <= beta <= 1.";

Begin["`Private`"]

categoricalFiniteRealNumberQ[value_] := Quiet[Check[
  NumberQ[N[value]] &&
    TrueQ[Im[N[value]] == 0] &&
    FreeQ[N[value], Indeterminate | ComplexInfinity | DirectedInfinity],
  False
]];

uniformCategoricalTransitionKernel::args =
  "uniformCategoricalTransitionKernel expects an Integer category count of at least 2 and a finite real beta value.";
uniformCategoricalTransitionKernel::range =
  "Beta must satisfy 0 <= beta <= 1.";

uniformCategoricalTransitionKernel[categoryCount_, beta_] := Module[
  {numericBeta, uniformKernel},
  If[
    !IntegerQ[categoryCount] || categoryCount < 2 ||
      !categoricalFiniteRealNumberQ[beta],
    Message[uniformCategoricalTransitionKernel::args];
    Return[$Failed]
  ];
  If[!TrueQ[0 <= beta <= 1],
    Message[uniformCategoricalTransitionKernel::range];
    Return[$Failed]
  ];
  numericBeta = N[beta];
  uniformKernel = ConstantArray[
    1./categoryCount,
    {categoryCount, categoryCount}
  ];
  (1 - numericBeta) IdentityMatrix[categoryCount] +
    numericBeta uniformKernel
];

uniformCategoricalTransitionKernel[___] := (
  Message[uniformCategoricalTransitionKernel::args];
  $Failed
);

(* === TESTS === *)

runCategoricalTests[] := Module[
  {passed = 0, assert, kernel, expected, permutation},
  assert[label_, expression_] := If[TrueQ[expression],
    passed++,
    Print["✗ categorical/uniformCategoricalTransitionKernel: ", label];
    Quit[1]
  ];

  kernel = uniformCategoricalTransitionKernel[4, 0.2];
  expected = ConstantArray[0.05, {4, 4}] + 0.8 IdentityMatrix[4];
  assert[
    "matches the uniform-corruption formula",
    Max[Abs[Flatten[kernel - expected]]] < 10^-14
  ];
  assert[
    "is row-stochastic and non-negative",
    Max[Abs[Total[kernel, {2}] - ConstantArray[1., 4]]] < 10^-14 &&
      Min[kernel] >= 0
  ];
  assert[
    "preserves the uniform distribution",
    Max[Abs[ConstantArray[1./4, 4] . kernel - ConstantArray[1./4, 4]]] <
      10^-14
  ];
  assert[
    "has identity and uniform endpoint kernels",
    uniformCategoricalTransitionKernel[3, 0] === N[IdentityMatrix[3]] &&
      uniformCategoricalTransitionKernel[3, 1] ===
        ConstantArray[1./3, {3, 3}]
  ];
  permutation = IdentityMatrix[4][[{3, 1, 4, 2}]];
  assert[
    "is invariant under category relabeling",
    Max[Abs[Flatten[
      permutation . kernel . Transpose[permutation] - kernel
    ]]] < 10^-14
  ];
  assert[
    "rejects invalid category counts, beta values, and arity",
    Quiet[uniformCategoricalTransitionKernel[1, 0.2]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3.5, 0.2]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3, -0.1]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3, 1.1]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3, Infinity]] === $Failed &&
      Quiet[uniformCategoricalTransitionKernel[3]] === $Failed
  ];

  Print["✓ categorical — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "categorical.wl"],
  Stochasma`Private`runCategoricalTests[]
]
