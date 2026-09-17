BeginPackage["Stochasma`"]

makeWolframNetPredictor::usage =
  "makeWolframNetPredictor[network, inputAdapter] returns a function called as predictor[xt, t] that adapts inputs to a Wolfram NetChain or NetGraph. By default, the network receives <|\"Sample\" -> xt, \"Time\" -> N[t]|>. An explicit inputAdapter[xt, t] may construct any input accepted by the network. The network is responsible for producing an epsilon prediction compatible with the sampler protocol; ddpmSample validates prediction shape and finiteness.";

Begin["`Private`"]

makeWolframNetPredictor::args =
  "makeWolframNetPredictor expects a NetChain or NetGraph and, optionally, a callable input adapter.";
makeWolframNetPredictor::adapter =
  "The input adapter must be Automatic or a callable expression.";
makeWolframNetPredictor::input =
  "The input adapter failed to construct a network input from xt and t.";
makeWolframNetPredictor::network =
  "The Wolfram neural network failed to evaluate the adapted input.";

clearlyNonCallableAdapterQ[value_] := NumberQ[value] || StringQ[value];

makeWolframNetPredictor[
  network : (_NetChain | _NetGraph),
  inputAdapter_ : Automatic
] := Module[{adapter},
  If[clearlyNonCallableAdapterQ[inputAdapter],
    Message[makeWolframNetPredictor::adapter];
    Return[$Failed]
  ];
  adapter = If[
    inputAdapter === Automatic,
    Function[{xt, time}, <|"Sample" -> xt, "Time" -> N[time]|>],
    inputAdapter
  ];
  Function[{xt, time},
    Module[{networkInput, prediction},
      networkInput = Quiet[Check[adapter[xt, time], $Failed]];
      If[networkInput === $Failed,
        Message[makeWolframNetPredictor::input];
        $Failed,
        prediction = Quiet[Check[network[networkInput], $Failed]];
        If[prediction === $Failed,
          Message[makeWolframNetPredictor::network];
          $Failed,
          prediction
        ]
      ]
    ]
  ]
];

makeWolframNetPredictor[___] := (
  Message[makeWolframNetPredictor::args];
  $Failed
);

End[]

EndPackage[]
