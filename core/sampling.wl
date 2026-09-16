BeginPackage["Stochasma`"]

Begin["`Private`"]

(* === TESTS === *)

runSamplingTests[] := Module[{passed = 0},
  Print["✓ sampling — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "sampling.wl"],
  Stochasma`Private`runSamplingTests[]
]
