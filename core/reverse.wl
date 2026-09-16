BeginPackage["Stochasma`"]

Begin["`Private`"]

(* === TESTS === *)

runReverseTests[] := Module[{passed = 0},
  Print["✓ reverse — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "reverse.wl"],
  Stochasma`Private`runReverseTests[]
]
