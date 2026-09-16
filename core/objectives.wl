BeginPackage["Stochasma`"]

Begin["`Private`"]

(* === TESTS === *)

runObjectivesTests[] := Module[{passed = 0},
  Print["✓ objectives — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "objectives.wl"],
  Stochasma`Private`runObjectivesTests[]
]
