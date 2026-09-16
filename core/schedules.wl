BeginPackage["Stochasma`"]

Begin["`Private`"]

(* === TESTS === *)

runSchedulesTests[] := Module[{passed = 0},
  Print["✓ schedules — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "schedules.wl"],
  Stochasma`Private`runSchedulesTests[]
]
