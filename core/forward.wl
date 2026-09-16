BeginPackage["Stochasma`"]

Begin["`Private`"]

(* === TESTS === *)

runForwardTests[] := Module[{passed = 0},
  Print["✓ forward — ", passed, " tests passed"];
  passed
];

End[]

EndPackage[]

If[
  MemberQ[FileNameTake /@ Select[$ScriptCommandLine, StringQ], "forward.wl"],
  Stochasma`Private`runForwardTests[]
]
