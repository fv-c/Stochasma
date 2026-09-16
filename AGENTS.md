# Stochasma — Codex Agent Instructions

## Context hierarchy

Read in this order and stop when sufficient:

1. `ARCHITECTURE.md`
2. `MODULE_INDEX.md`
3. Target source file

Never read files outside the current task scope.

## Implementation

- Keep one logical module per `.wl` file.
- Use `BeginPackage["Stochasma`"]` / `EndPackage[]`.
- Define a `::usage` message before every public function.
- Keep functions pure and never mutate inputs.
- Implement one function at a time.
- Put module tests below `(* === TESTS === *)`.
- Run module tests after every function.
- Do not add application-domain dependencies to the diffusion core.

## Reporting

- Success: `✓ module/function — N tests passed`
- Failure: `✗ function: one-line reason`

## Commits

Create one commit per function or coherent logical unit.

Format: `[module] description`
