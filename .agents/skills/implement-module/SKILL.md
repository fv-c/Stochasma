---
name: implement-module
description: Implement or extend a Wolfram Language module in Stochasma one public function at a time, with local mathematical tests and a commit after each passing unit. Do not use for tests-only work or unrelated repositories.
---

# Implement a Stochasma Module

## Protocol

1. Read `ARCHITECTURE.md`, then the target section of `MODULE_INDEX.md`.
2. Read only the target `.wl` module and its direct Stochasma dependencies.
3. Add the public `::usage` message before the implementation.
4. Implement one function without mutable global state or input mutation.
5. Add property-based or formula-based checks in the module's
   `(* === TESTS === *)` block.
6. Run `wolframscript -file <module.wl>` and do not proceed until it passes.
7. Report `✓ module/function — N tests passed`.
8. Commit the passing unit as `[module] description`.

Run `wolframscript -file tests/run_all_tests.wls` only at a milestone or when a
cross-module change requires aggregation.

## Core boundaries

- Keep the mathematical DDPM core independent of neural-network architecture.
- Accept generic real-valued numeric samples; do not assume images or music.
- Keep `t = 0` as the clean sample and schedule arrays at logical times `1..T`.
- Isolate generated randomness with `BlockRandom` and retain explicit-noise
  overloads for stochastic operations.
- Reject invalid input with `Message` and `$Failed`; do not silently repair it.
