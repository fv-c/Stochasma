---
name: run-tests
description: Run or debug Stochasma Wolfram Language module tests and the headless aggregate suite, preserving mathematical assertions and module scope. Do not use for net-new feature implementation.
---

# Run Stochasma Tests

## Module workflow

1. Read the target `.wl` module and only its direct Stochasma dependencies.
2. Identify the first failing assertion.
3. Fix the implementation; change a test only when its mathematical premise or
   Wolfram Language expression is demonstrably wrong.
4. Re-run `wolframscript -file <module.wl>`.
5. Report `✓ module/function — N tests passed` or
   `✗ function: one-line reason`.

## Aggregate workflow

At milestone boundaries, run:

```bash
wolframscript -file tests/run_all_tests.wls
```

Also verify local paclet loading in a clean Wolfram process through
`PacletDirectoryLoad` followed by `Needs["Stochasma`"]`.

## Test quality

- Assert canonical DDPM formulas, derived identities, time boundaries, shape
  preservation, and deterministic explicit-noise behavior.
- Cover scalar, vector, matrix, and higher-rank tensor samples where relevant.
- Test invalid ranges and malformed schedules through `$Failed`.
- Use explicit noise for formula checks and fixed seeds only for reproducibility
  checks.
- Keep tests headless and free of application-domain fixtures.
