# Stochasma

Stochasma is a general-purpose Wolfram Language paclet for diffusion-model
primitives. Version 0.1 implements Gaussian diffusion / DDPM while keeping
neural-network models and application adapters external. Version 0.2 adds
model-facing utilities without coupling the core to a network architecture.

The core operates on finite real numeric scalars and arrays of arbitrary rank.
It makes no image, music, or other application-domain assumptions and has no
Python dependency.

## Status

The unreleased development version `0.2.0` currently provides:

- linear and cosine beta schedules;
- canonical DDPM schedule coefficients;
- closed-form forward diffusion;
- clean-sample reconstruction;
- posterior mean and variance calculation;
- stochastic and explicit-noise reverse steps;
- framework-independent training samples and epsilon loss;
- predictor-driven DDPM sampling with seeded or explicit randomness;
- deterministic sinusoidal time embeddings.

## Time embeddings

`sinusoidalTimeEmbedding` maps a non-negative scalar time to cosine and sine
features without requiring a Wolfram neural-network object. The default
maximum period is `10000.`; odd dimensions receive one trailing zero.

```wl
timeFeatures = sinusoidalTimeEmbedding[500, 128];
Length[timeFeatures]
(* 128 *)

customFeatures = sinusoidalTimeEmbedding[
  500,
  128,
  "MaxPeriod" -> 1000.
];
```

## Local installation and loading

Load a development checkout without copying it:

```wl
PacletDirectoryLoad["/absolute/path/to/Stochasma"];
Needs["Stochasma`"];
```

Or install the checkout into the local paclet repository:

```wl
PacletInstall["/absolute/path/to/Stochasma"];
Needs["Stochasma`"];
```

No subcontext needs to be loaded manually.

## Minimal example

Logical time uses `t = 0` for the clean sample and `t = 1..T` for diffused
states. Schedule arrays contain the entries for `1..T`.

```wl
Needs["Stochasma`"]

betas = linearBetaSchedule[1000, 10^-4, 0.02];
schedule = makeDiffusionSchedule[betas];

x0 = {1., 0., -1.};
xt = forwardDiffuse[x0, 500, schedule];
```

Pass noise explicitly for a deterministic calculation:

```wl
noise = {0.25, -0.5, 1.};

xt = forwardDiffuse[x0, 500, schedule, noise];
x0Recovered = predictCleanSample[xt, 500, noise, schedule];

Max[Abs[x0Recovered - x0]] < 10^-10
(* True *)
```

## Randomness

- Automatic randomness consumes the current Wolfram random stream.
- An explicit integer `"Seed"` is reproducible and locally isolated.
- Explicit noise is fully deterministic and does not consult the random
  stream.

## Predictor protocol and sampling

A predictor is any callable with the contract `predictor[xt, t]`. It must
return an epsilon prediction with the same shape as `xt`; the core does not
require `NetGraph`, `NetTrain`, or any particular model representation.

```wl
zeroPredictor = Function[{xt, t}, 0. xt];
initialNoise = {0.2, -0.4, 0.8};

sample = ddpmSample[
  zeroPredictor,
  initialNoise,
  schedule,
  "Seed" -> 1234
];
```

With `"ReturnTrajectory" -> True`, `ddpmSample` returns
`<|"Sample" -> x0, "Trajectory" -> {xT, ..., x0}|>`. The `"Noises"` option
accepts a length-`T` list indexed by logical time for fully explicit reverse
randomness; its `t = 1` entry is validated but not added.

## Tests and example

```bash
wolframscript -file tests/run_all_tests.wls
wolframscript -file examples/gaussian_1d.wls
```

The test suite requires a locally installed and activated Wolfram Engine with
`wolframscript` on `PATH`. No GitHub-hosted workflow is included because that
runtime is not reliably available there. A self-hosted runner can execute the
same test command unchanged.

The scientific example constructs a synthetic bimodal one-dimensional dataset,
diffuses it at several times with one explicit noise realization, prints
distribution summaries, and exports comparative histograms to the system
temporary directory.
