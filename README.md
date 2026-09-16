# Stochasma

Stochasma is a general-purpose Wolfram Language paclet for diffusion-model
primitives. Version 0.1 implements Gaussian diffusion / DDPM while keeping
neural-network models and application adapters external. Version 0.2 adds
model-facing utilities without coupling the core to a network architecture.

The core operates on finite real numeric scalars and arrays of arbitrary rank.
It makes no image, music, or other application-domain assumptions and has no
Python dependency.

## Status

The unreleased development version `0.3.0` currently provides:

- linear and cosine beta schedules;
- canonical DDPM schedule coefficients;
- closed-form forward diffusion;
- clean-sample reconstruction;
- posterior mean and variance calculation;
- stochastic and explicit-noise reverse steps;
- framework-independent training samples and epsilon loss;
- predictor-driven DDPM sampling with seeded or explicit randomness;
- deterministic sinusoidal time embeddings;
- Wolfram `NetChain` and `NetGraph` predictor adapters;
- batched epsilon-prediction training data with controlled randomness;
- deterministic or controlled-stochastic DDIM sampling over full or
  subsampled reverse timesteps;
- a uniform categorical transition kernel;
- categorical transition sampling through arbitrary row-stochastic matrices;
- categorical transition schedules and cumulative kernels;
- time-indexed categorical forward diffusion with automatic or explicit
  uniform noise;
- exact categorical posterior probabilities;
- predicted-`x0` reverse probabilities and single-step categorical reverse
  sampling;
- predictor-driven categorical reverse sampling with trajectories and
  controlled randomness.

## Categorical diffusion

Categorical distributions use row vectors. `Q_t` is the one-step transition
from logical time `t - 1` to `t`, and the cumulative transition is
`Q̄_t = Q_1 . Q_2 . ... . Q_t`. Therefore,
`q(x_t | x_0) = Categorical[oneHot(x_0) . Q̄_t]`.

`categoricalForwardDiffuse` samples through an arbitrary transition matrix;
`categoricalForwardDiffuseAt` resolves logical time through the cumulative
kernel `Q̄_t` stored in a categorical schedule.

Matrix-based sampling accepts any validated `K`-by-`K` row-stochastic matrix,
including either `Q_t` or `Q̄_t`:

```wl
x0 = {1, 2, 3};

kernel = uniformCategoricalTransitionKernel[
  3,
  0.2
];

x1 = categoricalForwardDiffuse[
  x0,
  kernel
];
```

Time-indexed sampling uses the schedule's cumulative transition at `t`. As in
the Gaussian core, `t = 0` returns `x0` exactly and does not consume the random
stream. The explicit-noise form validates its noise at `t = 0` before returning
`x0`.

```wl
schedule = makeUniformCategoricalSchedule[
  3,
  {0.1, 0.2, 0.3}
];

xt = categoricalForwardDiffuseAt[
  x0,
  3,
  schedule
];

posterior = categoricalPosterior[
  1,
  2,
  2,
  schedule
];

predictedX0 = {0.7, 0.2, 0.1};

reverseProbabilities = categoricalReverseProbabilities[
  2,
  2,
  predictedX0,
  schedule
];

previousState = categoricalReverseStep[
  2,
  2,
  predictedX0,
  schedule
];

categoricalPredictor = Function[{state, time},
  {0.7, 0.2, 0.1}
];

x0Sample = categoricalSample[
  categoricalPredictor,
  2,
  schedule,
  "Seed" -> 1234
];
```

`predictedX0` is a model-predicted distribution over clean categories.
Stochasma combines it with `Q̄_(t-1)` and `Q_t`, then normalizes the resulting
joint-marginalized reverse weights once at the end. It is not a mixture of
individually normalized exact posteriors.

`categoricalSample` calls `predictor[xt, t]` from `T` down to `1`. It accepts
automatic randomness, a locally isolated integer seed, or a length-`T`
`"UniformNoises"` list indexed by logical time. With
`"ReturnTrajectory" -> True`, it returns `{xT, ..., x0}` alongside the final
sample.

## Time embeddings

`sinusoidalTimeEmbedding` maps a non-negative scalar time to cosine and sine
features without requiring a Wolfram neural-network object. It requires at
least two dimensions. The default maximum period is `10000.`; odd dimensions
receive one trailing zero.

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

## Wolfram neural-network adapter

`makeWolframNetPredictor` returns a function that adapts `predictor[xt, t]`
calls to a Wolfram neural network. With the automatic input adapter, a
`NetGraph` receives `<|"Sample" -> xt, "Time" -> N[t]|>`:

```wl
predictor = makeWolframNetPredictor[epsilonNetwork];
predictedNoise = predictor[xt, 500];
```

Pass an explicit input adapter for different port names, any locally chosen
time representation, or a single-input `NetChain`. Using
`sinusoidalTimeEmbedding` is an optional composition rather than a requirement
of the network bridge:

```wl
predictor = makeWolframNetPredictor[
  epsilonNetwork,
  Function[{sample, time},
    <|
      "State" -> sample,
      "TimeEmbedding" -> sinusoidalTimeEmbedding[time, 128]
    |>
  ]
];
```

The network is responsible for returning an epsilon prediction compatible
with the sampler protocol. The DDPM and DDIM samplers, rather than the adapter,
validate the prediction shape and finiteness.

## Training batches

`makeDiffusionTrainingBatch` creates one neutral training association per clean
sample. Automatic times are drawn uniformly from `1..T`, and automatic noises
match each sample's shape. Use an integer seed for reproducible, locally
isolated randomness:

```wl
batch = makeDiffusionTrainingBatch[
  cleanSamples,
  schedule,
  "Seed" -> 1234
];
```

Pass matching `"Times"` and `"Noises"` lists for a fully deterministic batch.
Each result has the same `"Clean"`, `"Noisy"`, `"Time"`, and `"Noise"` fields
as `makeDiffusionTrainingSample`, so callers remain free to adapt them to a
specific training framework or network port layout.

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

## DDIM sampling

`ddimSample` uses the same `predictor[xt, t]` contract. `"Eta" -> 0.` is
deterministic, while a positive eta enables controlled stochastic sampling.
`"Timesteps"` selects a full or subsampled reverse trajectory:

```wl
sample = ddimSample[
  predictor,
  initialNoise,
  schedule,
  "Eta" -> 0.,
  "Timesteps" -> {1000, 800, 600, 400, 200, 1}
];
```

Explicit timesteps must be non-empty, valid, and strictly decreasing; the
sampler always adds the final transition to logical time `0`. `initialNoise`
is interpreted as the state at the first resolved timestep. With automatic
timesteps this is `xT`; with an explicit sequence beginning below `T`, it is
the state at that first requested logical time. Explicit `"Noises"` must have
the same length and order as the resolved timestep list. Its final entry is
validated but ignored because the transition to `x0` adds no noise. With
`"ReturnTrajectory" -> True`, trajectory states and returned timesteps align
as `{xStart, ..., x0}` and `{tStart, ..., 0}`.

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
