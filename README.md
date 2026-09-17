# Stochasma

Stochasma is a general-purpose Wolfram Language paclet for diffusion-model
primitives. Version 0.1 implements Gaussian diffusion / DDPM while keeping
neural-network models and application adapters external. Version 0.2 adds
model-facing utilities and DDIM without coupling the core to a network
architecture. Version 0.3 adds scalar categorical diffusion and reverse
sampling. The unreleased Version 0.4 development line adds canonical D3PM
predicted-`x0` reverse sampling, validation-once sampler hot paths, and
sampler-selectable DDPM/DDIM latent composition while keeping production code
separate from the test suites.
Version 0.5 development adds generic conditioning without prescribing a data
representation or model architecture.

The core operates on finite real numeric scalars and arrays of arbitrary rank.
It makes no application-domain or data-representation assumptions and has no
Python dependency.

## Status

The latest stable release is `0.3.0`.
Development versions `0.4.0` and `0.5.0` are unreleased.

Current development capabilities:

- Gaussian diffusion;
- DDPM;
- DDIM;
- categorical diffusion and canonical D3PM predicted-`x0` reverse sampling;
- training utilities;
- Wolfram neural-network adapters;
- latent diffusion composition with selectable DDPM/DDIM sampling.

## Conditioning protocol

The v0.5 conditioning layer starts from the callable contract
`conditionedPredictor[xt, t, conditioning]`. The conditioning value is any
caller-owned Wolfram expression and is treated as opaque: Stochasma assigns no
required type, keys, shape, encoding, modality, or unconditional sentinel.

The expected prediction remains a property of the consumer. Gaussian
consumers expect finite real epsilon predictions matching `xt`; categorical
consumers expect valid clean-state probability vectors. Public samplers still
accept the two-argument `predictor[xt, t]` protocol.

Use `makeConditionedPredictor` to bind one conditioning value without teaching
the sampler about its representation:

```wl
conditionedPredictor = Function[{xt, t, conditioning},
  conditioning["Scale"] xt
];

predictor = makeConditionedPredictor[
  conditionedPredictor,
  <|"Scale" -> 0.25|>
];

sample = ddpmSample[predictor, initialNoise, schedule];
```

The wrapper passes the value and prediction through unchanged. The selected
sampler remains responsible for validating the prediction it consumes.

`classifierFreeGuidance` provides the representation-neutral affine
combination itself:

```wl
guidedPrediction = classifierFreeGuidance[
  unconditionedPrediction,
  conditionedPrediction,
  guidanceScale
];
```

It accepts same-shape finite real scalars or arrays and a finite non-negative
real scale. The operation preserves shape but does not normalize or clip its
result; a downstream consumer still applies its own output constraints.

Build a sampler-ready guided predictor by composing two independently bound
conditions:

```wl
unconditionedPredictor = makeConditionedPredictor[
  conditionedPredictor,
  unconditionedValue
];

conditionalPredictor = makeConditionedPredictor[
  conditionedPredictor,
  conditioning
];

guidedPredictor = makeClassifierFreeGuidedPredictor[
  unconditionedPredictor,
  conditionalPredictor,
  7.5
];

sample = ddpmSample[guidedPredictor, initialNoise, schedule];
```

For every `guidedPredictor[xt, t]` call, the wrapper evaluates the
unconditioned branch first and the conditioned branch second, once each. It
does so even at scales `0` and `1`, preserving a stable evaluation and random
stream contract. A failure in the first branch short-circuits the second.

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

predictor = Function[{state, time},
  {0.6, 0.3, 0.1}
];

x0 = categoricalSample[
  predictor,
  2,
  schedule,
  "Seed" -> 1234
];
```

`predictedX0` is a model-predicted distribution over clean categories. For
`t > 1`, Stochasma uses the D3PM predicted-`x0` parameterization

```text
p_theta(x_(t-1) | x_t)
  proportional to Sum_i p_theta(x_0 = i | x_t, t)
    q(x_(t-1), x_t | x_0 = i).
```

Equivalently, it computes `predictedX0 . Qbar_(t-1)`, multiplies that row
vector elementwise by `Q_t[:, xt]`, and normalizes once. At `t = 1`, it
returns `predictedX0` directly. If the combined unnormalized weights at
`t > 1` have zero or non-finite normalization, the reverse operation returns
`$Failed` without a fallback.

`categoricalSample` interprets its explicit `initialState` as `x_T` and never
generates a terminal prior internally: arbitrary transition kernels need not
make `Q̄_T` uniform. It is scalar-only in 0.3: `initialState` must be an
integer category label and `predictor[xt, t]` must return a length-`K`
probability list for `p_theta(x0 | xt, t)`.

The sampler calls `predictor[xt, t]` from `T` down to `1`. With
`"Noises" -> Automatic`, it consumes one caller-stream uniform variate per
reverse step. An integer `"Seed"` is reproducible and locally isolated. An
explicit length-`T` `"Noises"` list is deterministic, is indexed by logical
time (`Noises[[t]]` drives `t -> t - 1`), does not consume the random stream,
and takes precedence over `"Seed"`. With `"ReturnTrajectory" -> True`, the
result is `<|"Sample" -> x0, "Trajectory" -> {xT, ..., x0}, "Timesteps" ->
{T, ..., 0}|>`.

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

`makeConditionedDiffusionTrainingBatch` delegates the same diffusion work and
appends one aligned opaque `"Conditioning"` value to every association:

```wl
conditionedBatch = makeConditionedDiffusionTrainingBatch[
  cleanSamples,
  conditioningValues,
  schedule,
  "Seed" -> 1234
];
```

`conditioningValues` must be a list matching `cleanSamples`; each element may
have any representation, including `Automatic`. The wrapper performs no
broadcasting or dropout and consumes no additional randomness. Callers remain
responsible for supplying any unconditional sentinel or precomputed dropout
policy required by their model.

## Latent diffusion

`makeLatentDiffusionTrainingSample` evaluates a caller-supplied encoder once
and applies the existing Gaussian training objective to its output. The source
input may be structured or application-specific; the encoder must return a
finite real numeric scalar or non-empty array. The returned `"Clean"` field is
the encoded latent.

```wl
encoder = Function[input, input["Latent"]];

trainingSample = makeLatentDiffusionTrainingSample[
  encoder,
  <|"Latent" -> {0.2, -0.4, 0.8}|>,
  5,
  schedule
];
```

Pass a fifth argument for deterministic explicit noise matching the encoded
latent. Automatic noise consumes the caller's current random stream exactly as
`makeDiffusionTrainingSample` does.

`latentDiffusionSample` runs a selected sampler entirely in latent space, then
evaluates a caller-supplied decoder exactly once on the final latent `z0`.
The default sampler is `ddpmSample`:

```wl
decodedSample = latentDiffusionSample[
  decoder,
  predictor,
  initialLatentNoise,
  schedule,
  "SamplerOptions" -> {
    "Seed" -> 1234
  }
];
```

Choose DDIM explicitly without duplicating latent-layer logic:

```wl
decodedSample = latentDiffusionSample[
  decoder,
  predictor,
  initialLatent,
  schedule,
  "Sampler" -> ddimSample,
  "SamplerOptions" -> {
    "Eta" -> 0.,
    "Timesteps" -> {1000, 800, 600, 400, 200, 1},
    "ReturnTrajectory" -> True
  }
];
```

The wrapper accepts only `"Sampler"` and `"SamplerOptions"`; unknown wrapper
options and options rejected by the selected sampler produce a message and
`$Failed`. Trajectory mode returns `"Sample"` for the decoded value,
`"LatentSample"` for `z0`, and `"LatentTrajectory"` for the latent-only
trajectory. If the selected sampler returns `"Timesteps"`, the wrapper
preserves them unchanged. Intermediate latents are never decoded, and a
sampler failure prevents decoder evaluation.

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

The production files in `core/` and `models/` contain no embedded suites.
Module tests live in `tests/*.wlt`; the headless runner uses Wolfram
`VerificationTest` and `TestReport`, includes a clean paclet-loading/API check,
and returns a nonzero exit code on failure.

The test suite requires a locally installed and activated Wolfram Engine with
`wolframscript` on `PATH`. No GitHub-hosted workflow is included because that
runtime is not reliably available there. A self-hosted runner can execute the
same test command unchanged.

The scientific example constructs a synthetic bimodal one-dimensional dataset,
diffuses it at several times with one explicit noise realization, prints
distribution summaries, and exports comparative histograms to the system
temporary directory.
