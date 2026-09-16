# Stochasma — Architecture

## Purpose

Stochasma provides general-purpose diffusion-model primitives for the Wolfram
Language. Version 0.1 implements Gaussian diffusion / DDPM without assumptions
about images, music, or a particular neural-network architecture. Version 0.2
adds model-facing utilities and DDIM sampling while preserving that separation.
Version 0.3 begins a discrete diffusion layer with uniform categorical
transition kernels, explicit transition schedules, and shape-preserving
time-indexed forward sampling.

## Layers

```text
Schedules
    ↓
Forward Process
    ↓
Training Objective
    ↓
Predictor
    ↓
Reverse Process
    ↓
Sampler
```

Dependencies point downward only where mathematically required. The predictor
is supplied externally through the callable protocol `predictor[xt, t]` and is
not part of the deterministic DDPM core.

The DDPM sampler traverses `T, T-1, ..., 1` and produces `x0`. When requested,
its trajectory is ordered `{xT, x(T-1), ..., x0}`. Explicit DDPM reverse noises
use a length-`T` list indexed by logical time; the `t = 1` entry is not added
because the posterior variance is zero.

The DDIM sampler accepts either that full reverse sequence or a non-empty,
strictly decreasing subsequence of logical times. It calls the same
`predictor[xt, t]` protocol exactly at those times and then transitions to
mathematical time `0`. A returned DDIM trajectory aligns
`{xStart, ..., x0}` with `{tStart, ..., 0}`. Explicit DDIM noises follow the
requested timestep order and have the same length; the final entry is validated
but ignored because the transition to `0` has zero stochastic coefficient.

Model-facing utilities are independent of the deterministic DDPM core and are
independently composable with one another. Time embeddings may be used by an
input adapter, but the Wolfram neural-network bridge does not depend on a
specific embedding representation. Neural-network adapters translate
`predictor[xt, t]` calls into network inputs, but the core does not prescribe a
network architecture or input layout.

## Time convention

- `t = 0` always denotes the clean sample `x0`.
- `t = 1..T` denotes diffused states.
- Schedule arrays store entries for logical times `1..T`.
- Access to a schedule array at mathematical time `t` therefore uses Wolfram
  list index `t`; `t = 0` is handled explicitly and never indexes an array.

## Categorical forward process

Categorical distributions use a row-vector representation. `Q_t` is the
one-step categorical transition matrix from logical time `t - 1` to `t`.
The cumulative transition from the clean state is

```text
Q̄_t = Q_1 . Q_2 . ... . Q_t
```

so the forward marginal is

```text
q(x_t | x_0) = Categorical[oneHot(x_0) . Q̄_t]
```

`categoricalForwardDiffuse` is the matrix-based primitive: it samples through
any compatible row-stochastic matrix, whether that matrix is `Q_t`, `Q̄_t`,
or another categorical transition. It does not resolve logical time.

`categoricalForwardDiffuseAt` is the time-indexed process for
`q(x_t | x_0)`: it resolves `t` through a categorical schedule and applies the
stored cumulative matrix `Q̄_t`. As in the Gaussian core, `t = 0` is the
clean state, while schedule arrays are indexed at logical times `1..T`.

## Categorical reverse process

For scalar states `x_0 = i` and `x_t = k`, the exact posterior uses the
previous cumulative transition and the current one-step transition:

```text
q(x_(t-1) = j | x_t = k, x_0 = i)
  ∝ Q̄_(t-1)[i, j] Q_t[j, k]
```

At `t = 1`, `Q̄_0` is the identity matrix and does not index the schedule.
The model predicts a probability distribution on `x_0`, rather than the
reverse-step posterior directly. The canonical predicted-`x0` reverse
parameterization marginalizes the joint probabilities and normalizes once:

```text
p_theta(x_(t-1) | x_t)
  ∝ Σ_i p_theta(x_0 = i | x_t, t) q(x_(t-1), x_t | x_0 = i)

r = [p_theta(x_0 | x_t, t) . Q̄_(t-1)] ⊙ Q_t[:, x_t]

p_theta(x_(t-1) | x_t) = r / sum(r)
```

Here `⊙` denotes elementwise multiplication. At `t = 1`, `Q̄_0` is the
identity, so the predicted clean-state probabilities are still weighted by
the current transition likelihood before normalization. `categoricalReverseStep`
samples once from this joint-marginalized probability vector.

The categorical sampler traverses `T, T-1, ..., 1`. At each logical time it
calls `predictor[xt, t]` for a probability distribution over clean categories,
then samples the next state through `categoricalReverseStep`. A returned
trajectory is ordered `{xT, x(T-1), ..., x0}`. Explicit uniform variates use a
length-`T` list indexed by logical time, and every entry is consumed because
the categorical transition at `t = 1` remains stochastic in general.

## Invariants

- The Gaussian core accepts real-valued numeric scalars and arrays.
- Input and output shape is preserved.
- Functions are pure and inputs are never modified in place.
- Unseeded stochastic operations consume the caller's current random stream.
- Explicit integer seeds are reproducible and locally isolated with
  `BlockRandom`.
- Deterministic mathematical functions never generate random values.
- Every stochastic primitive has a deterministic explicit-noise form where
  applicable.
- No package-owned mutable global state is used.
- The core has no dependency on application domains.
- The core has no dependency on `NetGraph`, `NetTrain`, or a model architecture.
- Invalid parameters produce a message and `$Failed`; they are not silently
  clipped into a valid range.
- Categorical states are represented by integer labels `1..K`, and categorical
  transition kernels are finite, non-negative, square, and row-stochastic.
- Categorical transition products follow the row-vector order
  `Q̄_t = Q_1 . Q_2 . ... . Q_t`.
- Categorical forward sampling preserves scalar or array shape and accepts
  matching explicit uniform variates in `[0, 1)`.
- Categorical reverse sampling accepts scalar states, validates every
  predicted clean-state probability vector, and supports current-stream,
  locally seeded, or explicit uniform randomness.

## Module map

```text
core/schedules.wl   noise schedules and derived DDPM coefficients
core/forward.wl     forward process q(x_t | x_0)
core/reverse.wl     clean reconstruction, posterior, and reverse step
core/objectives.wl  framework-independent training primitives
core/sampling.wl    predictor-driven DDPM sampling loop
core/ddim.wl        predictor-driven full-step or subsampled DDIM sampling
core/categorical.wl categorical kernels, schedules, and forward and reverse sampling
models/embeddings.wl deterministic sinusoidal logical-time features
models/adapters.wl  Wolfram neural-network predictor bridge
models/training.wl  batched epsilon-prediction training data
```
