# Stochasma — Architecture

## Purpose

Stochasma provides general-purpose diffusion-model primitives for the Wolfram
Language. Version 0.1 implements Gaussian diffusion / DDPM without assumptions
about images, music, or a particular neural-network architecture. Version 0.2
adds model-facing utilities while preserving that separation.

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

The sampler traverses `T, T-1, ..., 1` and produces `x0`. When requested, its
trajectory is ordered `{xT, x(T-1), ..., x0}`. Explicit reverse noises use a
length-`T` list indexed by logical time; the `t = 1` entry is not added because
the posterior variance is zero.

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

## Module map

```text
core/schedules.wl   noise schedules and derived DDPM coefficients
core/forward.wl     forward process q(x_t | x_0)
core/reverse.wl     clean reconstruction, posterior, and reverse step
core/objectives.wl  framework-independent training primitives
core/sampling.wl    predictor-driven DDPM sampling loop
models/embeddings.wl deterministic sinusoidal logical-time features
models/adapters.wl  Wolfram neural-network predictor bridge
```
