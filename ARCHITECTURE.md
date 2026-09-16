# Stochasma — Architecture

## Purpose

Stochasma provides general-purpose diffusion-model primitives for the Wolfram
Language. Version 0.1 implements Gaussian diffusion / DDPM without assumptions
about images, music, or a particular neural-network architecture.

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
- Randomness is isolated with `BlockRandom`.
- Deterministic mathematical functions never generate random values.
- Every stochastic operation has a deterministic explicit-noise form.
- No global mutable state is used.
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
```
