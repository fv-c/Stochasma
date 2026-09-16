# Changelog

## 0.4.0 — Unreleased

## 0.3.0 — 2026-09-16

- Add a uniform categorical transition kernel.
- Add categorical transition sampling through arbitrary row-stochastic
  matrices.
- Add validated categorical transition schedules with cumulative transition
  matrices.
- Add uniform categorical schedule construction from beta sequences.
- Add time-indexed categorical forward diffusion through cumulative `Qbar_t`
  kernels.
- Add exact categorical posterior probabilities.
- Add categorical reverse-step sampling and canonical joint-marginalized
  predicted-`x0` reverse probabilities.
- Add a full scalar categorical reverse sampler with seeded, automatic, and
  explicit-noise randomness.

## 0.2.0 — 2026-09-16

- Add deterministic sinusoidal time embeddings for external predictors.
- Add a `NetChain` and `NetGraph` adapter for the predictor protocol.
- Add batched epsilon-prediction training data with controlled randomness.
- Add deterministic and controlled-stochastic DDIM sampling with full or
  subsampled reverse timesteps.
- Verify DDIM and DDPM ancestral equivalence for consecutive timesteps at
  `Eta -> 1` with corresponding explicit reverse noises.

## 0.1.0 — 2026-09-16

- Add a modern Wolfram paclet entry point under the public `Stochasma`` context.
- Add validated linear and cosine beta schedules.
- Add canonical Gaussian DDPM schedule coefficients.
- Reject beta schedules whose numerical evaluation produces non-finite or
  internally inconsistent derived coefficients.
- Add closed-form forward diffusion for numeric scalars and arbitrary-rank
  arrays.
- Add clean-sample reconstruction and deterministic posterior parameters.
- Add stochastic and explicit-noise reverse diffusion steps.
- Add framework-independent epsilon-prediction training samples and MSE loss.
- Add predictor-driven DDPM sampling, trajectories, seeds, and explicit step
  noises.
- Define automatic randomness as consuming the caller stream, integer seeds as
  locally isolated, and explicit noise as fully deterministic.
- Add a headless mathematical test suite and a synthetic one-dimensional
  Gaussian diffusion example.
- Document future neural, discrete, latent, conditioning, and external adapter
  layers without implementing them in the v0.1 core.
