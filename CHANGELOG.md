# Changelog

## 0.2.0 — Unreleased

- Add deterministic sinusoidal time embeddings for external predictors.

## 0.1.0 — Unreleased

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
