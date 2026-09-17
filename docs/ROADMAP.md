# Stochasma Roadmap

The roadmap records v0.2 and later directions. None of the items below belongs
to the v0.1 implementation. Checked items are implemented.

## 0.2

- [x] sinusoidal time embeddings;
- [x] Wolfram neural-network adapters;
- [x] training helpers;
- [x] DDIM sampling.

## 0.3

- [x] uniform categorical transition kernel;
- [x] categorical transition sampling;
- [x] discrete-state transition schedules;
- [x] time-indexed categorical forward diffusion;
- [x] exact categorical posterior;
- [x] predicted-`x0` reverse probabilities;
- [x] categorical single reverse step;
- [x] categorical reverse sampler.

## 0.4

- latent diffusion:
  - [x] encoder-backed latent-space training samples;
  - [x] decoded latent-space sampling;
  - [x] explicit DDPM/DDIM sampler selection and isolated sampler options;
- [x] canonical D3PM predicted-`x0` categorical reverse parameterization;
- [x] validation-once sampler hot paths;
- [x] test-suite extraction from production modules.

## 0.5

- [ ] generic conditioning protocol;
- [ ] conditioned predictor composition;
- [ ] classifier-free guidance primitives;
- [ ] conditioning-aware training utilities;
- [ ] conditioning regression tests.

## Future adapters

- structured symbolic data;
- domain-specific representations;
- external application adapters.

Application adapters remain outside the Stochasma core and must not introduce
domain-specific dependencies into the paclet.
