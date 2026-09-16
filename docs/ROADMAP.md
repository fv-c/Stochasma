# Stochasma Roadmap

The roadmap records v0.2 and later directions. None of the items below belongs
to the v0.1 implementation. Checked items are present on the development
branch.

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
- [ ] categorical reverse sampler.

## 0.4

- latent diffusion;
- conditioning interfaces.

## Future adapters

- structured symbolic data;
- Temporal System `stemData`;
- score-space diffusion.

Application adapters remain outside the Stochasma core. A future Temporal
System integration will load both paclets independently and translate external
`stemData` into a representation accepted by Stochasma.
