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

- categorical diffusion;
- discrete-state transition kernels.

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
