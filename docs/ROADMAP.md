# Stochasma Roadmap

The roadmap records intended directions only. None of the items below belongs
to the v0.1 implementation.

## 0.2

- time embeddings;
- Wolfram neural-network adapters;
- training helpers;
- DDIM sampling.

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
