# Stochasma

Stochasma is a general-purpose Wolfram Language paclet for diffusion-model
primitives. Version 0.1 focuses on mathematically verifiable Gaussian diffusion
and DDPM while keeping neural-network models and application adapters external.

## Status

Version 0.1 is under development.

## Local loading

```wl
PacletDirectoryLoad["/path/to/Stochasma"];
Needs["Stochasma`"];
```

The paclet has no Python dependency and no application-domain dependency.
