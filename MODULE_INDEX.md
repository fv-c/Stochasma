# Stochasma — Module Index

Public `::usage` messages are the primary API documentation. All public symbols
are loaded by `Needs["Stochasma`"]`.

| Module | Public symbol | Signature | Description |
|---|---|---|---|
| `core/schedules.wl` | `linearBetaSchedule` | `linearBetaSchedule[steps_Integer, betaStart_, betaEnd_]` | Construct a finite linear beta schedule. |
| `core/schedules.wl` | `cosineBetaSchedule` | `cosineBetaSchedule[steps_Integer, opts___]` | Construct a cosine beta schedule with configurable offset and generated-beta cap. |
| `core/schedules.wl` | `makeDiffusionSchedule` | `makeDiffusionSchedule[betas_List]` | Validate betas and derive all canonical DDPM coefficient arrays. |
| `core/forward.wl` | `forwardDiffuse` | `forwardDiffuse[x0_, t_Integer, schedule_Association]` | Sample the closed-form forward process from the current random stream. |
| `core/forward.wl` | `forwardDiffuse` | `forwardDiffuse[x0_, t_Integer, schedule_Association, noise_]` | Evaluate the forward process with explicit noise. |
| `core/reverse.wl` | `predictCleanSample` | `predictCleanSample[xt_, t_Integer, predictedNoise_, schedule_Association]` | Reconstruct an estimate of `x0`. |
| `core/reverse.wl` | `reverseMeanVariance` | `reverseMeanVariance[xt_, t_Integer, predictedNoise_, schedule_Association]` | Return predicted `x0` and posterior mean and variance. |
| `core/reverse.wl` | `reverseDiffuseStep` | `reverseDiffuseStep[xt_, t_Integer, predictedNoise_, schedule_Association]` | Sample one reverse step from the current random stream. |
| `core/reverse.wl` | `reverseDiffuseStep` | `reverseDiffuseStep[xt_, t_Integer, predictedNoise_, schedule_Association, noise_]` | Evaluate one reverse step with explicit noise. |
| `core/objectives.wl` | `makeDiffusionTrainingSample` | `makeDiffusionTrainingSample[x0_, t_Integer, schedule_Association]` | Create an epsilon-prediction training example from the current random stream. |
| `core/objectives.wl` | `makeDiffusionTrainingSample` | `makeDiffusionTrainingSample[x0_, t_Integer, schedule_Association, noise_]` | Create a deterministic explicit-noise training example. |
| `core/objectives.wl` | `epsilonPredictionLoss` | `epsilonPredictionLoss[predicted_, target_]` | Compute elementwise mean squared epsilon error. |
| `core/sampling.wl` | `ddpmSample` | `ddpmSample[predictor_, initialNoise_, schedule_Association, opts___]` | Run reverse sampling with automatic, seeded, or explicit step noise. |
| `core/ddim.wl` | `ddimSample` | `ddimSample[predictor_, initialNoise_, schedule_Association, opts___]` | Run deterministic or controlled-stochastic DDIM sampling over full or subsampled reverse timesteps. |
| `models/embeddings.wl` | `sinusoidalTimeEmbedding` | `sinusoidalTimeEmbedding[time_, dimensions_Integer, opts___]` | Construct at least two deterministic sinusoidal features for a non-negative diffusion time. |
| `models/adapters.wl` | `makeWolframNetPredictor` | `makeWolframNetPredictor[network_, inputAdapter_: Automatic]` | Return a callable `predictor[xt, t]` bridge; the network produces epsilon predictions and `ddpmSample` validates their shape and finiteness. |
| `models/training.wl` | `makeDiffusionTrainingBatch` | `makeDiffusionTrainingBatch[cleanSamples_List, schedule_Association, opts___]` | Create a batch of epsilon-prediction training associations with automatic, seeded, or explicit times and noises. |
