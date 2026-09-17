# Stochasma — Module Index

Public `::usage` messages are the primary API documentation. All public symbols
are loaded by `Needs["Stochasma`"]`.

| Module | Public symbol | Signature | Description |
|---|---|---|---|
| `core/schedules.wl` | `linearBetaSchedule` | `linearBetaSchedule[steps_Integer, betaStart_, betaEnd_]` | Construct a finite linear beta schedule. |
| `core/schedules.wl` | `cosineBetaSchedule` | `cosineBetaSchedule[steps_, opts___]` | Construct a cosine beta schedule with configurable offset and generated-beta cap. |
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
| `core/categorical.wl` | `uniformCategoricalTransitionKernel` | `uniformCategoricalTransitionKernel[categoryCount_, beta_]` | Construct a row-stochastic uniform-corruption kernel for categorical states. |
| `core/categorical.wl` | `makeCategoricalSchedule` | `makeCategoricalSchedule[transitionKernels_List]` | Validate one-step matrices `{Q_1, ..., Q_T}` and derive row-vector cumulative matrices `Q̄_t = Q_1 . ... . Q_t`. |
| `core/categorical.wl` | `makeUniformCategoricalSchedule` | `makeUniformCategoricalSchedule[categoryCount_, betas_List]` | Construct and validate a categorical schedule from uniform-corruption kernels. |
| `core/categorical.wl` | `categoricalForwardDiffuse` | `categoricalForwardDiffuse[state_, transitionKernel_List]` | Sample categorical states through an arbitrary compatible row-stochastic matrix using the current random stream. |
| `core/categorical.wl` | `categoricalForwardDiffuse` | `categoricalForwardDiffuse[state_, transitionKernel_List, uniformNoise_]` | Sample through an arbitrary compatible row-stochastic matrix with explicit uniform variates. |
| `core/categorical.wl` | `categoricalForwardDiffuseAt` | `categoricalForwardDiffuseAt[x0_, t_Integer, schedule_Association]` | Sample `q(x_t | x_0)` through the schedule's cumulative matrix `Q̄_t`. |
| `core/categorical.wl` | `categoricalForwardDiffuseAt` | `categoricalForwardDiffuseAt[x0_, t_Integer, schedule_Association, uniformNoise_]` | Evaluate time-indexed categorical forward diffusion with explicit uniform variates. |
| `core/categorical.wl` | `categoricalPosterior` | `categoricalPosterior[x0_, xt_, t_Integer, schedule_Association]` | Return the exact scalar-state probability vector `q(x_(t-1) | x_t, x_0)`. |
| `core/categorical.wl` | `categoricalReverseProbabilities` | `categoricalReverseProbabilities[xt_, t_Integer, predictedX0Probabilities_List, schedule_Association]` | Construct the D3PM predicted-`x0` reverse distribution by joint marginalization and one final normalization; return the predicted clean-state distribution directly at `t = 1`. |
| `core/categorical.wl` | `categoricalReverseStep` | `categoricalReverseStep[xt_, t_Integer, predictedX0Probabilities_List, schedule_Association]` | Sample one categorical reverse step from the current random stream. |
| `core/categorical.wl` | `categoricalReverseStep` | `categoricalReverseStep[xt_, t_Integer, predictedX0Probabilities_List, schedule_Association, uniformNoise_]` | Sample one categorical reverse step with an explicit uniform variate. |
| `core/categorical.wl` | `categoricalSample` | `categoricalSample[predictor_, initialState_, schedule_Association, opts___]` | Run the full scalar categorical reverse process; `initialState` must be an Integer scalar categorical state and is interpreted as caller-supplied `x_T`. |
| `models/embeddings.wl` | `sinusoidalTimeEmbedding` | `sinusoidalTimeEmbedding[time_, dimensions_, opts___]` | Construct at least two deterministic sinusoidal features for a non-negative diffusion time. |
| `models/conditioning.wl` | `makeConditionedPredictor` | `makeConditionedPredictor[conditionedPredictor_, conditioning_]` | Bind an opaque conditioning value to the three-argument conditioned-predictor protocol and return a standard two-argument predictor. |
| `models/conditioning.wl` | `classifierFreeGuidance` | `classifierFreeGuidance[unconditionedPrediction_, conditionedPrediction_, guidanceScale_]` | Combine same-shape finite real predictions with the standard classifier-free guidance affine formula and a non-negative scale. |
| `models/conditioning.wl` | `makeClassifierFreeGuidedPredictor` | `makeClassifierFreeGuidedPredictor[unconditionedPredictor_, conditionedPredictor_, guidanceScale_]` | Compose two standard predictors into an unconditioned-first classifier-free guided predictor. |
| `models/adapters.wl` | `makeWolframNetPredictor` | `makeWolframNetPredictor[network_, inputAdapter_: Automatic]` | Return a callable `predictor[xt, t]` bridge; the network produces epsilon predictions and `ddpmSample` validates their shape and finiteness. |
| `models/training.wl` | `makeDiffusionTrainingBatch` | `makeDiffusionTrainingBatch[cleanSamples_List, schedule_Association, opts___]` | Create a batch of epsilon-prediction training associations with automatic, seeded, or explicit times and noises. |
| `models/training.wl` | `makeConditionedDiffusionTrainingBatch` | `makeConditionedDiffusionTrainingBatch[cleanSamples_List, conditioningValues_List, schedule_Association, opts___]` | Create a diffusion training batch with one aligned opaque conditioning value per sample. |
| `models/latent.wl` | `makeLatentDiffusionTrainingSample` | `makeLatentDiffusionTrainingSample[encoder_, input_, t_Integer, schedule_Association]` | Encode an external input once and create a Gaussian epsilon-prediction training association in latent space, with optional explicit latent noise. |
| `models/latent.wl` | `latentDiffusionSample` | `latentDiffusionSample[decoder_, predictor_, initialLatent_, schedule_Association, opts___]` | Select DDPM or DDIM through `"Sampler"`, forward only `"SamplerOptions"`, decode the final latent once, and preserve latent trajectory metadata. |
