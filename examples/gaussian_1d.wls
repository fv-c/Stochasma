#!/usr/bin/env wolframscript

root = DirectoryName[DirectoryName[$InputFileName]];
PacletDirectoryLoad[root];
Needs["Stochasma`"];

{cleanData, noise} = BlockRandom[
  SeedRandom[20260916];
  {
    Join[
      RandomVariate[NormalDistribution[-2., 0.25], 1000],
      RandomVariate[NormalDistribution[2., 0.25], 1000]
    ],
    RandomVariate[NormalDistribution[0., 1.], 2000]
  }
];

schedule = makeDiffusionSchedule[
  linearBetaSchedule[250, 10^-4, 0.02]
];
times = {0, 50, 125, 250};
states = Association[
  Table[t -> forwardDiffuse[cleanData, t, schedule, noise], {t, times}]
];

summary = Table[
  <|
    "Time" -> t,
    "Mean" -> Mean[states[t]],
    "StandardDeviation" -> StandardDeviation[states[t]],
    "CorrelationWithClean" -> Correlation[cleanData, states[t]]
  |>,
  {t, times}
];

Print["Clean data: a balanced mixture of N[-2, 0.25] and N[2, 0.25]."];
Print[Dataset[summary]];

histograms = GraphicsRow[
  Table[
    Histogram[
      states[t],
      50,
      "PDF",
      PlotRange -> All,
      PlotLabel -> Row[{"t = ", t}],
      AxesLabel -> {"sample", "density"},
      ImageSize -> 260
    ],
    {t, times}
  ],
  ImageSize -> 1100
];

outputPath = FileNameJoin[{$TemporaryDirectory, "stochasma-gaussian-1d.png"}];
Export[outputPath, histograms];
Print["Histogram comparison exported to: ", outputPath];
