With[
  {root = DirectoryName[DirectoryName[$InputFileName]]},
  Scan[
    Get,
    FileNameJoin[{root, #}] & /@ {
      "core/schedules.wl",
      "core/forward.wl",
      "core/reverse.wl",
      "core/objectives.wl",
      "core/sampling.wl",
      "core/ddim.wl",
      "core/categorical.wl",
      "models/embeddings.wl",
      "models/conditioning.wl",
      "models/adapters.wl",
      "models/training.wl",
      "models/latent.wl"
    }
  ]
]
