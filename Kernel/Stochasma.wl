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
      "models/embeddings.wl",
      "models/adapters.wl"
    }
  ]
]
