library(tidyverse)

data0 <- enframe(read_lines("mainus_sdcel_experiment3.txt"), value = "line") |>
  filter(str_detect(line, 'TIME'))

kdtree   <- data0 |> filter(str_detect(line, 'Kdtree')) 
quadtree <- data0 |> filter(str_detect(line, 'Quadtree')) 

data <- kdtree |> bind_rows(quadtree) |>
  separate(col = line, into = c("ts","epoch","appId","tag","partitions","tree","stage","time"), sep = "\\|") |>
  select(partitions, stage, tree, time) |>
  mutate(partitions = as.numeric(partitions) / 1000, time = as.numeric(time)) |>
  group_by(partitions, tree, stage) |> summarise(time = mean(time)) 

dataTime <- data |> select(partitions, stage, tree, time)

## Creation
dataCreation <- dataTime |> filter(stage == "creation") |>
  select(partitions, time, tree)

dataCreation |> write_tsv("mainus_creation_times.tsv")

## Partitioning
dataPartitioning <- dataTime |> filter(stage == "partitioning") |>
  select(partitions, time, tree)

dataPartitioning |> write_tsv("mainus_partitioning_times.tsv")

## Overlay
dataOverlay <- dataTime |> filter(stage == "overlay") |>
  select(partitions, time, tree)

dataOverlay |> write_tsv("mainus_overlay_times.tsv")

## Space
dataSpace <- enframe(read_lines("mainus_sdcel_experiment3.txt"), value = "line") |>
  filter(str_detect(line, 'INFO')) |>
  filter(str_detect(line, 'space')) |>
  separate(col = line, into = c("ts","epoch","appId","tag","partitions","tree","space","nodes"), sep = "\\|") |>
  select(partitions, tree, nodes) |>
  mutate(partitions = as.numeric(partitions) / 1000, nodes = as.numeric(nodes)) |>
  group_by(partitions, tree) |> summarise(nodes = mean(nodes)) 

dataSpace |> write_tsv("mainus_overlay_space.tsv")
