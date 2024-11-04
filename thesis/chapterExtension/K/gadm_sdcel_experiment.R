library(tidyverse)

data0 <- enframe(read_lines("gadm_sdcel_experiment1.txt"), value = "line") |>
  filter(str_detect(line, 'TIME'))

kdtree   <- data0 |> filter(str_detect(line, 'Kdtree')) 
quadtree <- data0 |> filter(str_detect(line, 'Quadtree')) 

data_1 <- kdtree |> bind_rows(quadtree) |>
  separate(col = line, into = c("ts","epoch","appId","tag","partitions","tree","stage","time"), sep = "\\|") |>
  select(partitions, stage, tree, time) |>
  mutate(partitions = as.numeric(partitions) / 1000.0, time = as.numeric(time)) |>
  group_by(partitions, tree, stage) |> summarise(time = mean(time)) 

data0 <- enframe(read_lines("gadm_sdcel_experiment2.txt"), value = "line") |>
  filter(str_detect(line, 'TIME'))

kdtree   <- data0 |> filter(str_detect(line, 'Kdtree')) 
quadtree <- data0 |> filter(str_detect(line, 'Quadtree')) 

data_2 <- kdtree |> bind_rows(quadtree) |>
  separate(col = line, into = c("ts","epoch","appId","tag","tag2","partitions","tree","stage","time"), sep = "\\|") |>
  select(partitions, stage, tree, time) |>
  mutate(partitions = as.numeric(partitions) / 1000.0, time = as.numeric(time)) |>
  group_by(partitions, tree, stage) |> summarise(time = mean(time)) 

data <- data_1 |> bind_rows(data_2) |> filter(partitions < 22)

dataTime <- data |> select(partitions, stage, tree, time) 

## Creation
dataCreation <- dataTime |> filter(stage == "creation") |>
  select(partitions, time, tree)

dataCreation |> write_tsv("gadm_creation_times.tsv")

## Partitioning
dataPartitioning <- dataTime |> filter(stage == "partitioning") |>
  select(partitions, time, tree)

dataPartitioning |> write_tsv("gadm_partitioning_times.tsv")

## Overlay
dataOverlay <- dataTime |> filter(stage == "overlay") |>
  select(partitions, time, tree)

dataOverlay <- read_tsv("gadm_overlay.tsv")

## Space
dataSpace_1 <- enframe(read_lines("gadm_sdcel_experiment1.txt"), value = "line") |>
  filter(str_detect(line, 'INFO')) |>
  filter(str_detect(line, 'space')) |>
  separate(col = line, into = c("ts","epoch","appId","tag","partitions","tree","space","nodes"), sep = "\\|") |>
  select(partitions, tree, nodes) |>
  mutate(partitions = as.numeric(partitions) / 1000.0, nodes = as.numeric(nodes)) |>
  group_by(partitions, tree) |> summarise(nodes = mean(nodes)) 
dataSpace_2 <- enframe(read_lines("gadm_sdcel_experiment2.txt"), value = "line") |>
  filter(str_detect(line, 'INFO')) |>
  filter(str_detect(line, 'space')) |>
  separate(col = line, into = c("ts","epoch","appId","tag","tag2","partitions","tree","space","nodes"), sep = "\\|") |>
  select(partitions, tree, nodes) |>
  mutate(partitions = as.numeric(partitions) / 1000.0, nodes = as.numeric(nodes)) |>
  group_by(partitions, tree) |> summarise(nodes = mean(nodes)) 

dataSpace <- dataSpace_1 |> bind_rows(dataSpace_2) |> filter(partitions < 22000)

dataSpace |> write_tsv("gadm_overlay_space.tsv")