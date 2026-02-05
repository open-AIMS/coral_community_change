## Instructions for interactively running on HPC for
## development purposes.
## $ module load singularity
## $ singularity exec -B .:/home/Project ltmp_dashboard1.sif R
## > setwd("R")

library(tidyverse)
library(sf)
library(posterior)
library(future)
library(purrr)
library(furrr)
library(parellel)
library(HDInterval)
library(INLA)
## library(patchwork)

## Prepare directory structure
assign(x = "data_path", value = "../data/", envir = .GlobalEnv)
assign(x = "primary_path", value = "../data/primary/", envir = .GlobalEnv)
assign(x = "processed_path", value = "../data/processed/", envir = .GlobalEnv)
if (!dir.exists(processed_path)) dir.create(processed_path)
assign(x = "modelled_path", value = "../data/modelled/", envir = .GlobalEnv)
if (!dir.exists(modelled_path)) dir.create(modelled_path)
assign(x = "output_path", value = "../output/", envir = .GlobalEnv)
if (!dir.exists(output_path)) dir.create(output_path)

source("process_data.R")
source("fit_models.R")
source("process_models.R")
source("summary_plots.R")
