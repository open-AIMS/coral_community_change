Codes in support of coral community change
=============================================

Note, some parts of this codebase are designed to run on a HPC.
In particular, certain sections of `R/fit_models.R` and
`R/process_models.R` make use of either `furrr::map` or
`parallel::mclapply`.

The repo is structured as:

```
\
|- R
|  |- main.R
|  |- process_data.R
|  |- fit_models.R
|  |- process_models.R
|  |- summary_plots.R
|- LICENCE
|- Makefile
|- README.md
|- analysis.slurm
```

To be able to actually run the code, the structure needs to be:

```
\
|- data
|  |- primary
|  |  |- comp2021_Oct2025.RData
|  |  |- video_codes.csv
|  |  |- group_cover_transect.RData
|  |  |- comp21_broad_groups_lookup.csv
|  |- processed
|  |- modelled
|- R
|  |- main.R
|  |- process_data.R
|  |- fit_models.R
|  |- process_models.R
|  |- summary_plots.R
|- outputs
|- LICENCE
|- Makefile
|- README.md
|- analysis.slurm
|- ltmp_dashboard1.sif
```

To run the full codebase on a HPC:

```
make slurm_R
```

## Interactively running codes

After cloning the repo locally, you can step through the code.
The intended order of R scripts is:
- main.R
  - this is actually the main entry point and can be used to
    run the entire codebase
  - important it loads the required R packages and defines/creates the paths
- process_data.R
  - this script reads in the primary data sources and performs some data
    wrangling tasks
  - processed data are stored in `/data/processed`
- fit_models.R
  - this script prepares the data for `INLA` modelling
  - this script fits the `INLA` models in parellel using `furrr`
  - Note, it assumes there are at least 20 CPU cores available, if
    running outside a HPC environment, you are advised to reduced this
  - individual models are stored in `/data/modelled`
- process_models.R
  - this script extracts posteriors from the `INLA` models
  - this script performs various contrasts on the posteriors
  - collated data are stored in `/data/modelled/`
- summary_plots.R
  - this script collates the contrasts and generates a summary
    heat map
  - figures are output to `/output/`
