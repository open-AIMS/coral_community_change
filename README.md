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