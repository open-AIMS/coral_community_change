Codes in support of coral community change
=============================================

Note, some parts of this codebase are designed to run on a HPC.
In particular, certain sections of `R/fit_models.R` and
`R/process_models.R` make use of either `furrr::map` or
`parallel::mclapply`