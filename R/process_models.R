## Read in the tibble containing paths to the fitted INLA models
secshelf.models <- readRDS(file = paste0(modelled_path, "secshelf.models.rds"))


## Add the fitted values from the INLA models
options(future.globals.maxSize = 2 * 1024^3)
old_plan <- future::plan(future::multisession, workers = 20)

results <- furrr::future_pmap(.l = list(
  secshelf.models$newdata.yr,
  secshelf.models$mod,
  secshelf.models$newdata.i.yr),
  .f = ~ {
    newdata.yr <- ..1
    mod_file <- ..2
    newdata.i.yr <- ..3
    mod <- readRDS(mod_file)
    fitted_values <-
      newdata.yr |> 
        bind_cols(mod$summary.fitted.values[newdata.i.yr,]) |> 
        mutate(A_SECTOR = factor(A_SECTOR,
          levels = c('CG','PC','CL','CA','IN','TO','WH','PO','SW','CB'))) 
    list(fitted_values = fitted_values)
  },
  .progress = FALSE, 
  .options = furrr::furrr_options(scheduling = Inf)
)

secshelf.models <-
  secshelf.models |>
  mutate(fitted_values = sapply(results, FUN = \(x) x))


## Extracting the full posteriors from the INLA models
## ignore the warnings about seed!=0
results <- furrr::future_pmap(.l = list(
  secshelf.models$newdata.yr,
  secshelf.models$mod, 
  secshelf.models$newdata.i.yr),
  .f = ~ {
    newdata_yr <- ..1
    mod_file <- ..2
    newdata_i_yr <- ..3
    mod <- readRDS(mod_file)
    draws <- inla.posterior.sample(n = 1000, mod, seed = 123)
    cellmeans <- sapply(draws, function(x) x[[2]][newdata_i_yr])    # rearranging posterior draws into a meaningful format
    ## back-transform to response scale
    posteriors <- newdata_yr |>
    cbind(plogis(cellmeans)) |> 
    pivot_longer(cols = matches("[0-9]"), names_to = ".draw")
    list(posteriors = posteriors)
  },
  .progress = FALSE, 
  .options = furrr::furrr_options(scheduling = Inf)
)
  
secshelf.models <-
  secshelf.models |>
  mutate(posteriors = sapply(results, FUN = \(x) x))

saveRDS(secshelf.models, file = paste0(modelled_path, "secshelf.models_1.rds"))
secshelf.models <- readRDS(file = paste0(modelled_path, "secshelf.models_1.rds"))

## Summarise the model posteriors
secshelf.models <-
  secshelf.models |>  
  mutate(summary.dec = purrr::map(.x = posteriors,
    .f = ~ .x |> 
      dplyr::select(-n.points, -Reef_unique, -ReefSite,
        -ReefSiteTransect, -secshelfhalfdecade, -total.points) |> 
      group_by(A_SECTOR, SHELF, half_decade) |> 
      tidybayes::summarise_draws(median, mean, HDInterval::hdi))) 

saveRDS(secshelf.models, file = paste0(modelled_path, "secshelf.models_2.rds"))

future::plan(old_plan)
