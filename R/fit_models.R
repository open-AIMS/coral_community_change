## read in the processed data
comp2021.points <- readRDS(file = paste0(processed_path, "comp2021.points.rds"))

## Prepare for INLA modelling (Stage 1)
## - declare factors
secshelf.models <-
  comp2021.points |>
  dplyr::select(cCOMP_2021, A_SECTOR, SHELF) |>
  distinct() |> 
  #slice(1:10) |> 
  mutate(n = 1:n(), N = n()) |>                                          # purely so it can count
  mutate(data = pmap(.l = list(cCOMP_2021, A_SECTOR, SHELF, n, N),       # map = will run a loop for each combination of variables map works on one column only - pmap works on multiple
    .f = ~{                                                              # .f = function to apply (in this case its an unnamed function that creates the dataframe to model)
      ## cCOMP_2021 <- ..1
      ## A_SECTOR <- ..2
      ## SHELF <- ..3
      n <- ..4                                    
      N <- ..5
      print(paste("", n, "/", N))                                        # poor person's progress counter using n from line 17
      comp2021.points |>
        filter(cCOMP_2021 == ..1,
          A_SECTOR == ..2,
          SHELF == ..3) |> 
        mutate(
          A_SECTOR = factor(A_SECTOR,
            levels = c('CL','PC','CG','CA','IN','TO','WH','PO','SW','CB')),
          ReefSite = factor(paste(Reef_unique, SITE_NO)),
          ReefSiteTransect = factor(paste(Reef_unique, SITE_NO, TRANSECT_NO)),
          secshelfhalfdecade = factor(paste(A_SECTOR, SHELF, half_decade))) |> 
        droplevels()
    })) |>
  as_tibble()

## Prepare for INLA modelling (Stage 2)
## - add newdata (prediction cases)
secshelf.models <-
  secshelf.models |> 
  mutate(newdata.yr = purrr::map(.x = data,                              #using map to create new column called newdata.yr prediction data for INLA (only has one input - column called 'data')
    .f = ~{
      .x |> 
        dplyr::select(A_SECTOR, SHELF, half_decade) |>
        distinct() |>
        mutate(n.points = NA, 
          Reef_unique = NA, 
          ReefSite = NA, 
          ReefSiteTransect = NA,
          secshelfhalfdecade = factor(paste(A_SECTOR,SHELF,half_decade)),
          total.points = 1)
    }))

## Prepare for INLA modelling (Stage 3)
## - add newdata indicies
secshelf.models <-
  secshelf.models |>                                                     # row indices for the new data - only want predictions for new data rows, not for the observed data - so its tacked on the end of the raw dataframe
  mutate(newdata.i.yr = map2(.x = newdata.yr,                            # uses map2 bevcause there's 2 inputs .x and .y
    .y = data,
    .f = ~1:nrow(.x) + (nrow(.y))))

## Prepare for INLA modelling (Stage 3)
## - add combined data and prediction cases
secshelf.models <-
  secshelf.models |>                                                     # combining the input data plus newdata into a single df for input into INLA
  mutate(dat.yr = map2(.x = data,
    .y = newdata.yr,
    .f = ~.x %>% bind_rows(.y)))

## Fit the INLA models
## - assumes you have 20 CPU cores available - alter as appropriate
options(future.globals.maxSize = 2 * 1024^3)
old_plan <- future::plan(future::multisession, workers = 20)

## IMPORTANT - since it takes a while to run all the models, this
## function will check whether the model file exists and if it is, it
## will exit and return the model path.  If it does not exist, then it
## will fit the model and then return the model path.  So, if you have
## already fit the model for a combination, but want it to be re-run,
## then delete the model file from data/modelled

## Also IMPORTANT - storing large numbers of INLA models in a tibble
## is not a good idea as they are each very large.  Instead, we will
## save the individual models to disk and any keep paths to the files
## in the R tibble.
fit_model <- function(dat.yr, n, N) {
      print(paste("", n, "/", N))
      mod_name <- paste0(modelled_path, 'mod_',
        dat.yr$cCOMP_2021[1],
        '_',
        dat.yr$A_SECTOR[1],
        '_',
        dat.yr$SHELF[1],
        '.rda')
      print(mod_name)
      if (file.exists(mod_name)) return(mod_name)
        
      if (length(unique(dat.yr$secshelfdecade)) == 1) {
        print("Only a single sector/shelf/decade present")
        mod <- 
          inla(n.points~1 + #secshelfdecade+#A_SECTOR*SHELF*Decade +
            f(Reef_unique, model = 'iid') +
              f(ReefSite, model = 'iid') +
              f(ReefSiteTransect, model = 'iid') +
              f(REPORT_YEAR,model = 'iid'),
            data = dat.yr,     #.x,
            Ntrials = (dat.yr$total.points),
            family = "binomial",
            control.compute = list(config = TRUE),
            control.predictor = list(link = 1))
      } else {                                                           # give me a list of secshelfdec combinations that are all zeroes
        dex <- dat.yr |> group_by(secshelfhalfdecade)|>
          summarise(sum = sum(n.points,na.rm = TRUE),
            length(n.points)) |> 
          filter(sum == 0) |> pull(secshelfhalfdecade)
        print(dex)
        if (length(dex) == 0) {                                          # if no zeros run normal model (default priors)
          print("No zero-only combindations present - regular model")
          mod <- 
            inla(n.points~-1 + secshelfhalfdecade+
                   f(Reef_unique, model = 'iid') +
                   f(ReefSite, model = 'iid') +
                   f(ReefSiteTransect, model = 'iid') +
                   f(REPORT_YEAR,model = 'iid'),
              data = dat.yr,#.x,
              Ntrials = (dat.yr$total.points),
              family = "binomial",
              control.compute = list(config = TRUE),
              control.predictor = list(link = 1))
        } else {                                                         # if there are some zero combinations run this model - specified strong priors
          print("Zero-only combinations present - informative priors model used")
          mod <- 
            inla(n.points~secshelfhalfdecade+
                   f(Reef_unique, model = 'iid') +
                   f(ReefSite, model = 'iid') +
                   f(ReefSiteTransect, model = 'iid') +
                   f(REPORT_YEAR,model = 'iid'),
              data = dat.yr,#.x,
              Ntrials = (dat.yr$total.points),
              family = "binomial",
              control.predictor = list(link = 1))
          prior_terms <- paste0('secshelfhalfdecade',dex) 
          prior_mean <- lapply(prior_terms, function(x) log(0.001))
          prior_mean <- append(prior_mean, 0)
          names(prior_mean) <- c(prior_terms, "default")
          prior_prec <- lapply(prior_terms, function(x) 1)
          prior_prec <- append(prior_prec, 0.000001)
          names(prior_prec) <- c(prior_terms, "default")
          mod <- inla(n.points~-1+secshelfhalfdecade+
                      f(Reef_unique, model = 'iid') +
                      f(ReefSite, model = 'iid') +
                      f(ReefSiteTransect, model = 'iid') +
                      f(REPORT_YEAR,model = 'iid'),
            data = dat.yr,#.x,
            Ntrials = (dat.yr$total.points),
            family = "binomial",
            control.predictor = list(link = 1),
            control.compute = list(config = TRUE),
            control.fixed = list(mean = prior_mean,
              prec = prior_prec))
        }
        saveRDS(mod,file = mod_name)
        return(mod_name)
      }
      return(mod_name)
}

## Fit the model in parallell using furrr
results <- furrr::future_pmap(.l = list(secshelf.models$dat.yr,
  secshelf.models$n,
  secshelf.models$N),
  .f = ~ {
    dat.yr <- ..1
    n <- ..2
    N <- ..3
    mod.inla <- fit_model(dat.yr, n, N)
    mod.inla
  },
  .progress = FALSE, 
  .options = furrr::furrr_options(scheduling = Inf)
)

## Restore the old future plan
future::plan(old_plan)

## saveRDS(results, file = paste0(processed_path, "results.rds"))
## results <- readRDS(file = paste0(processed_path, "results.rds"))

## Pack the results into the main tibble
secshelf.models <-
  secshelf.models |>
  mutate(mod = sapply(results, FUN = \(x) x))

## Save the tibble
saveRDS(secshelf.models, file = paste0(modelled_path, "secshelf.models.rds"))


