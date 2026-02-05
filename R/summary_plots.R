## Read in the models tibble complete with posteriors
secshelf.models <- readRDS(file = paste0(modelled_path,
  "secshelf.models_2.rds"))

## Add a label variable that combines the COMP2021 codes
## with Sector and Shelf
secshelf.models <-
  secshelf.models |>
  mutate(nm = paste(cCOMP_2021, A_SECTOR, SHELF, sep = "_"))

## Make individual dynamite (argh) plots depicting the estimated
## mean and 95% credibility interval conditional on COMP20201
## code, half-decade, sector and shelf.
print(paste0("The Number of available cores:",
  parallelly::availableCores()))
{
  ## Define the function to produce individual plots
  plot_1 <- function(dat, titl, nm) {
    g <-
      dat |> 
      ggplot(aes(y = mean*100, x = half_decade)) +
      geom_bar(stat = 'identity',
        position = position_dodge(width = 1),
        show.legend = TRUE)+
      geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100),
        width = 0,
        position = position_dodge(width = 1),
        linewidth = 0.5) +
      scale_y_continuous('Hard coral cover (mean ± 95% CI)') +
      scale_x_discrete('Decade') +
      facet_grid(~ A_SECTOR ~ SHELF, scales = 'free_y')+
      ggtitle(titl)+
      theme_classic()+
      theme(axis.title = element_text(size = 10, face = 'bold'),
        axis.text.x = element_text(),
        axis.title.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 5.5),
        legend.key.size = unit(0.25, "cm"))+
      guides(fill = guide_legend(ncol = 8))
    
    ggsave(filename = paste0(output_path, nm, ".pdf"),
      plot = g, width = 6, height = 4)
    nm
  }

  ## Generate the plots in parallel
  summary_plots <-
    secshelf.models |>
    dplyr::select(-data, -newdata.yr, -newdata.i.yr, -dat.yr,
      -fitted_values, -posteriors) |> 
    mutate(decadal.plots_files = list(
      parallel::mcmapply(FUN = plot_1,
        summary.dec, cCOMP_2021, nm,
        mc.cores = 60,
        SIMPLIFY = FALSE, USE.NAMES = FALSE
      )
    )) |>
    mutate(decadal.plots_files = decadal.plots_files[[1]])

  print("Saving the summary plots")
  saveRDS(summary_plots, file = paste0(modelled_path, "summary_plots.rds"))
  print("Finished saving the summary plots!")
}

## Generate faceted version of the dynamite plots
{
  ## Define the function to produce individual plots
  faceted_plot <- function(dat, titl) {
    nm <- paste(output_path, titl, ".pdf", sep = "_")
    g <-
      dat |> 
      mutate(
        A_SECTOR = factor(A_SECTOR,
          levels = c('CG','PC','CL','CA','IN','TO','WH','PO','SW','CB')),
        SHELF = factor(SHELF,
          levels = c('I','M','O'))) |> 
      ggplot(aes(y = mean*100, x = half_decade)) +
      geom_bar(stat = 'identity',
        position = position_dodge(width = 1),
        show.legend = TRUE) +
      geom_errorbar(aes(ymin = lower*100, ymax = upper*100),
        width = 0,
        position = position_dodge(width = 1),
        linewidth = 0.5) +
      scale_y_continuous('Hard coral cover (mean ± 95% CI)')+
      scale_x_discrete('Half_decade')+
      facet_grid(~A_SECTOR ~ SHELF, scales = 'free_y')+
      ggtitle(titl) +
      theme_classic() +
      theme(axis.title = element_text(size = 10, face = 'bold'),
        axis.text.x = element_text(),
        axis.title.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 5.5),
        legend.key.size = unit(0.25, "cm"))+
      guides(fill = guide_legend(ncol = 8))
    ggsave(filename = nm,
      plot = g, width = 15, height = 12)
    nm  
  }

  ## Generate the plots in parallel
  secshelf.models.plots <-
    secshelf.models |>   
    ungroup() |> 
    dplyr::select(cCOMP_2021, summary.dec) |> 
    unnest('summary.dec') |> 
    group_by(cCOMP_2021) |>
    nest() |> 
    mutate(decadal.plots.facet = list(
      parallel::mcmapply(FUN = faceted_plot,
        data, cCOMP_2021,
        mc.cores = 60,
        SIMPLIFY = FALSE, USE.NAMES = FALSE
      )
    )) |>
    mutate(decadal.plots.facet = decadal.plots.facet[[1]])
  
  print("Saving the summary faceted plots")
  saveRDS(secshelf.models.plots, file = paste0(modelled_path, "secshelf.models.plots.rds"))
  print("Finished saving the summary facetd plots!")

}

## Perform pairwise contrasts on each half-decade using the full posteriors
{
  ## define the function to perform the contrasts
  apply_contrasts <- function(posteriors) {
    ## tukey matrix to compare every decade to every other decade
    decades <- levels(posteriors$half_decade)
    xmat <- emmeans:::tukey.emmc(decades)

    comp <-
      posteriors |>
      group_by(.draw) |>
      arrange(half_decade) |> 
      summarise(
        frac = exp(as.vector(as.vector(log(value)) %*% as.matrix(xmat))),
        perc = 100 * (frac - 1),
        value = as.vector(as.vector(value) %*% as.matrix(xmat)),
        Decade_comp = names(xmat),
        .groups = "drop"
      ) |>
      ungroup() |>
      group_by(.draw, Decade_comp) |> 
      pivot_longer(cols = c(frac, value, perc), names_to = "stat",
        values_to = "value") |> 
      ungroup()
    comp
  }

  ## perform the contrasts in parallel
  secshelf.models <- secshelf.models |>   
    mutate(comp_posteriors = list(
      parallel::mcmapply(FUN = apply_contrasts,
        posteriors,
        mc.cores = 60,
        SIMPLIFY = FALSE, USE.NAMES = FALSE
      )
    )) |>
    mutate(comp_posteriors = comp_posteriors[[1]])

  print("Saving the applied contrasts")
  saveRDS(secshelf.models, file = paste0(modelled_path, "secshelf.models.rds"))
  print("Finished saving the applied contrasts!")
}

## Summarise half-decadal posterior contrasts
{
  secshelf.models <- secshelf.models |>   
    mutate(comp_sum = purrr::map(.x = comp_posteriors,
      .f =  ~ {
        comp_sum <- .x |>
          #mutate(value=value*-1) |> 
          group_by(Decade_comp, stat) |>
          tidybayes::summarise_draws(median,mean,
            HDInterval::hdi,
            Pl = ~ mean(.x < 0),
            Pg = ~ mean(.x > 0)
          ) |> 
          mutate(heat_map_cat = ifelse(Pl>=0.95,1,
            ifelse(Pl>=0.9,2,
              ifelse(Pg>=0.95,5,
                ifelse(Pg>=0.9,4,3))))
          )
        comp_sum
      }
    ))
}

## Save the tibble 
saveRDS(secshelf.models, file = paste0(modelled_path, 'secshelf.models_3.rds'))

## Extract the summarised posterior contrasts from the tibble
secshelf.models <- readRDS(file = paste0(modelled_path, 'secshelf.models_3.rds'))
comp_2021_decadal_comparison_summary <- secshelf.models |> 
  dplyr::select(cCOMP_2021,A_SECTOR,SHELF,comp_sum) |> 
  unnest('comp_sum') 
saveRDS(comp_2021_decadal_comparison_summary,
  file = paste0(modelled_path, 'comp_2021_decadal_comparison_summary.rds'))


## Create a master summary focussing on the contrast of early 1990s
## and early 2020s
comp_2021_decadal_comparison_summary <- readRDS(
  file = paste0(modelled_path, 'comp_2021_decadal_comparison_summary.rds'))
heat_map_cat_summary <- comp_2021_decadal_comparison_summary |> 
  dplyr::select(cCOMP_2021,A_SECTOR,SHELF,Decade_comp,stat,heat_map_cat) |> 
  filter(stat=='perc') |> 
  pivot_wider(names_from = cCOMP_2021,values_from = heat_map_cat)

heat_map_cat_summary_9020 <- heat_map_cat_summary |> 
  filter(Decade_comp=='1990s - 2020s') |> 
  dplyr::mutate(likely_decline = rowSums(across(ACBX:S_POR_RUS, \(x) x == 5)) )
write.csv(heat_map_cat_summary_9020,
  file = paste0(output_path, 'heat_map_cat_summary_9020.csv'))
saveRDS(heat_map_cat_summary_9020,
  file = paste0(output_path, 'heat_map_cat_summary_9020.rds'))

## Generate a heatmap to summarise the contrasts
comp2021_Oct2025_df <- readRDS(file = paste0(processed_path, "comp2021.points.rds"))
secshelf.models <- readRDS(file = paste0(modelled_path, 'secshelf.models_3.rds'))
group.lookup <- comp2021_Oct2025_df |> 
  dplyr::select(GROUP_CODE,cCOMP_2021,COMP_2021_DESCRIPTION) |> 
  distinct()

heatmap.dat <- secshelf.models |> 
  dplyr::select(cCOMP_2021,A_SECTOR,SHELF,comp_sum) |> 
  unnest('comp_sum') |> 
  left_join(group.lookup,by=c('cCOMP_2021')) |> 
  mutate(cCOMP_2021=factor(cCOMP_2021, levels=c('ACTO','ACBX','ACD','ACSE','G_ECH_CB','G_HYD_CB','G_ISO_B','G_POC','G_POR_B',
    'G_SER','G_STY','F_AGA_CF','F_FUN_CECF','G_ECH_OTH','G_MER','G_MON','G_MYC',
    'G_OXY_ECL_ECY','G_PAC','G_PEC','G_TUR_DUN','G_POR_M','F_AGA_CEMS','F_EUPH_PLU',
    'F_MER_CEMS','F_MER_PLE_PLOC','F_SID_COS_PSA','G_ACA_MIC_HOM','G_AST','G_DIP',
    'G_GAL','G_GON_ALV_BER','G_HYD_OTH','G_ISO_CSE','G_LEP','G_LOB_AUST','G_MOS',
    'G_POR_CECS','S_POR_RUS','F_FUN_CMR','G_TUB_HET'))) |> 
  filter(stat=='perc',
    GROUP_CODE=='HC') |> 
  group_by(Decade_comp) |> 
  nest()

heatmap.dat <- heatmap.dat |> 
  mutate(heatmap.fig=purrr::map2(.x=data,
    .y=Decade_comp,
    .f=~{
      .x |> 
        ggplot(aes(x=cCOMP_2021,y=SHELF,fill=factor(heat_map_cat)))+
        geom_tile(alpha=0.5)+
        scale_fill_manual(labels=c('likely increase','possible increase','no change',
          'possible decrease','likely decrease'),
          values=c('blue','lightblue','white','orange','red'))+
        facet_grid(A_SECTOR~.,switch='y',space='free_y')+
        coord_cartesian(expand=0)+
        ggtitle(.y)+
        theme_bw()+
        theme(axis.text.x=element_text(size=8,angle=45,hjust=1),#vjust=0.5),
          strip.placement.y = 'outside',
          strip.background.y = element_blank(),
          panel.spacing.y = unit(0,'cm'),
          axis.title = element_blank(),
          legend.title = element_blank())
    }))

comp21_decadal_comparison_plot <-
  heatmap.dat |> 
  filter(Decade_comp == "early_90s - early_20s") |>
  _[["heatmap.fig"]][[1]]
ggsave(comp21_decadal_comparison_plot,
  file = paste0(output_path, 'comp21_decadal_comparison_plot_early_90s_to_early_20s.png'),
  height=9, width=12)
