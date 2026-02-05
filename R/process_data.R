## COMP21 Oct 2025 extract
## Supplied by Mike E
comp2021_Oct2025 <- get(load(paste0(primary_path, "comp2021_Oct2025.RData")))

## Video codes
## Supplied by Mike E
video_codes <- read.csv(paste0(primary_path, "video_codes.csv"))
video_codes_short <-
  video_codes |> 
  dplyr::select(VIDEO_CODE, GROUP_CODE, COMP_2021, COMP_2021_DESCRIPTION) |> 
  filter(!GROUP_CODE %in% c("IN", "WA", "SG", "OT", "AB")) |> 
  dplyr::distinct(GROUP_CODE, COMP_2021, COMP_2021_DESCRIPTION)

## Group cover transect
## Supplied by Mike E
group_cover_transect <- get(
  load(file = paste0(primary_path, "group_cover_transect.RData"))
)

## Broad COMP21 group lookups
## Supplied by Mike E
broad.gps <- read.csv(
  file = paste0(primary_path,
    "comp21_broad_groups_lookup.csv"),
  strip.white = TRUE)

## Process data
comp2021_Oct2025_df <-
  comp2021_Oct2025 |>
  filter(!COMP_2021 %in%
           c('F_SC_SCl_LOB', "COR_CBCF", "COR_CE", "COR_CL", "COR_CMCS")) |>
  left_join(broad.gps) |>
  mutate(ms_group = factor(ms_group)) |>
  filter(ms_group %in%
           c("Acropora", "branching coral", "encrusting foliose coral",
             "massive Porites", "massive submassive coral", "solitary coral")) |> 
  dplyr::mutate(P_CODE = factor(P_CODE),
    A_SECTOR = factor(A_SECTOR),
    SHELF = factor(SHELF),
    AIMS_REEF_NAME = factor(AIMS_REEF_NAME),
    cREPORT_YEAR = factor(REPORT_YEAR),
    SITE_NO = factor(SITE_NO),
    TRANSECT_NO = factor(TRANSECT_NO),
    ms_group = factor(ms_group),
    cREPORT_YEAR = factor(REPORT_YEAR),
    cCOMP_2021 = factor(COMP_2021),
    SITE_DEPTH = ifelse(P_CODE != "IN",9L,SITE_DEPTH),
    SITE_DEPTH = factor(SITE_DEPTH),
    Reef_unique = as.factor(paste(AIMS_REEF_NAME,REEF_ZONE,SITE_DEPTH))) |> 
  group_by(P_CODE, A_SECTOR, SHELF, AIMS_REEF_NAME,
    Reef_unique, SITE_DEPTH, REPORT_YEAR, SITE_NO, TRANSECT_NO,
    cREPORT_YEAR, ms_group, cCOMP_2021, COMP_2021) |>
  dplyr::summarise(cover = sum(COVER),
    n.points = sum(POINTS),
    total.points = unique(total.points)) |>
  ungroup() |> 
  mutate(
    Decade = case_when(
      REPORT_YEAR %in% c(1993,1994,1995,1996,1997,1998,1999,2000) ~ "1990s",
      REPORT_YEAR %in% c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010) ~ "2000s",
      REPORT_YEAR %in% c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020) ~ "2010s",
      .default = "2020s"
    ),
    Decade = factor(Decade)
  ) |> 
  mutate(
    Decade2 = case_when(
      REPORT_YEAR %in% c(1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003) ~ "1993-2003",
      REPORT_YEAR %in% c(2004,2005,2006,2007,2008,2009,2010,2011,2012,2013) ~ "2004-2013",
      .default = "2014-2023"
    ),
    Decade2 = factor(Decade2)
  ) |> 
  mutate(
    half_decade = case_when(
      REPORT_YEAR %in% c(1993,1994,1995) ~ "early_90s",
      REPORT_YEAR %in% c(1996,1997,1998,1999,2000) ~ "late_90s",
      REPORT_YEAR %in% c(2001,2002,2003,2004,2005) ~ "early_00s",
      REPORT_YEAR %in% c(2006,2007,2008,2009,2010) ~ "late_00s",
      REPORT_YEAR %in% c(2011,2012,2013,2014,2015) ~ "early_10s",
      REPORT_YEAR %in% c(2016,2017,2018,2019,2020) ~ "late_10s",
      .default = "early_20s"
    ),
    half_decade = factor(half_decade,
      levels = c('early_90s','late_90s','early_00s','late_00s',
        'early_10s','late_10s','early_20s')) 
  ) |> 
  left_join(video_codes_short,by = 'COMP_2021') |> 
  as.data.frame()

comp2021.points <- comp2021_Oct2025_df

saveRDS(comp2021.points, file = paste0(processed_path, "comp2021.points.rds"))

