# Clean Woreda File

library(here)
library(sf)
library(leaflet)
library(tidyverse)
library(stringr)

# Woreda -----------------------------------------------------------------------

## Load data
woreda_sf <- read_sf(here("raw_data", "ET_Admin3C_2023_3.geojson"))
woreda_df <- read_csv(here("raw_data", "Woreda_Master.csv"))

## Make Woreda ID
woreda_sf <- woreda_sf %>%
  dplyr::mutate(wid = fnid %>%
                  str_sub(-6, -1) %>%
                  as.numeric())

## Select variables
woreda_sf <- woreda_sf %>%
  dplyr::select(wid)

woreda_df <- woreda_df %>%
  dplyr::select(wid, 
                woreda, zone, region,
                pop_u, area) %>%
  dplyr::mutate(id = wid,
                pop_density = pop_u / area,
                label = paste0(
                  "Woreda: ", woreda, "<br>",
                  "Zone: ", zone, "<br>",
                  "Region: ", region, "<br>",
                  "Population: ", pop_u, "<br>",
                  "Population Density: ", round(pop_density, 2)
                ))

## Merge
woreda_sf <- woreda_sf %>%
  left_join(woreda_df, by = "wid")

## Fix and simplify geometries
woreda_sf <- woreda_sf %>% 
  st_make_valid() %>%
  st_simplify(100)

saveRDS(woreda_sf, here("clean_data", "woreda.Rds"))

# Woreda -----------------------------------------------------------------------
mde_df    <- read_csv(here("raw_data", "MDEs_Rural_1_to_850.csv"))

saveRDS(mde_df, here("clean_data", "mdes.Rds"))


# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = woreda_simp_sf)
