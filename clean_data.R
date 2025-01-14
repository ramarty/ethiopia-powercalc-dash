# Clean Woreda File

library(here)
library(sf)
library(leaflet)

woreda_sf <- read_sf(here("raw_data", "ET_Admin3C_2023_3.geojson"))

woreda_sf <- woreda_sf %>%
  dplyr::select(id, name, full_name)

woreda_sf <- woreda_sf %>% 
  st_make_valid()

woreda_simp_sf <- woreda_sf %>% st_simplify(10000)

saveRDS(woreda_simp_sf, here("clean_data", "woreda.Rds"))

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = woreda_simp_sf)
