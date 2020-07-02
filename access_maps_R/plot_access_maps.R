pacman::p_load(dplyr, readr, sf, tmap, leaflet, ggplot2)

##Path, zone system#############
#edit the paths to the required information
path_to_access_results_folder = "c:/users/carlloga/LRZ Sync+Share/CAPE/data/accessibility/"
path_to_zone_system_shp = "c:/users/carlloga/LRZ Sync+Share/CAPE/data/zones_de_upd/Zone_shape_file/de_zones_attributes.shp"

zones = st_read(path_to_zone_system_shp)
zones = zones %>% select(TAZ_id)

##Car#############

#read accessibility

car_access = read_csv(paste(path_to_access_results_folder, "car_access_11717.csv", sep = ""))

zones = zones %>% left_join(car_access)

#scale - do we need to scale?
zones = zones %>%
  mutate(access_pop_04_tt_norm = access_pop_04_tt / max(access_pop_04_tt, na.rm = T) * 100) %>%
  mutate(access_pop_10_tt_norm = access_pop_10_tt / max(access_pop_10_tt, na.rm = T) * 100)

p =  tm_basemap(leaflet::providers$CartoDB)

p = p + tm_shape(zones, "matsim") +
  tm_polygons(alpha = 0.6, "access_pop_10_tt_norm", border.alpha = 0.0, breaks = 10*0:10)

tmap_leaflet(p)



##Pt#############

pt_access = read_csv(paste(path_to_access_results_folder, "summary_accessibility_pt_11717.csv", sep = ""))

zones = zones %>% left_join(pt_access)

#scale - do we need to scale?

zones = zones %>%
  mutate(train_b_04 = train_b_04 / max(train_b_04, na.rm = T) * 100) %>%
  mutate(train_b_10 = train_b_10 / max(train_b_10, na.rm = T) * 100)

p =  tm_basemap(leaflet::providers$CartoDB)

p = p + tm_shape(zones, "matsim") +
  tm_polygons(alpha = 0.6, "train_b_04", border.alpha = 0.0, breaks = 10*0:10)

tmap_leaflet(p)
