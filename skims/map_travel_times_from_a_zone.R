pacman::p_load(readr, dplyr, ggplot2,sf,tmap)


modes = c("germany_all_v2/all", "germany_all_v2/bus_tram_subway", "germany_all_v2/bus")


all = list()

for (mode_index in 1:length(modes)){
  path = paste("c:/models/transit_germany/output/skims/",modes[mode_index],"pt_traveltimes.csv.gz", sep  ="")
  tt_skim = read_csv2(gzfile(path), guess_max = 100)
  all[[mode_index]] = tt_skim
}

origin_id = 6476

shp = st_read("c:/models/transit_germany/input/zones/de_zones_attributes.shp")


p =  tm_basemap(leaflet::providers$CartoDB)


for (mode_index in 1:length(modes)){
  times_from = all[[mode_index]] %>% filter(FROM == origin_id)
  times_from$TO = as.numeric(times_from$TO)
  times_from$FROM = as.numeric(times_from$FROM)
  times_from$VALUE = as.numeric(times_from$VALUE)/3600
  
  this_shp = shp %>% left_join(times_from, by = c("TAZ_id"="TO"))

  p = p + tm_shape(this_shp, modes[mode_index])
  p = p + tm_polygons(col ="VALUE", alpha = 0.6) 
}

tmap_leaflet(p)
☺