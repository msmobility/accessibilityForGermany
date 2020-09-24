pacman::p_load(readr, dplyr, ggplot2,sf,tmap, tidyr)


modes = c("germany_all_v2/all", "germany_all_v4/all")

all = list()
all_adaption = list()

for (mode_index in 1:length(modes)){
  path = paste("c:/models/transit_germany/output/skims/",modes[mode_index],"pt_traveltimes.csv.gz", sep  ="")
  tt_skim = read_csv2(gzfile(path), guess_max = 100)
  path = paste("c:/models/transit_germany/output/skims/",modes[mode_index],"pt_adaptiontimes.csv.gz", sep  ="")
  adaption_skim = read_csv2(gzfile(path), guess_max = 100)
  all[[mode_index]] = tt_skim
  all_adaption[[mode_index]] = adaption_skim
  rm(tt_skim)
  rm(adaption_skim)
}

origin_id = 6476

shp = st_read("c:/models/transit_germany/input/zones/de_zones_attributes.shp")


p =  tm_basemap(leaflet::providers$CartoDB)

all_times_from = list()

for (mode_index in 1:length(modes)){
  times_from = all[[mode_index]] %>% filter(FROM == origin_id)
  times_from$TO = as.numeric(times_from$TO)
  times_from$FROM = as.numeric(times_from$FROM)
  times_from$VALUE = as.numeric(times_from$VALUE)/3600
  
  adaption_times_from = all_adaption[[mode_index]] %>% filter(FROM == origin_id)
  adaption_times_from$TO = as.numeric(adaption_times_from$TO)
  adaption_times_from$FROM = as.numeric(adaption_times_from$FROM)
  adaption_times_from$VALUE_ADAPTION = as.numeric(adaption_times_from$VALUE)/3600
  
  adaption_times_from = adaption_times_from %>% select(TO, FROM, VALUE_ADAPTION)
  
  times_from  = times_from %>% left_join(adaption_times_from, by = c("FROM", "TO"))
  
  times_from$travel_time = times_from$VALUE - times_from$VALUE_ADAPTION
  
  all_times_from[[mode_index]] = times_from
    
  this_shp = shp %>% left_join(times_from, by = c("TAZ_id"="TO"))

  p = p + tm_shape(this_shp, modes[mode_index])
  p = p + tm_polygons(col ="VALUE", alpha = 0.6, breaks = c(0,0.5, 1, 2, 3, 4, 5, 100) ) 
}

tmap_leaflet(p)



summary = data.frame()
for (mode_index in 1:length(modes)){
  this_summary  = all_times_from[[mode_index]]
  this_summary$mode = modes[mode_index]
  summary = summary %>% bind_rows(this_summary)
  
}


summary_wide = summary %>% pivot_wider(values_from = c("VALUE", "VALUE_ADAPTION", "travel_time"), names_from = c("mode"))



ggplot(summary_wide, aes(x=`VALUE_germany_all_v2/all`, y = `VALUE_germany_all_v4/all`)) + geom_point(alpha = 0.1) + geom_abline(intercept = 0, slope = 1, color = "red")

write.table(summary_wide, "clipboard-1000k", sep = "\t", row.names = F)

  