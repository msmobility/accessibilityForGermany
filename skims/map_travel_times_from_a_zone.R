pacman::p_load(readr, dplyr, ggplot2,sf,tmap, tidyr)


modes = c("germany_ld_train_1/ld_train_")

all = list()
all_adaption = list()
all_share = list()

for (mode_index in 1:length(modes)){
  path = paste("c:/models/transit_germany/output/skims/",modes[mode_index],"pt_traveltimes.csv.gz", sep  ="")
  tt_skim = read_csv2(gzfile(path), guess_max = 100)
  # path = paste("c:/models/transit_germany/output/skims/",modes[mode_index],"pt_adaptiontimes.csv.gz", sep  ="")
  # adaption_skim = read_csv2(gzfile(path), guess_max = 100)
  path = paste("c:/models/transit_germany/output/skims/",modes[mode_index],"pt_trainshare_bytime.csv.gz", sep  ="")
  share = read_csv2(gzfile(path), guess_max = 100)
  all[[mode_index]] = tt_skim
  # all_adaption[[mode_index]] = adaption_skim
  all_share[[mode_index]] = share
  rm(tt_skim)
  # rm(adaption_skim)
  rm(share)
}

# for (mode_index in 1:length(modes)){
#   path = paste("c:/models/transit_germany/output/skims/",modes[mode_index],"pt_traveltimes.csv.gz", sep  ="")
#   tt_skim = read_csv2(gzfile(path), guess_max = 100)
#   # path = paste("c:/models/transit_germany/output/skims/",modes[mode_index],"pt_adaptiontimes.csv.gz", sep  ="")
#   # adaption_skim = read_csv2(gzfile(path), guess_max = 100)
#   path = paste("c:/models/transit_germany/output/skims/",modes[mode_index],"pt_trainshare_bytime.csv.gz", sep  ="")
#   share = read_csv2(gzfile(path), guess_max = 100)
#   all[[mode_index]] = tt_skim
#   # all_adaption[[mode_index]] = adaption_skim
#   all_share[[mode_index]] = share
#   rm(tt_skim)
#   # rm(adaption_skim)
#   rm(share)
# }

origin_id = 6645
#/shp = st_read("c:/models/transit_germany/input/zones/de_zones_attributes.shp")
shp = st_read("c:/models/transit_germany/input/zones/zones_eu_skimCalculation.shp")


p =  tm_basemap(leaflet::providers$CartoDB)

all_times_from = list()

for (mode_index in 1:length(modes)){
  times_from = all[[mode_index]] %>% filter(FROM == origin_id)
  times_from$TO = as.numeric(times_from$TO)
  times_from$FROM = as.numeric(times_from$FROM)
  times_from$VALUE = as.numeric(times_from$VALUE)/3600
  
  share_from = all_share[[mode_index]] %>% filter(FROM == origin_id)
  share_from$TO = as.numeric(share_from$TO)
  share_from$FROM = as.numeric(share_from$FROM)
  share_from$VALUE_SHARE = as.numeric(share_from$VALUE)
  
  share_from = share_from %>% select(TO, FROM, VALUE_SHARE)
  
  times_from  = times_from %>% left_join(share_from, by = c("FROM", "TO"))
  
  #times_from = times_from %>% mutate(travel_time = VALUE_SHARE *  VALUE)
  
  times_from = times_from %>% mutate(travel_time = (VALUE))
  
  
  
  times_from = times_from %>% filter(!is.infinite(travel_time))
  
  all_times_from[[mode_index]] = times_from
    
  this_shp = shp %>% left_join(times_from, by = c("TAZ_id"="TO"))

  p = p + tm_shape(this_shp, paste(modes[mode_index],"time"))
  p = p + tm_polygons(col ="travel_time", alpha = 0.6, border.alpha = 0,
                      breaks = c(0,0.5, 1, 2, 3, 4, 5, 10, 20, 100) ) 
  #p = p + tm_shape(this_shp, paste(modes[mode_index],"share"))
  #p = p + tm_polygons(col ="VALUE_SHARE", alpha = 0.6 )
}

tmap_leaflet(p)




summary = data.frame()
for (mode_index in 1:length(modes)){
  this_summary  = all_times_from[[mode_index]]
  this_summary$mode = modes[mode_index]
  summary = summary %>% bind_rows(this_summary)
  
}


summary_wide = summary %>% pivot_wider(values_from = c("VALUE", "VALUE_SHARE", "travel_time"), names_from = c("mode"))



ggplot(summary_wide, aes(x=`VALUE_germany_all_v2/all`, y = `VALUE_germany_all_v4/all`)) + geom_point(alpha = 0.1) + geom_abline(intercept = 0, slope = 1, color = "red")

write.table(summary_wide, "clipboard-1000k", sep = "\t", row.names = F)

write.csv(times_from, "c:/models/transit_germany/output/skims/trainTimesFromHbf.csv")
  