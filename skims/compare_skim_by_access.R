pacman::p_load(readr, dplyr, ggplot2,sf,tmap, tidyr)


modes = c("germany_all_v2/bus", "germany_all_v1/bus")
mode_names = c("new_bus", "old_bus")


all = list()

for (mode_index in 1:length(modes)){
  path = paste("c:/models/transit_germany/output/skims/",modes[mode_index],"pt_traveltimes.csv.gz", sep  ="")
  tt_skim = read_csv2(gzfile(path), guess_max = 100)
  all[[mode_index]] = tt_skim
}

rm(tt_skim)

population <- read_csv("C:/models/transit_germany/input/zones/pop_per_zone_TAZ_id.csv")

population = population %>% select(id = TAZ_id, pop)


alpha = 1 #exponent of the population
beta = -0.4 #

accessibility = data.frame()

for (mode_index in 1:length(modes)){
  tt = all[[mode_index]]
  tt$TO = as.numeric(tt$TO)
  tt$FROM = as.numeric(tt$FROM)
  tt$VALUE = as.numeric(tt$VALUE)/3600 
  
  tt = tt %>% left_join(population, by  =c("TO" = "id"))
  tt = tt %>% rowwise() %>% 
    mutate(exponential = exp(beta * VALUE)) %>% 
    mutate(acc = pop^alpha * exponential)
  
  this_accessibility = tt %>% group_by(FROM) %>% summarize(access = sum(acc, na.rm = T))
  this_accessibility$case = mode_names[mode_index]
  
  accessibility = accessibility %>% bind_rows(this_accessibility)
  rm(this_accessibility)
  print(mode_names[mode_index])
}


accessibility_2 = accessibility %>% pivot_wider(names_from = "case", values_from = "access")


ggplot(accessibility_2, aes(x = old_bus, y = new_bus)) +
  geom_point(alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, color = "red")

