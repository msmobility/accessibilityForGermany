pacman::p_load(readr, dplyr, ggplot2,sf,tmap, tidyr, rhdf5, here)

# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("rhdf5")


#source("omx/omx2.R")

source(paste(here(), "/omx/omx2.R", sep = ""))
source("C:/code/accessibilityForGermany/omx/omx2.R")

path = "Z:/projects/2019/BASt/data/matrices/car/omx_car_matrix/omx_de_w_connectors/"

dist_file = "car_11717_travel_dist_m_w_connectors.omx"
time_file = "car_11717_travel_time_m_w_connectors.omx"

d_matrix = readMatrixOMX(paste(path, dist_file, sep =""), "mat1")
t_matrix = readMatrixOMX(paste(path, time_file, sep =""), "mat1")


##Analysis of travel times and distances 

#use a subsample due to the large size
n_zones = 9

zones_subset = sample(1:11717, n_zones)

submatrix = data.frame()

for (i in zones_subset){
  for (j in 1:11717){
    row = data.frame(orig = i, dest = j, t = t_matrix[i,j], d = d_matrix[i,j]) 
    submatrix = submatrix %>% bind_rows(row)
    rm(row)
  }
  print(paste("Completed zone ", i, sep = "" ))
}

ggplot(submatrix, aes(x = d/t * 3.6)) + geom_histogram() + xlim(0,140) + xlab("Speed (km/h)")

ggplot(submatrix, aes(x = d/1000)) + geom_histogram() + xlab("Distance (all) (km)")

ggplot(submatrix, aes(x = d/1000)) + geom_histogram(binwidth = 1) + xlim(0,100) + xlab("Distance (<100) (km)")
 
ggplot(submatrix, aes(x = d/1000, fill = if_else(d/1000 < 10, T, F))) +
  xlim(0,50) + 
  geom_histogram(binwidth = 1) +
  xlab("Distance from every origin to every destination (<50) (km)")

ggplot(submatrix, aes(x = d/1000, fill = if_else(d/1000 < 10, T, F))) +
  geom_histogram(binwidth = 1) +
  xlim(0,50) + 
  xlab("Distance from origin to each destination (<50) (km)") + 
  facet_wrap(.~orig)

ggplot(submatrix, aes(x = d/1000)) + geom_histogram(binwidth = 1) + xlim(0,30) + xlab("Distance (<30) (km)")

ggplot(submatrix, aes(x = d/1000)) + geom_histogram(binwidth = 1) + xlim(0,10) + xlab("Distance (<10) (km)")


ggplot(submatrix, aes(x = d/1000)) + stat_ecdf() + xlim(0,100) + xlab("Distance (<100) (km)")

ggplot(submatrix, aes(x = d/1000)) + stat_ecdf() + xlim(0,30) + xlab("Distance (<30) (km)")

ggplot(submatrix, aes(x = d/1000)) + stat_ecdf() + xlim(0,10) + xlab("Distance (<10) (km)")

##Analysis of intrazonal travel times and distances

intrazonals = data.frame()

for (j in 1:11717){
  intrazonals = intrazonals %>% bind_rows(data.frame(zone = j, t = t_matrix[j,j], d = d_matrix[j,j]))
}

zeros = intrazonals %>% filter(t == 0)

ggplot(intrazonals, aes(x = d/1000)) + geom_histogram(binwidth = 1) + xlab("Distance (all intrazonals) (km)")
ggplot(intrazonals, aes(x = t/60)) + geom_histogram() + xlab("Time (all intrazonals) (min)")
ggplot(intrazonals, aes(x = d/t * 3.6)) + geom_histogram() + xlab("Speed (all intrazonals) (km/h)")


p =  tm_basemap(leaflet::providers$CartoDB)
test = shp %>% left_join(intrazonals, by =c("TAZ_id" = "zone"))
p = p + tm_shape(test)


p = p + tm_polygons(col ="t", alpha = 0.6, breaks = c(0,0.5, 1, 2, 3, 4, 5, 100) ) 
tmap_leaflet(p)
