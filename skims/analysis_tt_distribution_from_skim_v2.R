pacman::p_load(readr, dplyr, ggplot2,sf,tmap, tidyr, rhdf5, here)

# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("rhdf5")


#source("omx/omx2.R")

source(paste(here(), "/omx/omx2.R", sep = ""))
source("C:/code/accessibilityForGermany/omx/omx2.R")

mode = "ld_train_v4"

path = "c:/models/transit_germany/output/skims/"

matrices_file = "_matrices.omx"

omx_file_path = paste(path,mode, "/", mode, matrices_file, sep  ="")

listOMX(omx_file_path)

time = readMatrixOMX(omx_file_path, "travel_time_s")
distance = readMatrixOMX(omx_file_path, "distance_m")
access_time = readMatrixOMX(omx_file_path, "access_time_s")
egress_time = readMatrixOMX(omx_file_path, "egress_time_s")

transfers = readMatrixOMX(omx_file_path, "train_time_share")

##Analysis of travel times and distances 

#use a subsample due to the large size
n_zones = 200

zones_subset = sample(1:11879, n_zones)

submatrix = data.frame()

for (i in zones_subset){
  for (j in zones_subset){
    row = data.frame(orig = i,
                     dest = j, 
                     t = time[i,j],
                     d = distance[i,j],
                     a = access_time[i,j],
                     e = egress_time[i,j]) 
    submatrix = submatrix %>% bind_rows(row)
    rm(row)
  }
  print(paste("Completed zone ", i, sep = "" ))
}

ggplot(submatrix, aes(x = d/t * 3.6)) + geom_histogram() +
  xlab("Speed (km/h)") + xlim(0,150)

ggplot(submatrix, aes(x = d/(t+a+e) * 3.6)) + geom_histogram() +
  xlab("Speed (km/h)") + xlim(0,150)

ggplot(submatrix, aes(x = d/1000)) +
  geom_histogram() + 
  xlab("Distance (all) (km)")

ggplot(submatrix, aes(x = a/3600)) +
  geom_histogram() + 
  xlab("Access (h)")

ggplot(submatrix, aes(x = e/3600)) +
  geom_histogram() + 
  xlab("Egress (h)")

ggplot(submatrix, aes(x = d/1000, y=t/3600)) + geom_point(alpha = 0.1)



##Analysis of intrazonal travel times and distances

intrazonals = data.frame()

for (j in 1:11879){
  intrazonals = intrazonals %>% bind_rows(data.frame(zone = j, t = time[j,j], d = distance[j,j]))
}

ggplot(intrazonals, aes(x = d/1000)) + geom_histogram(binwidth = 1) + xlab("Distance (all intrazonals) (km)")
ggplot(intrazonals, aes(x = t/60)) + geom_histogram() + xlab("Time (all intrazonals) (min)")
ggplot(intrazonals, aes(x = d/t * 3.6)) + geom_histogram() + xlab("Speed (all intrazonals) (km/h)")


