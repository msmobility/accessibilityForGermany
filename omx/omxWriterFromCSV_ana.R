source("C:/code/accessibilityForGermany/omx/omx2.R")

path = "C:/models/transit_germany/output/skims/germany_ld_bus_1/ld_bus_pt_trainshare_bytime.csv.gz"
#path = "C:/models/transit_germany/output/skims/germany_auto_2/car_distances.csv.gz"
#path = "Z:/projects/2019/BASt/data/matrices/car/car_eu_matrices_11717/car_traveltimes.csv.gz"
tt_skim = read_csv2(gzfile(path), guess_max = 100)
m = as.integer(max(tt_skim$FROM))

mat1 = matrix(nrow= m, ncol = m)
tt_skim$VALUE = as.numeric(tt_skim$VALUE)
for (i in 1:nrow(tt_skim)){
  if (i %% 1000000 == 0){
    print(i)
  }
  mat1[tt_skim$FROM[i],tt_skim$TO[i]] = tt_skim$VALUE[i]
}


fileName = "C:/models/transit_germany/output/skims/germany_ld_bus_1/ld_bus_inVehProp.omx"
createFileOMX(fileName, m,m )

writeMatrixOMX(fileName, mat1, "mat1",NaValue = 360000)

lookup = 1:11864L


writeLookupOMX(fileName, LookupVector = lookup, LookupSaveName = "vlookup1")
listOMX(fileName)

#only if the rhdf5 packege is not installed
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("rhdf5")

