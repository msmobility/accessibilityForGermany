# # # # # # # # # # # # # # # # # # # Potential Accessibility # # # # # # # # # # # # # # # # 

car_tt <- read_delim("E:/skims/skims/tertiary_car_matrix_12.03/car_traveltimes.csv", 
                     ";", escape_double = FALSE, trim_ws = TRUE)

infin <- car_tt[grepl("Inf", car_tt$VALUE),]
tt <- car_tt[!grepl("Inf", car_tt$VALUE),]

tt$tt_h <- tt$VALUE/3600 # convert to hours
tt$exp_04_tt <- exp(0.4*tt$tt_h)  # Beta 0.4
tt$exp_06_tt <- exp(-0.6*tt$tt_h)  # Beta 0.6
tt$exp_08_tt <- exp(-0.8*tt$tt_h)  # Beta 0.8
tt$exp_1_tt <- exp(-1*tt$tt_h)  # Beta 1
tt$exp_1.2_tt <- exp(1.2*tt$tt_h)  # Beta 1.2
tt$exp_1.4_tt <- exp(-1.4*tt$tt_h)  # Beta 1.4
tt$exp_2.5_tt <- exp(2.5*tt$tt_h)  # Beta 2.5
tt$exp_5_tt <- exp(-5*tt$tt_h)  # Beta 5
tt$exp_10_tt <- exp(-10*tt$tt_h)  # Beta 10

# column rename
colnames(tt)[colnames(tt)=="FROM"] <- "OBJECTID"

population <- read_csv("E:/skims/shp/new_zones_with_pop/zones_with_popl.csv")
pop_per_zone <- population %>%
  dplyr::group_by(taz) %>%
  dplyr::summarize(pop = sum(population))
colnames(pop_per_zone)[colnames(pop_per_zone)=="zone_id"] <- "OBJECTID"
# column rename
sum <- merge(tt, pop_per_zone, by = "OBJECTID")
# 1000 000 can be removed if needed
sum$exp_pop_04 <- sum$exp_04_tt * sum$pop/1000000
sum$exp_pop_06 <- sum$exp_06_tt * sum$pop/1000000
sum$exp_pop_08 <- sum$exp_08_tt * sum$pop/1000000
sum$exp_pop_1 <- sum$exp_1_tt * sum$pop/1000000
sum$exp_pop_1.2 <- sum$exp_1.2_tt * sum$pop/1000000
sum$exp_pop_1.4 <- sum$exp_1.4_tt * sum$pop/1000000
sum$exp_pop_2.5 <- sum$exp_2.5_tt * sum$pop/1000000
sum$exp_pop_5 <- sum$exp_5_tt * sum$pop/1000000
sum$exp_pop_10 <- sum$exp_10_tt * sum$pop/1000000

sum1 <- sum %>%
  dplyr::group_by(TO) %>%
  dplyr::summarize(count = n(), access_06 = sum(exp_pop_06), access_08 = sum(exp_pop_08),
                   access_1 = sum(exp_pop_1), access_1.2 = sum(exp_pop_1.2), access_1.4 = sum(exp_pop_1.4),
                   access_2.5 = sum(exp_pop_2.5), access_5 = sum(exp_pop_5), access_10 = sum(exp_pop_10))

colnames(sum1)[colnames(sum1)=="TO"] <- "OBJECTID"
sum2 <- merge(sum1, pop_per_zone, by = "OBJECTID")

setwd("E:/skims/accessibility")
fwrite(sum2,"car_access_tt_b.csv")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 




# # # # # # # # # # # # # # # Cumulative Accessibility # # # # # # # # # # # # # # # # # # # # # # 

car_tt <- read_delim("E:/skims/skims/tertiary_car_matrix_12.03/car_traveltimes.csv", 
                     ";", escape_double = FALSE, trim_ws = TRUE)

infin <- car_tt[grepl("Inf", car_tt$VALUE),]
tt <- car_tt[!grepl("Inf", car_tt$VALUE),]

tt$tt_min <- tt$VALUE/60 # convert to hours
colnames(tt)[colnames(tt)=="FROM"] <- "OBJECTID"

population <- read_csv("E:/skims/shp/new_zones_with_pop/zones_with_pop_final.csv")
pop_per_zone <- population %>%
  dplyr::group_by(zone_id) %>%
  dplyr::summarize(pop = sum(population))
colnames(pop_per_zone)[colnames(pop_per_zone)=="zone_id"] <- "OBJECTID"
# column rename
sum <- merge(tt, pop_per_zone, by = "OBJECTID")
# 1000 000 only to make number smaller. can be erased if needed
sum$access_30 <- ifelse(sum$tt_min <= 30, sum$pop/1000000, 0)
sum$access_60 <- ifelse(sum$tt_min <= 60, sum$pop/1000000, 0)
sum$access_90 <- ifelse(sum$tt_min <= 90, sum$pop/1000000, 0)


cumul_access <- sum %>%
  dplyr::group_by(TO) %>%
  dplyr::summarize(count = n(), access_30 = sum(access_30), 
                   access_60 = sum(access_60), 
                   access_90 = sum(access_90))


setwd("E:/skims/accessibility")
fwrite(cumul_access,"cumul_access.csv")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
