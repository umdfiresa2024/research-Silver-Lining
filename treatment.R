library("tidyverse")
library("dplyr")
library("lubridate")

# read in ZHVI values of DMV area as dfdmv
# remove unnecessary columns and times when price is NA
dfdmv <- read.csv("~/Library/CloudStorage/GoogleDrive-rcoronel@terpmail.umd.edu/Shared drives/2024 FIRE-SA/DATA/Silver Lining/Zhvi_DMV2.csv")
dfdmv <- dfdmv %>% select(-c("X.1","Metro","X"))
dfdmv <- dfdmv %>% filter(!is.na(Price))

# manually created vector with Silver Line zip codes and their station opening date
# if no station is in the zip code, then opening date is set as 2030-01-01 to be in the future
stations_opening_dates <- data.frame(
  RegionName = c(20743, 20774, 20785, 20002, 20003,
              20004, 20005, 20006, 20007, 20019, 
              20024, 20250, 20260, 20052, 20037, 
              20410, 20585, 20148, 20166, 20147, 
              20041, 20170, 20171, 20190, 22046, 
              22203, 22205, 22201, 22043, 22102, 
              22182, 22213, 22209),
  opening_date = as.Date(c("1980-11-22","2004-12-18","2030-01-01","2030-01-01","1977-07-01",
                         "1977-07-01","1976-03-29","1977-07-01","2030-01-01","1980-11-22",
                         "1977-07-01","2030-01-01","2030-01-01","2030-01-01","1977-07-01",
                         "2030-01-01","2030-01-01","2030-01-01","2022-11-15","2022-11-15",
                         "2030-01-01","2022-11-15","2022-11-15","2014-07-26","2030-01-01",
                         "1979-12-11","1986-06-07","1979-12-11","2030-01-01","2014-07-26",
                         "2014-07-26","2030-01-01","1977-07-01")))

# using lubridate, make a date column
dfdmv <- dfdmv %>%
  mutate(date = make_date(Year, Month, 1))

# merge the zip codes and the opening dates together by "RegionName"
dfdmv_merge <- merge(dfdmv,stations_opening_dates,by="RegionName")

# make a station_open column
# if a station has opened in the zip code yet, then 1, else 0
dfdmv_merge <- dfdmv_merge %>%
  mutate(station_open = ifelse(date >= opening_date, 1, 0))

# remove the opening_date column
dfdmv_merge <- dfdmv_merge %>% select(-opening_date)

# sort the df by RegionName then BedroomCnt then date
dfdmv_merge <- dfdmv_merge %>% 
  arrange(RegionName,BedroomCnt,date)

# Write the dataframe dfdmv_merge to a CSV file named "dfdmv_merge.csv"
write.csv(dfdmv_merge, "~/Documents/GitHub/research-Silver-Lining/dfdmv_with_treatment.csv", row.names = FALSE)
