library("tidyverse")
library("dplyr")

dfdmv <- read.csv("dfdmv_with_treatment.csv")
silver_2014_2022 <- read.csv("station_zips_2014_2022.csv")
silver_2014 <- silver_2014_2022 %>% filter (open== 2014)
silver_2022 <- silver_2014_2022 %>% filter (open== 2022)

dfdmv <- dfdmv %>%
  mutate(zip_in_2014 = ifelse(RegionName %in% silver_2014$ZIPCODE & Year >= 2014, 1, 0))

dfdmv <- dfdmv %>%
  mutate(zip_in_2022 = ifelse(RegionName %in% silver_2022$ZIPCODE & Year >= 2022, 1, 0))
write.csv(dfdmv, "dfdmv_with_treatment_NEW.csv")
