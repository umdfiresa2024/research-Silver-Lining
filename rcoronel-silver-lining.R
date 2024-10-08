install.packages("tidyverse")
library("tidyverse")
d <- dir()
print(d)
df2 <- read.csv("Zip_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
df3 <- df2 %>%
  filter(State == "MD" | State == "VA" | State == "DC") %>%
  mutate(City = ifelse(City == "", Metro, City)) %>%
  select(-RegionID, -SizeRank, -RegionType, -StateName, -Metro, -CountyName)
df4 <- df3 %>%
  pivot_longer(
    cols = 4:294,
    names_to = "date",
    values_to = "ZHVI"
  )
zip <- c(20721,20774,20743,20019,20003,20024,20005,20006,20037,22209,22201,
         22203,22205,22102,22182,20190,20191,20170,20171,20166,20147)
df5 <- df4 %>% 
  filter(RegionName %in% zip)
open_2004 <- c(20741,20743,20744)
open_2014 <- c(20190,22182,22102)
open_2022 <- c(20147,20166,20171,20170,20191)
open_prior <- setdiff(zip, c(open_2004, open_2014, open_2022))
df2004 <- df5 %>%
  mutate(open = ifelse(RegionName %in% open_2004,1,0)) %>%
  filter(open == 1)
df2014 <- df5 %>%
  mutate(open = ifelse(RegionName %in% open_2014,1,0)) %>%
  filter(open == 1)

