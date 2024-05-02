# install tidyverse package
install.packages("tidyverse")
library("tidyverse")

# read in desired file (2 bedroom unit rents)
df2 <- read.csv("Zip_zhvi_bdrmcnt_2_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")

# keep only zip codes in MD, VA, & DC and eliminate unneeded variables
df3 <- df2 %>%
  filter(State == "MD" | State == "VA" | State == "DC") %>%
  mutate(City = ifelse(City == "", Metro, City)) %>%
  select(-RegionID, -SizeRank, -RegionType, -StateName, -Metro, -CountyName)

# convert df from wide format to long format
df4 <- df3 %>%
  pivot_longer(
    cols = 4:294,
    names_to = "date",
    values_to = "ZHVI"
  )

# add in separate month and year columns and filter out any values where ZHVI is null
df5 <- df4 %>%
  mutate(year = substr(date, 2, 5),
         month = substr(date, 7, 8)) %>%
  select(-date) %>%
  filter(!is.na(ZHVI))

# vector of all zipcodes where Silver Line stations are located
zip <- c(20721,20774,20743,20019,20003,20024,20005,20006,20037,22209,22201,
         22203,22205,22102,22182,20190,20191,20170,20171,20166,20147)

# filter out any zip codes without a Silver Line station
df6 <- df5 %>% 
  filter(RegionName %in% zip)

# vectors for each of the zip codes based on what year their Silver Line station opened
open_2004 <- c(20741,20743,20744)
open_2014 <- c(20190,22182,22102)
open_2022 <- c(20147,20166,20171,20170,20191)
open_prior <- setdiff(zip, c(open_2004, open_2014, open_2022))

# create dfs for each opening year
df2004 <- df6 %>%
  mutate(open = ifelse(RegionName %in% open_2004,1,0)) %>%
  filter(open == 1)
df2014 <- df6 %>%
  mutate(open = ifelse(RegionName %in% open_2014,1,0)) %>%
  filter(open == 1)
df2022 <- df6 %>%
  mutate(open = ifelse(RegionName %in% open_2022,1,0)) %>%
  filter(open == 1)
dfprior <- df6 %>%
  mutate(open = ifelse(RegionName %in% open_prior,1,0)) %>%
  filter(open == 1)

# create dfs with the mean ZHVI value for each month for plotting
df2004_mean <- df2004 %>%
  group_by(year, month) %>%
  summarize(mean = mean(ZHVI))
df2014_mean <- df2014 %>%
  group_by(year, month) %>%
  summarize(mean = mean(ZHVI))
df2022_mean <- df2022 %>%
  group_by(year, month) %>%
  summarize(mean = mean(ZHVI))
dfprior_mean <- dfprior %>%
  group_by(year, month) %>%
  summarize(mean = mean(ZHVI))
dfall_mean <- df6 %>%
  group_by(year, month) %>%
  summarize(mean = mean(ZHVI))

# Plot each separate opening year's zipcodes and the mean of all zipcodes
ggplot() +
  geom_line(data = df2004_mean, aes(x = as.Date(paste(year, month, "01", sep = "-")), 
                                    y = mean, color = "2004")) +
  geom_line(data = df2014_mean, aes(x = as.Date(paste(year, month, "01", sep = "-")), 
                                    y = mean, color = "2014")) +
  geom_line(data = df2022_mean, aes(x = as.Date(paste(year, month, "01", sep = "-")), 
                                    y = mean, color = "2022")) +
  geom_line(data = dfprior_mean, aes(x = as.Date(paste(year, month, "01", sep = "-")), 
                                     y = mean, color = "Pre-2000")) +
  geom_line(data = dfall_mean, aes(x = as.Date(paste(year, month, "01", sep = "-")), 
                                     y = mean, color = "All")) +
  labs(x = "Year", y = "Mean ZHVI Value", color = "Year Opened") +
  ggtitle("Mean ZHVI Value by Month for Each Year (2 Bedroom Units)")
