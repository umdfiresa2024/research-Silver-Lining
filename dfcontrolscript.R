library("tidyverse")
df_control <- read.csv("~/Library/CloudStorage/GoogleDrive-rcoronel@terpmail.umd.edu/Shared drives/2024 FIRE-SA/DATA/Silver Lining/Zhvi_data/Zhvi_DMV2.csv")


df_control2 <- df_control %>% 
  filter( df_control$CountyName == "Loudoun County" | df_control$CountyName == "Fairfax County")

df <- read.csv("~/Documents/GitHub/research-Silver-Lining/dfdmv_with_treatment_NEW.csv")
df2 <- df %>% filter( df$CountyName == "Loudoun County" | df$CountyName == "Fairfax County")

df_control3 <- df_control2 %>%
  filter(! df_control2$RegionName %in% df2$RegionName)

df_control3$date <- as.Date(paste(df_control3$Year,df_control3$Month,"01",sep="-"),"%Y-%m-%d")

df_control4 <- df_control3 %>% drop_na()

write.csv(df_control4,"~/Downloads/dfsilvercontrol.csv",row.names=FALSE)
