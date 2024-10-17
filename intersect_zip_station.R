library("terra")
library("tidyverse")

#https://opendata.dc.gov/datasets/DCGIS::metro-stations-regional/explore?location=38.909217%2C-77.374507%2C9.70
metro<-vect("Metro_Stations_Regional/Metro_Stations_Regional.shp")
line<-vect("Metro_Lines_Regional/Metro_Lines_Regional.shp")

metrodf<-as.data.frame(metro)

st1<-c("Wiehle-Reston East", "Spring Hill", "Greensboro", "Tysons", "McLean")
st2<-c("Ashburn", "Loudoun Gateway", "Dulles International Airport", 
       "Innovation Center", "Herndon", "Reston Town Center")

#select metro that opens in 2014
silver_st1<-subset(metro, metro$NAME %in% st1)

#create area to crop silver line shapefile
silver_st1_buff<-buffer(as.lines(silver_st1), width=1000)

#create a buffer around this specific area of the silver line
silver_2014<-buffer(crop(line, silver_st1_buff), width=1000)

plot(line)
plot(silver_2014, col="blue", add=TRUE)
plot(silver_st1, col="red", add=TRUE)

#find zip code that intersects with this portion of the silver line
va<-vect("VA/VA_Zip_Codes.shp")
va_project<-project(va, crs(metro))
va_2014<-terra::intersect(va_project, silver_2014)
va_2014_df<-as.data.frame(va_2014) |>
  select(ZIP_CODE) |>
  rename(ZIPCODE=ZIP_CODE) |>
  mutate(open=2014)

#check if it intersects with only 6 zip codes
plot(silver_2014, col="blue", alpha=0.5)
plot(va_project, add=TRUE)

############execute the same task for the 2022 expansion of the silver line######
#select metro that opens in 2022
silver_st2<-subset(metro, metro$NAME %in% st2)

#create area to crop silver line shapefile
silver_st2_buff<-buffer(as.lines(silver_st2), width=1000)

#create a buffer around this specific area of the silver line
silver_2022<-buffer(crop(line, silver_st2_buff), width=2000)

va_2022<-terra::intersect(va_project, silver_2022)
va_2022_df<-as.data.frame(va_2022) |>
  select(ZIP_CODE) |>
  rename(ZIPCODE=ZIP_CODE) |>
  mutate(open=2022)

plot(line)
plot(silver_2022, col="blue", add=TRUE)
plot(silver_st2, col="red", add=TRUE)

#find zip code that intersects with this portion of the silver line############



#combine everything together
zips<-rbind(va_2014_df, va_2022_df)
