install.packages("sf")
library("sf")
library("tidyverse")
library("dplyr")
library("terra")
library("sf")

# reading in stuff
dfdmv <- read.csv("dfdmv_with_treatment.csv")
metro_line<- vect("~/Downloads/Metro_Lines_Regional/metro_lines_regional.shp")
md_zip <- vect("~/Downloads/MD/Maryland_Political_Boundaries_-_ZIP_Codes_-_5_Digit.shp")
dc_zip <- vect("~/Downloads/DC/Zip_Codes.shp")
va_zip <- vect("~/Downloads/VA/VA_Zip_Codes.shp")

#misc misc
metro_line$NAME2 <- metro_line$NAME

dc_projection <- project(metro_line, crs(dc_zip))
dc_intersection <- terra::intersect(dc_projection,dc_zip) 

va_projection <- project(metro_line, crs(va_zip))
va_intersection <- terra::intersect(va_zip,va_projection) 

md_projection <- project(metro_line, crs(md_zip))
md_intersection <- terra::intersect(md_zip,md_projection) 
#silvermd_df <- as.data.frame(silver_md_intersection)
print(va_intersection)
#plot(dc_zip,col="yellow")

#CHECKPOINT
#MD
selected_spatvector <- md_intersection[, c("ZIPCODE1","ZIPNAME", "NAME")]
print(selected_spatvector)
md_test<-as.data.frame(selected_spatvector)

#DC
selected_spatvector <- dc_intersection[, c( "ZIPCODE", "NAME")]
print(selected_spatvector)
dc_test<-as.data.frame(selected_spatvector)
#plot(silver_md_intersection,add=TRUE)

#VA
selected_spatvector <- va_intersection[, c( "ZIP_CODE", "NAME","PO_NAME")]
print(selected_spatvector)
va_test<-as.data.frame(selected_spatvector)

#OK SO NOW WE HAVE 3 Dataframes: md_test, dc_test, va_test

#standardize dataframe col names
md_test<-md_test %>% rename(ZIPCODE=ZIPCODE1,LINE=NAME,NAME = ZIPNAME)
dc_test<-dc_test %>% rename(LINE=NAME)
va_test<-va_test %>% rename(LINE=NAME, ZIPCODE = ZIP_CODE, NAME = PO_NAME)
dc_test<-dc_test%>%mutate(NAME ="DC")
combined<-rbind(md_test,dc_test,va_test)
silver_line_zip <- combined%>% filter(LINE=="silver")
everything_else_zip <- combined %>% filter(LINE!="silver")

#find similarities
similarities <- inner_join(silver_line_zip,everything_else_zip , by="ZIPCODE")

#wrapping up
FINAL_ZIP_LIST <- similarities %>% summarize(ZIPCODE)
FINAL_ZIP_LIST<-unique(FINAL_ZIP_LIST) #get unique zips
write.csv(FINAL_ZIP_LIST, "overlapping_zipcodes.csv")


test <- combined %>% filter(LINE=="silver")

F2 = left_join(silver_line_zip,FINAL_ZIP_LIST, by="ZIPCODE")

write.csv(silver_line_zip, "silver_zips.csv")
