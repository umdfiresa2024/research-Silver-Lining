library("terra")
library("tidyverse")
library("sf")

#zipcodes <- vect("~/Downloads/zipcodes/tl_2020_us_zcta520.shp")
lines <- vect("~/Downloads/Metro_Lines_Regional/Metro_Lines_Regional.shp")
silver_line <- lines[lines$NAME == "silver"]
# Filter for specific states (assuming there's a 'state' column)
plot(silver_line, main = "DC Metro Silver Line")
md_zip <- vect("~/Downloads/Maryland_Political_Boundaries_-_ZIP_Codes_-_5_Digit (1)/Maryland_Political_Boundaries_-_ZIP_Codes_-_5_Digit.shp")
dc_zip <- vect("~/Downloads/DC/Zip_Codes.shp")
va_zip <- vect("~/Downloads/VA_Zip_Codes_8831355910966913844/VA_Zip_Codes.shp")
plot(md_zip)
plot(dc_zip)
plot(va_zip)
# Plot the filtered zip code

silver_md_projection <- project(silver_line, crs(md_zip))
silver_md_intersection <- terra::intersect(md_zip,silver_md_projection) 
silvermd_df <- as.data.frame(silver_md_intersection)

silver_va_projection <- project(silver_line, crs(va_zip))
silver_va_intersection <- terra::intersect(va_zip,silver_va_projection) 
silverva_df <- as.data.frame(silver_va_intersection)

silver_dc_projection <- project(silver_line, crs(dc_zip))
silver_dc_intersection <- terra::intersect(dc_zip,silver_dc_projection) 
silverdc_df <- as.data.frame(silver_dc_intersection)
# Plot the spatial object directly
plot(silver_md_projection)
plot(silver_md_projection, add = TRUE, col = "green")
plot(silver_dc_projection, add = TRUE, col = "red")
plot(silver_va_projection, add = TRUE, col = "gold")
plot(silver_md_projection, add=TRUE,col = "blue")


plot(silver_dc_projection)
plot(md_zip, add = TRUE)
plot(dc_zip, add = TRUE)
plot(va_zip, add = TRUE)
plot(silver_dc_projection, add = TRUE, col = "gold")

# delete rows not intersecting
md_zipcodes_vector <- silvermd_df$ZIPCODE1
dc_zipcodes_vector <- silverdc_df$ZIPCODE
dc_zipcodes_vector2 <- as.character(dc_zipcodes_vector)
va_zipcodes_vector <- silverva_df$ZIP_CODE
dmv_zipcodes_vector <- c(md_zipcodes_vector,dc_zipcodes_vector2,va_zipcodes_vector)

us_zipcodes_df <- as.data.frame(zipcodes)
us_zipcodes_df2 <- us_zipcodes_df[us_zipcodes_df$ZCTA5CE20 %in% dmv_zipcodes_vector, ]
us_zipcodes_df2$INTPTLAT20 <- as.numeric(us_zipcodes_df2$INTPTLAT20)
us_zipcodes_df2$INTPTLON20 <- as.numeric(us_zipcodes_df2$INTPTLON20)

# Convert to SpatVector using latitude and longitude
spatial_vect <- vect(us_zipcodes_df2, 
                     geom = c("INTPTLON20", "INTPTLAT20"), 
                     crs = "EPSG:4326")  # Adjust CRS if needed

writeVector(spatial_vect, "dmv_zipcodes.shp")
