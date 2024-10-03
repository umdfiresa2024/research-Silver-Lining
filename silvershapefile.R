library("terra")
library("tidyverse")
library("sf")

# load in all US zip codes (takes a while)
zipcodes <- vect("~/Library/CloudStorage/GoogleDrive-rcoronel@terpmail.umd.edu/Shared drives/2024 FIRE-SA/DATA/Silver Lining/zipcodes/tl_2020_us_zcta520.shp")

# load in all metro lines and keep the silver line
lines <- vect("~/Downloads/Metro_Lines_Regional/Metro_Lines_Regional.shp")
silver_line <- lines[lines$NAME == "silver"]

# load in MD, DC, and VA zip codes separately
md_zip <- vect("~/Library/CloudStorage/GoogleDrive-rcoronel@terpmail.umd.edu/Shared drives/2024 FIRE-SA/DATA/Silver Lining/Maryland_Political_Boundaries_-_ZIP_Codes_-_5_Digit (1)/Maryland_Political_Boundaries_-_ZIP_Codes_-_5_Digit.shp")
dc_zip <- vect("~/Library/CloudStorage/GoogleDrive-rcoronel@terpmail.umd.edu/Shared drives/2024 FIRE-SA/DATA/Silver Lining/DC/Zip_Codes.shp")
va_zip <- vect("~/Library/CloudStorage/GoogleDrive-rcoronel@terpmail.umd.edu/Shared drives/2024 FIRE-SA/DATA/Silver Lining/VA/VA_Zip_Codes.shp")

# intersect and project state zip codes with Silver Line 
# to keep just Silver Line-intersecting zip codes
# then turn into a zip code df
silver_md_projection <- project(silver_line, crs(md_zip))
silver_md_intersection <- terra::intersect(md_zip,silver_md_projection) 
silvermd_df <- as.data.frame(silver_md_intersection)

silver_va_projection <- project(silver_line, crs(va_zip))
silver_va_intersection <- terra::intersect(va_zip,silver_va_projection) 
silverva_df <- as.data.frame(silver_va_intersection)

silver_dc_projection <- project(silver_line, crs(dc_zip))
silver_dc_intersection <- terra::intersect(dc_zip,silver_dc_projection) 
silverdc_df <- as.data.frame(silver_dc_intersection)

# plot the silver line and the zip codes if needed
# plot(silver_dc_projection)      # plot this first to get just the view of the Silver Line
# plot(md_zip, add = TRUE)
# plot(dc_zip, add = TRUE)
# plot(va_zip, add = TRUE)
# plot(silver_dc_projection, add = TRUE, col = "gold")

# eliminate all other columns and rows such that just the intersecting zip codes
# are kept, converting them all into strings if needed
md_zipcodes_vector <- silvermd_df$ZIPCODE1
dc_zipcodes_vector <- silverdc_df$ZIPCODE
dc_zipcodes_vector2 <- as.character(dc_zipcodes_vector)
va_zipcodes_vector <- silverva_df$ZIP_CODE
dmv_zipcodes_vector <- c(md_zipcodes_vector,dc_zipcodes_vector2,va_zipcodes_vector)

# take the whole US zip code shapefile and keep only zip codes that the Silver Line
# intersects
us_zipcodes_df <- as.data.frame(zipcodes)
us_zipcodes_df2 <- us_zipcodes_df[us_zipcodes_df$ZCTA5CE20 %in% dmv_zipcodes_vector, ]
us_zipcodes_df2$INTPTLAT20 <- as.numeric(us_zipcodes_df2$INTPTLAT20)
us_zipcodes_df2$INTPTLON20 <- as.numeric(us_zipcodes_df2$INTPTLON20)

# Convert to SpatVector using latitude and longitude
spatial_vect <- vect(us_zipcodes_df2, 
                     geom = c("INTPTLON20", "INTPTLAT20"), 
                     crs = "EPSG:4326")  # Adjust CRS if needed

# write and save file
writeVector(spatial_vect, "dmv_zipcodes.shp")
