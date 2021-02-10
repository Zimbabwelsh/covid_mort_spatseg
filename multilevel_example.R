library(sf)
library(tmap)
library(dplyr)

sheffield <- st_read("Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain.shp")
sheffield %>% filter(lad17nm == "Sheffield" |lad17nm == "Doncaster"| lad17nm == "Barnsley" | lad17nm == "Rotherham")


sheffield <- sheffield[c(273:276),]

msoa <- st_read("Middle_Layer_Super_Output_Areas__December_2011__Boundaries.shp")
shef_msoa <- dplyr::filter(msoa, grepl('Sheffield|Rotherham|Barnsley|Doncaster', msoa11nm))

### generate deaths for each MSAO following poisson distribution
shef_msoa$deaths <- abs(rpois(172, 2))
# sort the dataframe to use the same data but with similar values in each LAD
x <- arrange(shef_msoa, deaths3)
shef_msoa$deaths2 <- x$deaths


### plot the two togetre
c <- tm_shape(shef_msoa) + tm_polygons(c("deaths3", "deaths4"), title=c("A", "B")) + tm_shape(sheffield) + tm_borders(lwd=3,  col = "Black")

tmap_save(c, "multilevel_example.png", height = 10)

