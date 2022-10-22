#Initiatlize the app
packages <- c("shiny",  "ggplot2",  "tidyverse",  "shinydashboard",  "leaflet",
              "magrittr", "lubridate", "reshape", "tidyverse", "DT",  "knitr", 
              "corrplot", "sf", "tmap", "rgdal", "htmlwidgets", "terra")

for (p in packages) {
  if (!require(p,  character.only = TRUE)) {
    install.packages(p)
  }
  library(p,  character.only = TRUE)
}

#Read the data
dengue <- read_csv("Data/dengue_final.csv")

# read the subzone data and shapefile

mpsz_area <- st_read("Data/data/MP14_SUBZONE_NO_SEA_PL.dbf")
mpsz_area <- st_transform(mpsz_area,  3414)
# plot(mpsz_area,  max.plot=1)

map2(dengue$Longitude,  dengue$Latitude,  ~st_point(c(.x,  .y))) %>%
    st_sfc(crs = 3414) %>%
    st_sf(dengue[, -(1:2)],  .) -> dengue_sf

bind_cols(
    dengue, 
    mpsz_area[as.numeric(st_within(dengue_sf,  mpsz_area)), ]
    ) %>%
    select(Longitude,  Latitude,  INC_CRC,  subzone_name=SUBZONE_N) %>% 
    mutate(subzone_name = str_to_title(subzone_name))
    
glimpse(dengue_sf)

#remove NA values from the data
dengue_sf <- na.omit(dengue_sf)