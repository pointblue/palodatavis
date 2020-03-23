
# READ ME -----------------------------------------------------------------
# create map showing migratory connectivity results

#PACKAGES
library(tidyverse)
library(sf)
library(leaflet)

#INPUT DATA
coordpath <- "rawdat/Summary_MarinBirds_MeanLocsWhereTheyWent.xlsx"
iconpath <- "images/icons/logo.png"

swthrange <- 'rawdat/cath_ustu_pl.shp'

# PALETTE
pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666')

#OUTPUT
mappath <- "docs/migration_map.html"


# SET UP ------------------------------------------------------------------

shp <- readxl::read_excel(here::here(coordpath)) %>% 
  st_as_sf(coords = c("Mean.Lon", "Mean.Lat"), 
           crs = '+proj=longlat +datum=WGS84 +no_defs') %>% 
  mutate(specieslab = recode(Species, 'HETH' = "Hermit Thrush",
                             'GCSP' = "Golden-crowned Sparrow",
                             'FOSP' = "Fox Sparrow",
                             'SWTH' = "Swainson's Thrush"),
         TagType = recode(TagType, 'Light' = 'Geolocator'))

pal1 <- colorFactor(palette = pointblue.palette[c(3,2,1,4)], 
                    domain = unique(shp$specieslab))

pal2 <- colorFactor(palette = pointblue.palette[c(3,2)], 
                    domain = unique(swthpoly$season))

logoicon <- makeIcon(iconUrl = here::here(iconpath), 
                     iconWidth = 25, iconHeight = 25)

swthpoly <- st_read(here::here(swthrange)) %>%
  st_set_crs('+proj=longlat +datum=WGS84') %>%
  filter(ORIGIN != 4) %>% #drop migration range
  group_by(ORIGIN) %>%
  summarize() %>%
  mutate(season = case_when(ORIGIN == 2 ~ 'breeding',
                            ORIGIN == 3 ~ 'wintering'))

# CREATE MAP --------------------------------------------------------------

map1 <- leaflet(shp) %>% 
  setView(lng = -122.735527, lat = 42, zoom = 3) %>% 
  
  # background terrain
  addProviderTiles("Esri.WorldGrayCanvas") %>% 
  
  # range map
  addPolygons(data = swthpoly,
              fillColor = ~ pal2(season),
              stroke = FALSE,
              group = "Swainson's Thrush") %>% 
  
  # add icon for Palo
  addMarkers(lng = -122.735527, lat = 37.929851, 
             icon = logoicon,
             label = "Palomarin Field Station",
             popup = "Palomarin Field Station") %>% 
  
  # add migration data
  addCircleMarkers(radius = 5,
                   stroke = FALSE,
                   fillColor =  ~ pal1(specieslab),
                   fillOpacity = 0.7,
                   label = ~TagType,
                   popup = ~TagType) %>% 
  
  addLayersControl(overlayGroups = c('Fox Sparrow', 'Golden-crowned Sparrow',
                                     'Hermit Thrush', "Swainson's Thrush"),
                   position = "bottomleft",
                   options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup('Golden-crowned Sparrow') %>%
  hideGroup('Hermit Thrush') %>%
  hideGroup('Fox Sparrow') %>%
  hideGroup("Swainson's Thrush") %>% 
  
  addLegend(pal = pal1,
            values = shp$specieslab,
            title = 'Species',
            position = 'topright',
            opacity = 1)

# # add CSS
# map1$dependencies <- c(map1$dependencies,
#                        list(
#                          htmltools::htmlDependency(
#                            name = 'leaflet_style',
#                            version = '1.0.0',
#                            src = here::here('Rmd'),
#                            stylesheet = 'leaflet_style.css'
#                          )
#                        ))

title <- paste0('Palomarin Migratory Connectivity')

htmlwidgets::saveWidget(map1,
                        here::here(mappath),
                        selfcontained = TRUE,
                        title = title)
