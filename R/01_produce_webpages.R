
# README ------------------------------------------------------------------

# script to render Rmd files and update all web pages


# PROJECT README ----------------------------------------------------------

rmarkdown::render(here::here('Rmd/README.Rmd'), 
                  output_file = here::here('README.md'))


# LANDING PAGE ------------------------------------------------------------

rmarkdown::render(here::here('Rmd/index.Rmd'), 
                  output_file = here::here('docs/index.html'))


# HOW ARE THE BIRDS DOING -------------------------------------------------

source('R/graph_annual_capturestats_total.R')

source('R/graph_annual_capturestats_focalspp.R')

rmarkdown::render(here::here('Rmd/bandingstats.Rmd'), 
                  output_file = here::here('docs/banding.html'))


# WEATHER & CLIMATE -------------------------------------------------------

source('R/graph_annual_weatherstats.R')

rmarkdown::render(here::here('Rmd/climate.Rmd'), 
                  output_file = here::here('docs/climate.html'))


# HABITAT & COMMUNITY CHANGE ----------------------------------------------

source('R/graph_annual_vegheight.R')

source('R/graph_annual_territories.R')

rmarkdown::render(here::here('Rmd/habitat.Rmd'), 
                  output_file = here::here('docs/habitat.html'))


# CROSSROADS & CONNECTIONS ------------------------------------------------

source('R/graph_seasonal_capturestats.R')

source('R/map_migration_data.R')

rmarkdown::render(here::here('Rmd/connections.Rmd'), 
                  output_file = here::here('docs/connections.html'))


