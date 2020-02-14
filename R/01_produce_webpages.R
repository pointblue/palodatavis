
# README ------------------------------------------------------------------

# script to render Rmd files and update all web pages


# PROJECT README ----------------------------------------------------------

rmarkdown::render(here::here('Rmd/README.Rmd'), 
                  output_file = here::here('README.md'))


# LANDING PAGE ------------------------------------------------------------

rmarkdown::render(here::here('Rmd/index.Rmd'), 
                  output_file = here::here('docs/index.html'))


# HOW ARE THE BIRDS DOING -------------------------------------------------



# WEATHER & CLIMATE -------------------------------------------------------



# HABITAT & COMMUNITY CHANGE ----------------------------------------------



# CROSSROADS & CONNECTIONS ------------------------------------------------



