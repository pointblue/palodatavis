# READ ME -----------------------------------------------------------------

# graph annual territory numbers for key species

#PACKAGES
library(tidyverse)
library(foreign)
library(plotly)

#INPUT DATA
# original:
# terrpath <- 'Z:/Terrestrial/programs_and_projects/palomarin/Palodata/territories/palo_breeding_territories.csv'

# local copy:
terrpath <- "rawdat/palo_breeding_territories.csv"

# PALETTE
pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666')

#OUTPUT
out <- "docs/widget/graph_annual_territories.html"


# PREP DATA ----------------------------------------------------------

terrdat <- read_csv(terrpath, col_types = cols()) %>% 
  select(year = YEAR, wiwa, sosp, wren, nwcs) %>% 
  pivot_longer(-year, values_to = 'territories', names_to = 'species') %>% 
  mutate(species = factor(species,
                          levels = c('wren', 'sosp', 'nwcs', 'wiwa')),
         species2 = recode(species,
                           nwcs = "Nuttall's White-crowned Sparrow",
                           sosp = "Song Sparrow",
                           wiwa = "Wilson's Warbler",
                           wren = "Wrentit")) %>% 
  mutate_at(vars(species, species2), as.factor)

maxyear <- terrdat %>% select(year) %>% max()

# res <- terrdat %>% 
#   split(.$species) %>% 
#   map(~ loess(territories ~ year, data = .)) %>% 
#   map_df(predict) %>% 
#   mutate(year = unique(terrdat$year)) %>% 
#   gather(-year, key = 'species', value = 'smooth')
# 
# terrdat %>% left_join(res, by = c('species', 'year')) %>% 
#   plot_ly(x = ~year,
#           color = ~species2,
#           colors = pointblue.palette[1:4]) %>%

# FIT SMOOTHED LINES ------------------------------------------------------

g1 <- ggplot(terrdat, aes(year, territories, color = species)) +
  geom_smooth(method="gam", method.args = list(family="quasipoisson"), 
              formula = y ~ s(x), se = FALSE)

# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot
dat2 <- tibble(x = gg1$data[[1]]$x,
                   smooth = gg1$data[[1]]$y,
                   species = rep(levels(terrdat$species), each = 80)) %>% 
  left_join(terrdat %>% select(species, species2), by = c('species'))




# PLOTLY ------------------------------------------------------------------

pal <- setNames(pointblue.palette[1:4], 
                c("Nuttall's White-crowned Sparrow", "Song Sparrow", 
                  "Wilson's Warbler", "Wrentit"))


graph1 <- plot_ly() %>% 
  add_lines(data = dat2,
            x = ~x,
            y = ~smooth,
            line = list(width = 3),
            hoverinfo = 'none',
            legendgroup = ~species2,
            color = ~species2,
            colors = pal) %>% 
  add_markers(data = terrdat,
              x = ~year,
              y = ~territories,
              marker = list(size = 8),
              text = ~territories,
              hoverinfo = 'text',
              text = ~paste0('</br>', year, ' : ', territories),
              legendgroup = ~species2,
              showlegend = F,
              color = ~species2,
              colors = pal) %>% 
  layout(yaxis = list(title = 'Territories',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      range = c(0,100),
                      showgrid = FALSE,
                      automargin = TRUE),
         xaxis = list(title = NA,
                      showline = TRUE,
                      ticks = 'outside',
                      range = c(1980,maxyear+4),
                      showgrid = FALSE),
         legend = list(x = 0.01, xanchor = 'left', y = 1, yanchor = 'top', 
                       bgcolor = NA, orientation = 'h'),
         hovermode = 'x',
         margin = list(r = 0, b = 10, t = 10)) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d',
                                       'zoomIn2d', 'zoomOut2d',
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(graph1,
                        here::here(out),
                        selfcontained = TRUE,
                        title = 'Annual territories')
