# READ ME -----------------------------------------------------------------

# graph annual territory numbers for key species

#PACKAGES
library(tidyverse)
library(foreign)
library(plotly)

# INPUT DATA
# local copy of territory counts:
terrpath <- "rawdat/palo_breeding_territories.csv"

# PALETTE
pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666')

#OUTPUT
out <- "docs/widget/graph_annual_territories.html"


# PREP DATA ----------------------------------------------------------

terrdat <- read_csv(terrpath, col_types = cols())  |>  
  select(year = YEAR, wiwa, sosp, wren, nwcs) |> 
  pivot_longer(-year, values_to = 'territories', names_to = 'species') |> 
  mutate(species = factor(species,
                          levels = c('wren', 'nwcs', 'sosp', 'wiwa')),
         species2 = recode(species,
                           nwcs = "Nuttall's White-crowned Sparrow",
                           sosp = "Song Sparrow",
                           wiwa = "Wilson's Warbler",
                           wren = "Wrentit")) |> 
  mutate_at(vars(species, species2), as.factor)

maxyear <- terrdat |> select(year) |> max()


# FIT SMOOTHED LINES ------------------------------------------------------

g1 <- ggplot(terrdat, aes(year, territories, color = species)) +
  geom_smooth(method = "gam", method.args = list(family = "quasipoisson"), 
              formula = y ~ s(x), se = FALSE)

# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot
dat2 <- tibble(x = gg1$data[[1]]$x,
                   smooth = gg1$data[[1]]$y,
                   species = rep(levels(terrdat$species), each = 80)) |> 
  left_join(terrdat |> select(species, species2) |> distinct(), by = c('species'))




# PLOTLY ------------------------------------------------------------------

pal <- setNames(pointblue.palette[1:4], 
                c("Nuttall's White-crowned Sparrow", "Song Sparrow", 
                  "Wilson's Warbler", "Wrentit"))


graph1 <- plot_ly() |> 
  add_lines(data = dat2,
            x = ~x,
            y = ~smooth,
            line = list(width = 3),
            hoverinfo = 'none',
            legendgroup = ~species2,
            color = ~species2,
            colors = pal) |> 
  add_markers(data = terrdat,
              x = ~year,
              y = ~territories,
              marker = list(size = 8),
              text = ~territories,
              hoverinfo = 'x+text',
              text = ~territories,
              legendgroup = ~species2,
              showlegend = F,
              color = ~species2,
              colors = pal) |> 
  layout(yaxis = list(title = 'Territories',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      range = c(0, 90),
                      showgrid = FALSE,
                      automargin = TRUE),
         xaxis = list(title = NA,
                      showline = TRUE,
                      ticks = 'outside',
                      range = c(1980,maxyear+2),
                      showgrid = FALSE),
         legend = list(x = 1, xanchor = 'right', y = 1, yanchor = 'top'),
         hovermode = 'x',
         margin = list(r = 0, b = 10, t = 10)) |>
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d',
                                       'zoomIn2d', 'zoomOut2d',
                                       'pan2d', 'toggleSpikelines'))

graph1$dependencies <- c(graph1$dependencies,
                         list(
                           htmltools::htmlDependency(
                             name = 'plotly_style_nomargin',
                             version = '1.0.0',
                             src = 'docs/widget/lib',
                             stylesheet = 'plotly_style.css'
                           )
                         ))

graph1

htmlwidgets::saveWidget(graph1,
                        here::here(out),
                        selfcontained = FALSE,
                        libdir = 'lib',
                        title = 'Annual territories')
