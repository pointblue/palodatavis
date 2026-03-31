
# READ ME -----------------------------------------------------------------

# graph seasonal capture stats for several species to illustrate seasonal
# change in bird community

#PACKAGES
library(tidyverse)
library(foreign)
library(plotly)

# INPUT DATA 
# from script 00_process_band_nethrs
effort = read_csv('output/nethrs_effort.csv') # total by year and month
band = read_csv('output/band_captures.csv') # total by species, year, and month


# PALETTE
pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666')

#OUTPUT
out <- "docs/widget/graph_seasonal_capturestats.html"

# CALCULATE STATS ---------------------------------------------------------

# total net hours per month:
effort <- foreign::read.dbf(nethrspath) |> select(PROJECT:DUPE) |> 
  filter(LOCATION == 'PN' & DATE >= '2008-01-01') |> # subset to more recent data to simplify
  mutate(year = format(DATE, '%Y'),
         month = format(DATE, '%m'),
         monthlab = format(DATE, '%b')) |> 
  group_by(year, month, monthlab) |> 
  summarize(nethours = sum(NETHOURS),
            .groups = 'drop')


# filter and fill zeroes
band_subset = band |> filter(year >= 2008) |> 
  filter(SPEC %in% c('WIWA', 'SWTH', 'HETH', 'GCSP', 'WREN')) |> 
  mutate(SPEC = as.factor(as.character(SPEC)),
         year = as.factor(year)) |> 
  complete(SPEC, nesting(year, month), fill = list(n = 0)) |> 
  mutate(year = as.numeric(as.character(year)))

# calculate capture rate per 1000 net hours
dat <- left_join(band_subset, effort, by = c('month', 'year')) |> 
  mutate(rate = n / nethours * 1000,
         monthlab = month.abb[as.numeric(month)],
         month = as.numeric(month) + 0.5,
         SPEC = factor(SPEC, levels = c('WREN', 'SWTH', 'WIWA', 'HETH', 'GCSP')),
         speclab = recode(SPEC, 
                          GCSP = 'Golden-crowned Sparrow',
                          WIWA = "Wilson's Warbler",
                          SWTH = "Swainson's Thrush",
                          HETH = "Hermit Thrush",
                          WREN = 'Wrentit')) |> 
  select(spec = SPEC, speclab, monthlab, month, year, rate)

# average capture rate per month over multiple years:
datavg <- dat |> 
  group_by(spec, speclab, monthlab, month)|> 
  summarize(rate = mean(rate),
            .groups = 'drop') |> 
  mutate(ratelab = format(round(rate, digits = 1), nsmall = 1)) |> 
  ungroup()

# FIT SMOOTHED LINES ------------------------------------------------------

g1 <- ggplot(dat, aes(month, rate, color = speclab)) +
  geom_smooth(method = "gam", method.args = list(family = "quasipoisson"), 
              formula = y ~ s(x, k = 7), se = FALSE, n = 111)

# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot
dat2 <- data.frame(x = gg1$data[[1]]$x,
                   smooth = gg1$data[[1]]$y,
                   speclab = rep(levels(dat$speclab), each = 111)) %>% 
  mutate(speclab = factor(speclab, levels = levels(dat$speclab))) %>% 
  left_join(datavg %>% select(speclab, x = month, monthlab, 
                              avgrate = rate, avgratelab = ratelab),
            by = c('x', 'speclab'))


# PLOTLY ------------------------------------------------------------------

# build interactive plot
pal <- setNames(pointblue.palette[1:5], 
                c("Wrentit", "Swainson's Thrush", "Wilson's Warbler",
                  "Hermit Thrush", "Golden-crowned Sparrow"))

graph1 <- plot_ly(dat2) %>%
  add_lines(x = ~x,
            y = ~smooth,
            color = ~speclab,
            legendgroup = ~speclab,
            colors = pal,
            line = list(width = 3),
            hoverinfo = 'none') %>%
  # add_markers(x = ~x, 
  #             y = ~avgrate,
  #             color = ~speclab,
  #             legendgroup = ~speclab,
  #             hoverinfo = 'text',
  #             text = ~paste0('</br>', speclab,
  #                            '</br>', monthlab, ' avg rate: ', avgratelab),
  #             showlegend = FALSE) %>% 
  layout(yaxis = list(title = 'Capture rate (per 1000 net hours)',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      range = c(0,20),
                      showgrid = FALSE,
                      automargin = TRUE,
                      hoverformat = '.2f'),
         xaxis = list(title = NA,
                      showline = TRUE,
                      ticks = 'outside',
                      tickmode = 'array',
                      tickvals = seq(1.5, 12.5, 1),
                      ticktext = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                   'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                      showgrid = FALSE),
         legend = list(x = 1, xanchor = 'right', y = 1, yanchor = 'top'),
         margin = list(r = 0, b = 10, t = 10)) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d',
                                       'zoomIn2d', 'zoomOut2d',
                                       'pan2d', 'toggleSpikelines'))
graph1

graph1$dependencies <- c(graph1$dependencies,
                         list(
                           htmltools::htmlDependency(
                             name = 'plotly_style_nomargin',
                             version = '1.0.0',
                             src = 'docs/widget/lib',
                             stylesheet = 'plotly_style.css'
                           )
                         ))

htmlwidgets::saveWidget(graph1,
                        here::here(out),
                        selfcontained = FALSE,
                        libdir = 'lib',
                        title = 'Seasonal capture stats')
