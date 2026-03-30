# READ ME -----------------------------------------------------------------

# graph annual capture stats for focal species

# PACKAGES
library(tidyverse)
library(foreign)
library(plotly)

# INPUT DATA
# path to local copies of net hours and banding data
nethrspath <- "rawdat/allpalonthrs.dbf"
bandpath <- "rawdat/allnumb.dbf"

# PALETTE
pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666')

# CALCULATE STATS ---------------------------------------------------------
# study spp:
spplist <- c('WREN', 'SOSP', 'NWCS', 'SPTO', 'CASJ', # residents
             'WIWA', 'SWTH', 'FOSP', 'GCSP', 'HETH') # migrants

effort = read_csv('output/nethrs_effort.csv') # total by year and month
band = read_csv('output/band_captures.csv') # total by species, year, and month

dat =  band |> 
  filter(SPEC %in% spplist) |> # filter to study spp
  # fill in zeroes
  mutate(SPEC = as.factor(SPEC),
         year = as.factor(year),
         month = as.factor(month)) |>  
  complete(SPEC, nesting(year, month), fill = list(n = 0)) |> 
  mutate(year = as.numeric(as.character(year))) |> 
  full_join(effort, by = c('year', 'month')) |> # join to net hours
  # assign months to specific seasons
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         season = case_when(month >= 3 & month <= 7 ~ 'spring', # 5 months
                            month >= 8 & month <= 10 ~ 'fall', # 3 months
                            TRUE ~ 'winter'), # 4 months
         # group Jan and Feb with previous Nov and Dec
         year = case_when(month <= 2 ~ year - 1,
                          TRUE ~ year))

# check for complete years:
dat |> group_by(SPEC, year) |> count() |> filter(n < 12)
# >> 1978 (because Jan/Feb 1979)
# >> 2025 (10 months because Jan/Feb not yet included)

dat_sum = dat |>
  # drop incomplete seasons
  filter(year > 1978) |> 
  filter(!(year == 2025 & season == 'winter')) |> 
  # summarize captures and net hours over full years (Mar-Feb)
  group_by(SPEC, year) |> 
  summarize(n = sum(n),
            nethours = sum(nethours),
            .groups = 'drop') |> 
  # calculate capture rate per 1000 net hours
  mutate(rate = n / nethours * 1000,
         year = as.numeric(year)) |> 
  select(spec = SPEC, year, rate)
  


# CREATE STATIC PLOTS-----------------------------------------------------

## residents--------
plot_residents = purrr::map(
  spplist[1:5] |> setNames(spplist[1:5]), 
  function(x) {
    dat_sum |> filter(spec == x) |> 
      ggplot(aes(year, rate)) + 
      geom_smooth(method = 'gam', method.args = list(family = 'quasipoisson'),
                  se = FALSE,
                  #formula = y ~ s(x, k = 15), se = FALSE,
                  linewidth = 1.5, color = pointblue.palette[2]) +
      geom_line(color = pointblue.palette[6]) + 
      geom_point(size = 2, color = pointblue.palette[4]) +
      labs(x = 'Annual capture rate (1979-2025)', 
           y = 'Capture rate (per 1000 net hours)') +
      scale_x_continuous(breaks = seq(1980, max(dat$year), 10)) +
      ylim(0, NA) +
      theme_classic() 
  })

patchwork::wrap_plots(plot_residents) # check they look ok

# write to file
purrr::map(names(plot_residents),
           function(x) {
             ggsave(plot = plot_residents[[x]],
                    filename = paste0('docs/fig/residents/', x, '.jpg'), 
                    width = 5, height = 3.5, units = 'in', dpi = 300)
           })


## migrants-------
plot_migrants = purrr::map(
  spplist[6:10] |> setNames(spplist[6:10]), 
  function(x) {
    dat_sum |> filter(spec == x) |> 
      ggplot(aes(year, rate)) + 
      geom_smooth(method = 'gam', method.args = list(family = 'quasipoisson'),
                  #formula = y ~ s(x, k = 15), 
                  se = FALSE,
                  linewidth = 1.5, color = pointblue.palette[2]) +
      geom_line(color = pointblue.palette[6]) + 
      geom_point(size = 2, color = pointblue.palette[4]) +
      labs(x = 'Annual capture rate (1979-2025)', 
           y = 'Capture rate (per 1000 net hours)') +
      scale_x_continuous(breaks = seq(1980, max(dat$year), 10)) +
      ylim(0, NA) +
      theme_classic() 
})

patchwork::wrap_plots(plot_migrants) # check they look ok

# write to file
purrr::map(names(plot_migrants),
           function(x) {
             ggsave(plot = plot_migrants[[x]],
                    filename = paste0('docs/fig/migrants/', x, '.png'), 
                    width = 5, height = 3.5, units = 'in', dpi = 300)
           })

# ALTERNATE INTERACTIVE PLOTS----------

## residents--------
residents = dat_sum |> filter(spec %in% spplist[1:5]) |> 
  mutate(
    ratelab = format(round(rate, digits = 1), nsmall = 1),
    spec = factor(spec, 
                  levels = c('CASJ', 'NWCS', 'SOSP', 'SPTO', 'WREN'),
                  labels = c('California Scrub-Jay', 
                             "Nuttall's White-crowned Sparrow",
                             'Song Sparrow', 
                             'Spotted Towhee', 
                             'Wrentit'))
  )

g1 <- ggplot(residents, aes(year, rate, color = spec)) +
  geom_smooth(method = "gam", 
              method.args = list(family = "quasipoisson"), 
              formula = y ~ s(x), se = FALSE)
g1

# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot
dat_g1 <- data.frame(
  x = gg1$data[[1]]$x,
  smooth = gg1$data[[1]]$y,
  spec = rep(levels(residents$spec), each = 80))
  
# build interactive plot
pal1 <- setNames(pointblue.palette[1:5], 
                c('California Scrub-Jay', 
                  "Nuttall's White-crowned Sparrow",
                  'Song Sparrow', 
                  'Spotted Towhee', 
                  'Wrentit'))

graph1 <- plot_ly() |> 
  add_lines(data = dat_g1 |> filter(spec == 'Wrentit'), # make this one show by default to start
            x = ~x,
            y = ~smooth,
            color = ~spec,
            colors = pal1,
            legendgroup = ~spec,
            hoverinfo = 'none') |> 
  add_lines(data = dat_g1 |> filter(spec != 'Wrentit'),
            x = ~x,
            y = ~smooth,
            color = ~spec,
            colors = pal1,
            legendgroup = ~spec,
            hoverinfo = 'none',
            visible = 'legendonly') |> 
  add_markers(data = residents |> filter(spec == 'Wrentit'),
              x = ~year,
              y = ~rate,
              marker = list(size = 5),
              color = ~spec,
              legendgroup = ~spec,
              showlegend = FALSE,
              hoverinfo = 'x+text',
              text = ~ratelab) |> 
  add_markers(data = residents |> filter(spec != 'Wrentit'),
              x = ~year,
              y = ~rate,
              marker = list(size = 5),
              color = ~spec,
              legendgroup = ~spec,
              showlegend = FALSE,
              hoverinfo = 'x+text',
              text = ~ratelab,
              visible = 'legendonly') |> 
  layout(yaxis = list(title = 'Capture rate (per 1000 net hours)',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      range = c(0, 15),
                      showgrid = FALSE,
                      automargin = TRUE,
                      hoverformat = '.2f'),
         xaxis = list(title = NA,
                      showline = TRUE,
                      ticks = 'outside',
                      showgrid = FALSE),
         hovermode = 'x',
         legend = list(x = 1, xanchor = 'right', y = 1, yanchor = 'top'),
         margin = list(r = 0, b = 10, t = 10)) |> 
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d',
                                       'zoomIn2d', 'zoomOut2d',
                                       'pan2d', 'toggleSpikelines'))

# test in viewer pane:
graph1


graph1$dependencies <- c(graph1$dependencies,
                         list(
                           htmltools::htmlDependency(
                             name = 'plotly_style_nomargin',
                             version = '1.0.0',
                             src = 'docs/widgets/lib',
                             stylesheet = 'plotly_style.css'
                           )
                         ))

# file path of final interactive graphic widget
out <- "docs/widget/graph_resident_capturestats.html"

htmlwidgets::saveWidget(graph1,
                        here::here(out),
                        selfcontained = FALSE,
                        libdir = 'lib',
                        title = 'Annual capture stats')         

## migrants---------

migrants = dat_sum |> filter(spec %in% spplist[6:10]) |> 
  mutate(
    ratelab = format(round(rate, digits = 1), nsmall = 1),
    group = if_else(spec %in% c('WIWA', 'SWTH'), 'Neotropical', 'Neotemperate'),
    spec = factor(spec, 
                  levels = c('WIWA', 'SWTH', 'FOSP', 'GCSP', 'HETH'),
                  labels = c("Wilson's Warbler", 
                             "Swainson's Thrush",
                             'Fox Sparrow', 
                             'Golden-crowned Sparrow',
                             'Hermit Thrush'))
  )

g2 <- ggplot(migrants, aes(year, rate, color = spec)) +
  geom_smooth(method = "gam", 
              method.args = list(family = "quasipoisson"), 
              formula = y ~ s(x), se = FALSE)
g2

# build plot object for rendering 
gg2 <- ggplot_build(g2)

# extract data for the loess lines from the 'data' slot
dat_g2 <- data.frame(
  x = gg2$data[[1]]$x,
  smooth = gg2$data[[1]]$y,
  spec = rep(levels(migrants$spec), each = 80)) |> 
  mutate(group = if_else(spec %in% c("Wilson's Warbler", "Swainson's Thrush"), 
                         'Neotropical', 'Neotemperate'))

# build interactive plot
pal2 <- setNames(pointblue.palette[1:5], 
                 c("Wilson's Warbler", 
                   "Swainson's Thrush",
                   'Fox Sparrow', 
                   'Golden-crowned Sparrow',
                   'Hermit Thrush'))

graph2 <- plot_ly() |> 
  add_lines(data = dat_g2 |> filter(group == "Neotropical"), # make this one show by default to start
            x = ~x,
            y = ~smooth,
            color = ~spec,
            colors = pal2,
            legendgroup = ~group,
            hoverinfo = 'none') |> 
  add_lines(data = dat_g2 |> filter(group != "Neotropical"),
            x = ~x,
            y = ~smooth,
            color = ~spec,
            colors = pal2,
            legendgroup = ~group,
            hoverinfo = 'none',
            visible = 'legendonly') |> 
  add_markers(data = migrants |> filter(group == "Neotropical"),
              x = ~year,
              y = ~rate,
              marker = list(size = 5),
              color = ~spec,
              legendgroup = ~group,
              showlegend = FALSE,
              hoverinfo = 'x+text',
              text = ~ratelab) |> 
  add_markers(data = migrants |> filter(group != "Neotropical"),
              x = ~year,
              y = ~rate,
              marker = list(size = 5),
              color = ~spec,
              legendgroup = ~group,
              showlegend = FALSE,
              hoverinfo = 'x+text',
              text = ~ratelab,
              visible = 'legendonly') |> 
  layout(yaxis = list(title = 'Capture rate (per 1000 net hours)',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      range = c(0, 15),
                      showgrid = FALSE,
                      automargin = TRUE,
                      hoverformat = '.2f'),
         xaxis = list(title = NA,
                      showline = TRUE,
                      ticks = 'outside',
                      showgrid = FALSE),
         hovermode = 'x',
         legend = list(x = 1, xanchor = 'right', y = 1, yanchor = 'top',
                       orientation = 'h'),
         margin = list(r = 0, b = 10, t = 10)) |> 
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d',
                                       'zoomIn2d', 'zoomOut2d',
                                       'pan2d', 'toggleSpikelines'))

# test in viewer pane:
graph2


graph2$dependencies <- c(graph2$dependencies,
                         list(
                           htmltools::htmlDependency(
                             name = 'plotly_style_nomargin',
                             version = '1.0.0',
                             src = 'docs/widgets/lib',
                             stylesheet = 'plotly_style.css'
                           )
                         ))

# file path of final interactive graphic widget
out <- "docs/widget/graph_migrant_capturestats.html"

htmlwidgets::saveWidget(graph2,
                        here::here(out),
                        selfcontained = FALSE,
                        libdir = 'lib',
                        title = 'Annual capture stats') 
