
# READ ME -----------------------------------------------------------------

# graph seasonal capture stats for several species to illustrate seasonal
# change in bird community

#PACKAGES
library(tidyverse)
library(plotly)

#INPUT DATA
# original copies:
# nethrspath <- 'Z:/Terrestrial/band/nethrs/allpalonethrs.dbf'
# bandpath <- 'Z:/Terrestrial/programs_and_projects/palomarin/Palodata/Band/allbandpalo.dbf'

# local copies:
nethrspath <- "rawdat/allpalonthrs_through2018.CSV"
bandpath <- "rawdat/allbandpalo_through2018.CSV"

# PALETTE
pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666')

#OUTPUT
out <- "docs/widget/graph_seasonal_capturestats.html"

# CALCULATE STATS ---------------------------------------------------------

# average net hours per month:
effort <- read_csv(here::here(nethrspath), 
                   col_types = cols_only(location = col_factor(), 
                                         date = col_date(format = '%m/%d/%Y'), 
                                         nethours = col_number())) %>% 
  filter(location == 'PN') %>% 
  mutate(year = format(date, '%Y'),
         month = format(date, '%m'),
         monthlab = format(date, '%b')) %>% 
  filter(year > 2008) %>% #cleaner data?
  # total by year and month:
  group_by(year, month, monthlab) %>% 
  summarize(nethours = sum(nethours)) %>% 
  ungroup()

# banding data
captures <- read_csv(here::here(bandpath), 
                     col_types = cols_only(loc = 'f',
                                           date = col_date(format = '%m/%d/%Y'),
                                           size = 'f',
                                           code = 'f',
                                           bandnumb = 'n',
                                           spec = 'f',
                                           age = 'n',
                                           sex = 'f')) %>% 
  mutate(year = format(date, '%Y'),
         month = format(date, '%m')) %>% 
  filter(loc == 'PN' & year > 2008 & 
           spec %in% c('WIWA', 'SWTH', 'HETH', 'GCSP', 'WREN')) %>% 
  # count total captures for each species by year and month
  group_by(spec, year, month) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(spec = as.factor(as.character(spec))) %>% 
  complete(spec, year, month, fill = list(n = 0))

# calculate capture rate per 1000 net hours
dat <- left_join(captures, effort, by = c('month', 'year')) %>% 
  mutate(rate = n / nethours * 1000,
         month = as.numeric(month) + 0.5,
         monthlab = factor(monthlab, levels = unique(monthlab)),
         spec = factor(spec, levels = c('WREN', 'SWTH', 'WIWA', 'HETH', 'GCSP')),
         speclab = recode(spec, 
                          GCSP = 'Golden-crowned Sparrow',
                          WIWA = "Wilson's Warbler",
                          SWTH = "Swainson's Thrush",
                          HETH = "Hermit Thrush",
                          WREN = 'Wrentit')) %>% 
  select(spec, speclab, monthlab, month, year, rate)

datavg <- dat %>% 
  group_by(spec, speclab, monthlab, month) %>% 
  summarize(rate = mean(rate)) %>% 
  mutate(ratelab = format(round(rate, digits = 1), nsmall = 1)) %>% 
  ungroup()

# FIT SMOOTHED LINES ------------------------------------------------------

g1 <- ggplot(dat, aes(month, rate, color = speclab)) +
  geom_smooth(method="gam", method.args = list(family="quasipoisson"), 
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
  add_markers(x = ~x, 
              y = ~avgrate,
              color = ~speclab,
              legendgroup = ~speclab,
              hoverinfo = 'text',
              text = ~paste0('</br>', speclab,
                             '</br>', monthlab, ' avg rate: ', avgratelab),
              showlegend = FALSE) %>% 
  layout(yaxis = list(title = 'Capture rate (per 1000 net hours)',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      # range = c(0,20),
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
         legend = list(x = 0.01, xanchor = 'left', y = 1, yanchor = 'top', 
                       bgcolor = NA),
         margin = list(r = 0, b = 10, t = 10)) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d',
                                       'zoomIn2d', 'zoomOut2d',
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(graph1,
                        here::here(out),
                        selfcontained = TRUE,
                        title = 'Seasonal capture stats')
