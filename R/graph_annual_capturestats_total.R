
# READ ME -----------------------------------------------------------------

# graph annual capture stats for all of Palo

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
out <- "docs/widget/graph_total_capturestats.html"

# CALCULATE STATS ---------------------------------------------------------

# total net hours per year:
effort <- read_csv(here::here(nethrspath), 
                   col_types = cols_only(location = col_factor(), 
                                         date = col_date(format = '%m/%d/%Y'), 
                                         nethours = col_number())) %>% 
  filter(location == 'PN') %>% 
  mutate(year = format(date, '%Y'),
         month = format(date, '%m')) %>% 
  filter(year >= 1979) %>% 
  # total by year and month:
  group_by(year, month) %>% 
  summarize(nethours = sum(nethours)) %>% 
  ungroup()

# total captures per year
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
  filter(loc == 'PN' & year >= 1979) %>% 
  # count total captures for each year and month
  group_by(year, month) %>% 
  count() %>% 
  ungroup()

# calculate capture rate per 1000 net hours, and fit smoothed line
dat <- left_join(captures, effort, by = c('year', 'month')) %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         season = case_when(month >= 3 & month <= 7 ~ 'spring',
                            month >= 8 & month <= 10 ~ 'fall',
                            TRUE ~ 'winter'),
         # group Jan and Feb with previous Nov and Dec
         year = case_when(month <= 2 ~ year - 1,
                          TRUE ~ year)) %>% 
  group_by(year, season) %>% 
  summarize(n = sum(n),
            nethours = sum(nethours)) %>% 
  # calculate annual total
  pivot_longer(n:nethours) %>% 
  pivot_wider(names_from = season, values_from = value) %>% 
  # drop 1978 (only data from Jan-Feb 1979 showing as winter 1978)
  filter(!is.na(spring)) %>% 
  mutate(total = fall + spring + winter) %>% 
  pivot_longer(winter:total, names_to = 'season') %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  # calculate rate
  mutate(rate = n / nethours * 1000,
         ratelab = format(round(rate, digits = 1), nsmall = 1),
         season = factor(season, levels = c('total', 'spring', 'fall', 'winter')),
         season = recode(season, total = 'Total',
                         spring = 'Spring/Summer (Mar - Jul)',
                         fall = 'Fall (Aug - Oct)',
                         winter = 'Winter (Nov - Feb)')) %>% 
  select(year, season, rate, ratelab)


# FIT SMOOTHED LINES ------------------------------------------------------

g1 <- ggplot(dat, aes(year, rate, color = season)) +
  geom_smooth(method="gam", method.args = list(family="quasipoisson"), 
              formula = y ~ s(x), se = FALSE)

# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot
dat2 <- data.frame(x = gg1$data[[1]]$x,
                   smooth = gg1$data[[1]]$y,
                   season = rep(levels(dat$season), each = 80)) %>% 
  mutate(season = factor(season, levels = c('Total', 
                                            'Spring/Summer (Mar - Jul)', 
                                            'Fall (Aug - Oct)',
                                            'Winter (Nov - Feb)')))

# PLOTLY ------------------------------------------------------------------

# gg1 <- ggplot(dat, aes(year, rate, group = season, color = season)) + 
#   geom_line(color = pointblue.palette[6]) +
#   geom_point() +
#   geom_smooth(method = 'gam', method.args = list(family = 'quasipoisson'),
#               formula = y ~ s(x), se = FALSE) +
#   labs(color = NULL) +
#   theme_classic()

# build interactive plot
pal <- setNames(pointblue.palette[1:4], 
                c("Winter (Nov - Feb)", "Spring/Summer (Mar - Jul)", 
                  "Fall (Aug - Oct)", "Total"))

graph1 <- plot_ly() %>%
  add_lines(data = dat2 %>% filter(season == 'Total'),
            x = ~x,
            y = ~smooth,
            color = ~season,
            colors = pal,
            legendgroup = ~season,
            hoverinfo = 'none',
            line = list(width = 4)) %>% 
  add_lines(data = dat2 %>% filter(season != 'Total'),
            x = ~x,
            y = ~smooth,
            color = ~season,
            colors = pal,
            legendgroup = ~season,
            hoverinfo = 'none',
            visible = 'legendonly') %>% 
  add_markers(data = dat %>% filter(season == 'Total'),
              x = ~year,
              y = ~rate,
              color = ~season,
              legendgroup = ~season,
              showlegend = FALSE,
              hoverinfo = 'x+text',
              text = ~ratelab) %>%
  add_markers(data = dat %>% filter(season != 'Total'),
            x = ~year,
            y = ~rate,
            color = ~season,
            legendgroup = ~season,
            showlegend = FALSE,
            hoverinfo = 'x+text',
            text = ~ratelab,
            visible = 'legendonly') %>% 
  layout(yaxis = list(title = 'Capture rate (per 1000 net hours)',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      range = c(0, 275),
                      showgrid = FALSE,
                      automargin = TRUE,
                      hoverformat = '.2f'),
         xaxis = list(title = NA,
                      showline = TRUE,
                      ticks = 'outside',
                      showgrid = FALSE),
         hovermode = 'x',
         legend = list(x = 1, xanchor = 'right', y = 1, yanchor = 'top'),
         margin = list(r = 0, b = 10, t = 10)) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d',
                                       'zoomIn2d', 'zoomOut2d',
                                       'pan2d', 'toggleSpikelines'))

graph1$dependencies <- c(graph1$dependencies,
                         list(
                           htmltools::htmlDependency(
                             name = 'plotly_style_nomargin',
                             version = '1.0.0',
                             src = here::here('Rmd'),
                             stylesheet = 'plotly_style.css'
                           )
                         ))

htmlwidgets::saveWidget(graph1,
                        here::here(out),
                        selfcontained = TRUE,
                        title = 'Annual capture stats by season')

