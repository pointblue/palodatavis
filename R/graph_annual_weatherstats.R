
# READ ME -----------------------------------------------------------------

# graph annual weather stats: total bioyear rain, average temps

#PACKAGES
library(tidyverse)
library(foreign)
library(plotly)

#INPUT DATA
# original copies:
# wthrpath <- 'Z:\Terrestrial\programs_and_projects\palomarin\Palodata\Weather\compiled\paloallwthr.dbf'

# local copy:
wthrpath <- "rawdat/paloallwthr.dbf"

# PALETTE
pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666')

#OUTPUT
out <- "docs/widget/graph_annual_weatherstats.html"

# SUMMARIZE DATA ---------------------------------------------------------

dat <- foreign::read.dbf(wthrpath) %>% 
  select(DATE, TIME, RAIN, RAIN_INCH, RAIN_CUMUL, HIGH, LOW) %>% 
  mutate(TIME = case_when(TIME == '9999' ~ NA_character_,
                          TRUE ~ as.character(TIME)),
         RAIN = case_when(RAIN == 999.99 ~ NA_real_,
                          TRUE ~ RAIN),
         RAIN_INCH = case_when(RAIN_INCH == 99.99 ~ NA_real_,
                             TRUE ~ RAIN_INCH),
         HIGH = case_when(HIGH == 999 ~ NA_integer_,
                          TRUE ~ HIGH),
         LOW = case_when(LOW == 999 ~ NA_integer_,
                         TRUE ~ LOW)) %>% 
  filter(DATE >= '1976-01-01' & RAIN_CUMUL == TRUE) %>% 
  mutate(year = as.numeric(format(DATE, '%Y')),
         month = as.numeric(format(DATE, '%m')),
         bioyear = case_when(month >= 7 ~ paste0(year, '-', substr(year+1, 3, 4)),
                             month <= 6 ~ paste0(year - 1, '-', substr(year, 3, 4))),
         # for plotting purposes, align bioyear with July-Dec calendar year
         bioyearx = case_when(month >= 7 ~ year,
                              month <= 6 ~ year - 1),
         AVGTEMP = (HIGH + LOW) / 2) 
  
raindat <- dat %>% 
  group_by(bioyear, bioyearx) %>% 
  summarize(rain = sum(RAIN),
            nrain = length(!is.na(RAIN))) %>% 
  filter(bioyearx >= 1975)

tempdat <- dat %>% 
  group_by(year) %>% 
  summarize(avgtemp = mean(AVGTEMP, na.rm = TRUE),
            navgtemp = length(!is.na(AVGTEMP)))

sdat <- full_join(tempdat, raindat, by = c('year' = 'bioyearx')) %>% 
  mutate(rain = case_when(nrain < 300 ~ NA_real_,
                          TRUE ~ rain)) %>% 
  select(year, bioyear, avgtemp, rain) %>% 
  pivot_longer(avgtemp:rain) %>% 
  mutate(lab = case_when(name == 'rain' ~ format(round(value, digits = 0), nsmall = 0),
                         name == 'avgtemp' ~ format(round(value, digits = 2), nsmall = 2)),
         lab = case_when(name == 'rain' ~ paste0(lab, 'mm'),
                         name == 'avgtemp' ~ paste0(lab, '\u00B0C')),
         name = recode(name,
                       rain = 'Total precipitation (mm)',
                       avgtemp = 'Annual average temperature (\u00B0C)'),
         name = as.factor(name))

# FIT SMOOTHED LINES ------------------------------------------------------

g1 <- sdat %>% 
  ggplot(aes(year, value, color = name)) +
  geom_smooth(method="gam", method.args = list(family="quasipoisson"), 
              formula = y ~ s(x), se = FALSE)

# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot
dat2 <- data.frame(x = gg1$data[[1]]$x,
                   smooth = gg1$data[[1]]$y,
                   name = rep(levels(sdat$name), each = 80)) 

# PLOTLY ------------------------------------------------------------------

# build interactive plot
pal <- setNames(pointblue.palette[c(2, 4)], 
                c('Annual average temperature (\u00B0C)', 
                  'Total precipitation (mm)'))

graph1 <- plot_ly() %>%
  add_trace(data = dat2 %>% filter(name == 'Annual average temperature (\u00B0C)'),
            x = ~x,
            y = ~smooth,
            color = ~name,
            colors = pal,
            line = list(width = 3),
            hoverinfo = 'none',
            type = 'scatter',
            mode = 'lines',
            legendgroup = ~name,
            showlegend = FALSE) %>% 
  add_trace(data = dat2 %>% filter(name == 'Total precipitation (mm)'),
            x = ~x,
            y = ~smooth,
            yaxis = 'y2',
            color = ~name,
            colors = pal,
            line = list(width = 3),
            hoverinfo = 'none',
            type = 'scatter',
            mode = 'lines',
            legendgroup = ~name,
            showlegend = FALSE) %>% 
  add_trace(data = sdat %>% filter(name == 'Annual average temperature (\u00B0C)'),
            x = ~year,
            y = ~value,
            color = ~name,
            colors = pal,
            line = list(width = 1),
            hoverinfo = 'text',
            text = ~paste0(year, ': ', lab),
            type = 'scatter',
            mode = 'markers',
            legendgroup = ~name) %>%
  add_trace(data = sdat %>% filter(name == 'Total precipitation (mm)'),
            x = ~year,
            y = ~value,
            yaxis = 'y2',
            color = ~name,
            colors = pal,
            line = list(width = 1),
            hoverinfo = 'text',
            text = ~paste0(bioyear, ': ', lab),
            type = 'scatter',
            mode = 'markers',
            legendgroup = ~name) %>%
  layout(yaxis = list(title = 'Temperature (\u00B0C)\n\n\n',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      range = c(0,20),
                      showgrid = FALSE,
                      automargin = TRUE,
                      hoverformat = '.2f'),
         yaxis2 = list(title = 'Precipitation (mm)',
                       overlaying = 'y',
                       side = 'right',
                       font = list(size = 14),
                       showline = TRUE,
                       ticks = 'outside',
                       tick0 = 0,
                       ticksuffix = '  ',
                       range = c(0,2000),
                       showgrid = FALSE,
                       automargin = TRUE,
                       hoverformat = '.2f'),
         xaxis = list(title = NA,
                      showline = TRUE,
                      # ticks = 'outside',
                      # tickmode = 'array',
                      # tickvals = seq(1975, 2015, 5),
                      # ticktext = unique(sdat$bioyear)[seq(1, 41, 5)],
                      showgrid = FALSE),
         hovermode = 'x',
         legend = list(x = 0.01, xanchor = 'left', y = 1, yanchor = 'top', 
                       bgcolor = NA),
         margin = list(r = 0, b = 0, t = 0, l = 50)) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d',
                                       'zoomIn2d', 'zoomOut2d',
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(graph1,
                        here::here(out),
                        selfcontained = TRUE,
                        title = 'Annual weather stats')
