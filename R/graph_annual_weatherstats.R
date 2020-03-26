
# READ ME -----------------------------------------------------------------

# graph annual weather stats: total bioyear rain, average temps

#PACKAGES
library(tidyverse)
library(foreign)
library(plotly)

#INPUT DATA
# original copies:
# wthrpath <- 'Z:\Terrestrial\programs_and_projects\palomarin\Palodata\Weather\compiled\ paloallwthr.dbf'

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
  filter(DATE >= '1975-01-01' & RAIN_CUMUL == TRUE) %>% 
  mutate(year = as.numeric(format(DATE, '%Y')),
         month = as.numeric(format(DATE, '%m')),
         bioyear = case_when(month >= 7 ~ paste0(year, '-', substr(year+1, 3, 4)),
                             month <= 6 ~ paste0(year - 1, '-', substr(year, 3, 4))),
         year = case_when(month >= 7 ~ year,
                          month <= 6 ~ year - 1)) 
  
sdat <- dat %>% 
  mutate(AVGTEMP = (HIGH + LOW) / 2) %>% 
  group_by(bioyear, year) %>% 
  summarize(rain = sum(RAIN),
            nrain = length(!is.na(RAIN)),
            high = mean(HIGH, na.rm = TRUE),
            nhigh = length(!is.na(HIGH)),
            low = mean(LOW, na.rm = TRUE),
            nlow = length(!is.na(LOW)),
            avgtemp = mean(AVGTEMP, na.rm = TRUE),
            navgtemp = length(!is.na(AVGTEMP))) %>% 
  filter(nrain > 300) %>% 
  select(year, bioyear, rain, avgtemp) %>% 
  # convert to meters
  mutate(rain = rain / 100) %>% 
  pivot_longer(rain:avgtemp) %>% 
  mutate(lab = format(round(value, digits = 2), nsmall = 2),
         lab = case_when(name == 'rain' ~ paste0(lab, 'm'),
                         name == 'avgtemp' ~ paste0(lab, 'C')),
         name = recode(name,
                       rain = 'Total precipitation (m)',
                       avgtemp = 'Annual average temperature (C)'),
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
                   season = rep(levels(sdat$name), each = 80)) 

# PLOTLY ------------------------------------------------------------------

# build interactive plot
pal <- setNames(pointblue.palette[c(2, 4)], 
                c('Annual average temperature (C)', 'Total precipitation (m)'))

graph1 <- plot_ly() %>%
  add_trace(data = dat2,
            x = ~x,
            y = ~smooth,
            color = ~season,
            colors = pal,
            line = list(width = 3),
            hoverinfo = 'none',
            type = 'scatter',
            mode = 'lines',
            legendgroup = ~season,
            showlegend = FALSE) %>% 
  add_trace(data = sdat,
            x = ~year,
            y = ~value,
            color = ~name,
            colors = pal,
            line = list(width = 1),
            hoverinfo = 'text',
            text = ~paste0(bioyear, ': ', lab),
            type = 'scatter',
            mode = 'markers',
            legendgroup = ~name) %>%
  layout(yaxis = list(title = 'Preciptation (m) or temperature (C)',
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
                      tickvals = seq(1975, 2015, 5),
                      ticktext = unique(sdat$bioyear)[seq(1, 41, 5)],
                      showgrid = FALSE),
         hovermode = 'x',
         legend = list(x = 0.01, xanchor = 'left', y = 1, yanchor = 'top', 
                       bgcolor = NA),
         margin = list(r = 0, b = 0, t = 0, l = 0)) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d',
                                       'zoomIn2d', 'zoomOut2d',
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(graph1,
                        here::here(out),
                        selfcontained = TRUE,
                        title = 'Annual weather stats')
