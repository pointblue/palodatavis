# READ ME -----------------------------------------------------------------
# Script to produce interactive figure with annual weather stats at Palomarin,
# including: total bioyear rain, average temps
#
# AUTHORS: Kristen Dybala (original), updated by Sarah Needles (2024 capstone project)
#
# CHANGES:
# 2024: update data through 2023; exclude temps of "99" as NA values

#PACKAGES
library(tidyverse)
library(foreign) # > only needed if reading from dbf directly
library(plotly)

# INPUT DATA
# local copy of weather data:
wthrpath <- 'rawdat/paloallwthr_12Apr2024.csv'

# STANDARD COLOR PALETTE
pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666')

#OUTPUT
#out <- "docs/widget/graph_annual_weatherstats.html" # original combo- temp + precip

out1 <- 'docs/widget/graph_avgtemp_stats.html'
out2 <- 'docs/widget/graph_precip_stats.html'

# DATA CLEANING & FORMATTING--------------------------------------------------
# clean up raw data
rawdat <- read_csv(wthrpath) |>  
  select(DATE, TIME, RAIN, RAIN_CUMUL, HIGH, LOW) |> 
  mutate(DATE = as.Date(DATE, format = '%m/%d/%Y'),
         TIME = case_when(TIME == '9999' ~ NA_character_,
                          TRUE ~ as.character(TIME)),
         RAIN = case_when(RAIN == 999.99 ~ NA_real_,
                          TRUE ~ RAIN),
         HIGH = case_when(HIGH == 999 | HIGH == 99 ~ NA_integer_, 
                          TRUE ~ HIGH),
         LOW = case_when(LOW == 999 | LOW == 99 ~ NA_integer_,
                         TRUE ~ LOW)) |> 
  filter(DATE >= '1976-01-01')

# check for any missing or unexpected values
str(rawdat)
summary(rawdat)

testthat::test_that('test that all temperature values are sensible', {
  testthat::expect_lt(max(rawdat$HIGH, na.rm = TRUE), 50) # HIGH temps should not be >50C
  testthat::expect_lt(max(rawdat$LOW, na.rm = TRUE), 25) # LOW temps should not be >25C
})

rawdat |> filter(is.na(RAIN) & RAIN_CUMUL) # one missing 2020-09-29 (and likely zero)
rawdat |> filter(DATE == '2020-09-29')
rawdat |> filter(DATE == '2020-09-28')
rawdat |> filter(DATE == '2020-09-30')

# identify bioyears for precip data and calculate daily average temp
dat = rawdat |> 
  filter(RAIN_CUMUL) |> 
  mutate(year = as.numeric(format(DATE, '%Y')),
         month = as.numeric(format(DATE, '%m')),
         bioyear = case_when(month >= 7 ~ paste0(year, '-', substr(year+1, 3, 4)),
                             month <= 6 ~ paste0(year - 1, '-', substr(year, 3, 4))),
         # for plotting purposes, align bioyear with July-Dec calendar year
         bioyearx = case_when(month >= 7 ~ year,
                              month <= 6 ~ year - 1),
         # daily average temp
         AVGTEMP = (HIGH + LOW) / 2) 
summary(dat)

# WEATHER STATS----------------------------------------------------

## annual------------

# annual precip totals by bioyear
raindat <- dat |> 
  group_by(bioyear, bioyearx) |> 
  summarize(rain = sum(RAIN, na.rm = TRUE),
            nrain = length(!is.na(RAIN)),
            .groups = 'drop') |>  
  filter(bioyearx >= 1975)
str(raindat)
summary(raindat)

summary(lm(rain ~ bioyearx, raindat)) # NS

# annual (calendar) mean of daily average temperatures, low temps, high temps
tempdat <- dat |> 
  group_by(year) |> 
  summarize(avgtemp = mean(AVGTEMP, na.rm = TRUE),
            low = mean(LOW, na.rm = TRUE),
            high = mean(HIGH, na.rm = TRUE),
            navgtemp = length(!is.na(AVGTEMP)),
            .groups = 'drop')
str(tempdat)
summary(tempdat)

summary(lm(avgtemp ~ year, tempdat)) # + NS
summary(lm(high ~ year, tempdat)) # + NS
summary(lm(low ~ year, tempdat)) # + NS

ggplot(tempdat, aes(year, avgtemp)) + geom_point() + geom_smooth()
ggplot(tempdat, aes(year, high)) + geom_point() + geom_smooth()
ggplot(tempdat, aes(year, low)) + geom_point() + geom_smooth()
# >> all have a strong warming trend 1980-2000, then flatter

summary(lm(avgtemp ~ year, tempdat |> filter(year <= 2000))) # +, p = 0.00763
summary(lm(avgtemp ~ year, tempdat |> filter(year > 2000))) # -, NS

summary(lm(high ~ year, tempdat |> filter(year <= 2000))) # +, p = 0.00339
summary(lm(high ~ year, tempdat |> filter(year > 2000))) # -, NS

summary(lm(low ~ year, tempdat |> filter(year <= 2000))) # +, p = 0.0388
summary(lm(low ~ year, tempdat |> filter(year > 2000))) # -, NS


## monthly--------
dat |> group_by(year, month) |> count() |> 
  pivot_wider(names_from = month, values_from = n) |> print(n = Inf)
# all present and accounted for


temp_monthly = dat |> 
  group_by(year, month) |> 
  summarize(avgtemp = mean(AVGTEMP, na.rm = TRUE),
            low = mean(LOW, na.rm = TRUE),
            high = mean(HIGH, na.rm = TRUE),
            navgtemp = length(!is.na(AVGTEMP)),
            .groups = 'drop')

# check for significant trends
temp_monthly |> split(temp_monthly$month) |> 
  purrr::map(\(df) lm(avgtemp ~ year, data = df)) |> 
  purrr::map(summary)
# Jan: sig+ (p = 0.00493)
# Feb: +, NS
# Mar: +, NS
# Apr: -, NS
# May: -, NS
# Jun: -, NS
# Jul: -, NS
# Aug: -, NS
# Sep: +, NS
# Oct: +, NS
# Nov: +, p = 0.0532
# Dec: +, NS

temp_monthly |> split(temp_monthly$month) |> 
  purrr::map(\(df) lm(high ~ year, data = df)) |> 
  purrr::map(summary)
# Jan: sig+ 
# Feb: +, NS
# Mar: +, NS
# Apr: -, NS
# May: sig-
# Jun: -, NS
# Jul: sig-
# Aug: sig-
# Sep: +, NS
# Oct: +, NS
# Nov: sig+
# Dec: +, NS

temp_monthly |> split(temp_monthly$month) |> 
  purrr::map(\(df) lm(low ~ year, data = df)) |> 
  purrr::map(summary)
# Jan: +, NS 
# Feb: -, NS
# Mar: +, NS
# Apr: -, NS
# May: +, NS
# Jun: +, NS
# Jul: +, NS
# Aug: +, NS
# Sep: +, NS
# Oct: +, NS
# Nov: +, NS
# Dec: +, NS
# >> no significant linear trends in low temps


ggplot(temp_monthly, aes(year, avgtemp, color = as.factor(month))) + 
  geom_point() + geom_smooth(method = 'lm', se = FALSE) 
ggplot(temp_monthly, aes(year, avgtemp)) + geom_point() + 
  geom_smooth(method = 'lm') +
  facet_wrap(~month)
# overall WARMING trend in Sep-Mar
# COOLING in Apr-Aug

ggplot(temp_monthly, aes(year, high, color = as.factor(month))) + 
  geom_smooth(method = 'lm', se = FALSE) 
ggplot(temp_monthly, aes(year, high)) + geom_point() + 
  geom_smooth(method = 'lm') +
  facet_wrap(~month)

## seasonal---------
# seasonal average temps (different season definitions than capture stats)

tempseasonal = dat |> 
  mutate(
    season = case_when(month >= 5 & month <= 8 ~ "Summer (May - Aug)", # 4 months
                       month >= 11 | month <= 1 ~ 'Winter (Nov - Jan)', # 4 months
                       month >= 2 & month <= 4 ~ 'Spring (Feb - Apr)',
                       TRUE ~ "Fall (Sep - Oct)"),  # 2 months
    # group Jan-Feb with the previous Nov-Dec
    year = case_when(month <= 2 ~ year - 1,
                     TRUE ~ year))

tempseasonal |> group_by(year, season) |> count() |> 
  pivot_wider(names_from = season, values_from = n) |> print(n = Inf)
# >> missing most 1975 (includes only Jan 1976) and part of winter 2023 (Jan 2024 missing)

temp_seas = tempseasonal |> 
  filter(year > 1975) |> 
  filter(!(year == 2023 & season == 'Winter (Nov - Jan)')) |> 
  group_by(year, season) |> 
  summarize(avgtemp = mean(AVGTEMP, na.rm = TRUE),
            low = mean(LOW, na.rm = TRUE),
            high = mean(HIGH, na.rm = TRUE),
            navgtemp = length(!is.na(AVGTEMP)),
            .groups = 'drop')

# check for seasonal trends
temp_seas |> split(temp_seas$season) |> 
  purrr::map(\(df) lm(avgtemp ~ year, data = df)) |> 
  purrr::map(summary)
# Sep-Oct: NS (pos trend)
# Feb-Apr: NS (neg trend)
# May-Aug: NS (neg trend)
# Nov-Jan: sig +

temp_seas |> split(temp_seas$season) |> 
  purrr::map(\(df) lm(high ~ year, data = df)) |> 
  purrr::map(summary)
# Sep-Oct: NS (pos trend)
# Feb-Apr: NS (neg trend)
# May-Aug: sig -
# Nov-Jan: sig +
ggplot(temp_seas, aes(year, high)) + geom_point() + 
  geom_smooth(method = 'gam') +
  facet_wrap(~season)

bind_rows(tempdat |> mutate(season = 'Annual'), 
          temp_seas |> filter(grepl('Summer|Winter', season))) |> 
  ggplot(aes(year, high, color = season)) + 
  geom_smooth(method = 'gam', se = FALSE)

bind_rows(tempdat |> mutate(season = 'Annual'), 
          temp_seas |> filter(grepl('Summer|Winter', season))) |> 
  ggplot(aes(year, avgtemp, color = season)) + 
  geom_smooth(method = 'gam', se = FALSE)



# PLOTLY ------------------------------------------------------------------
# build interactive plots


## Annual and seasonal temp------
alltemp = bind_rows(
  tempdat |> mutate(season = 'Annual'),
  temp_seas |> filter(grepl('Summer|Winter', season))) |> 
  mutate(
    lab = format(round(avgtemp, digits = 2), nsmall = 2))

g1 <- ggplot(alltemp, aes(year, avgtemp, color = season)) +
  geom_smooth(method = "gam", method.args = list(family = "gaussian"), 
              formula = y ~ s(x), se = FALSE)
g1


# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot to use with plotly
pdat1 <- data.frame(x = gg1$data[[1]]$x,
                   smooth = gg1$data[[1]]$y,
                   season = rep(unique(alltemp$season), each = 80)) 

# color palette: Apr-Aug are cooling; Sep-Mar warming
pal <- setNames(pointblue.palette[c(4,3,2)],
                unique(alltemp$season))


graph1 <- plot_ly() |> 
  # smoothed trend line for avgtemp
  add_trace(data = pdat1 |>  filter(season == 'Annual'),
            x = ~x,
            y = ~smooth,
            color = ~season,
            colors = pal,
            line = list(width = 4),
            hoverinfo = 'none',
            type = 'scatter',
            mode = 'lines',
            legendgroup = ~season,
            showlegend = TRUE) |> 
  # smoothed trend line for seasonal avgtemps
  add_trace(data = pdat1 |>  filter(season != 'Annual'),
            x = ~x,
            y = ~smooth,
            color = ~season,
            colors = pal,
            hoverinfo = 'none',
            type = 'scatter',
            mode = 'lines',
            legendgroup = ~season,
            visible = 'legendonly',
            showlegend = TRUE) |>
  # annual points 
  add_markers(data = alltemp |>  filter(season == 'Annual'),
            x = ~year,
            y = ~avgtemp,
            color = ~season,
            colors = pal,
            #line = list(width = 1),
            hoverinfo = 'text',
            text = ~paste0(year, ': ', lab),
            type = 'scatter',
            #mode = 'markers+lines',
            marker = list(size = 8),
            legendgroup = ~season,
            showlegend = FALSE) |> 
  # annual points and connecting lines for avg low temp
  add_markers(data = alltemp |> filter(season != 'Annual'),
            x = ~year,
            y = ~avgtemp,
            color = ~season,
            colors = pal,
            #line = list(width = 1),
            hoverinfo = 'text',
            text = ~paste0(year, ': ', lab),
            type = 'scatter',
            #mode = 'markers+lines',
            #marker = list(size = 8),
            legendgroup = ~season,
            visible = 'legendonly',
            showlegend = FALSE) |>
  layout(
    yaxis = list(title = 'Daily Average Temperature (\u00B0C)\n\n\n',
                 font = list(size = 14),
                 showline = TRUE,
                 ticks = 'outside',
                 tick0 = 0,
                 range = c(5, 20),
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
    margin = list(r = 0, b = 0, t = 0, l = 50, pad = 0)) |> 
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
                        here::here(out1),
                        selfcontained = FALSE,
                        libdir = 'lib',
                        title = 'Average temperature stats')


## Annual precip data----------

raindat |> filter(nrain < 365)
# >> missing half of 1975-76 and half of 2023-24

allrain = raindat |> 
  filter(bioyearx > 1975 & bioyearx < 2023) |> 
  mutate(
    lab = paste0(format(round(rain, digits = 0), nsmall = 0), ' mm'))

g2 <- ggplot(allrain, aes(bioyearx, rain)) +
  geom_smooth(method = "gam", method.args = list(family = "gaussian"), 
              formula = y ~ s(x), se = FALSE)
g2

# build plot object for rendering 
gg2 <- ggplot_build(g2)

# extract data for the loess lines from the 'data' slot to use with plotly
pdat2 <- data.frame(x = gg2$data[[1]]$x,
                    smooth = gg2$data[[1]]$y) 


graph2 <- plot_ly() |> 
  # smoothed trend line for precip
  add_trace(data = pdat2,
            x = ~x,
            y = ~smooth,
            color = I(pointblue.palette[4]),
            line = list(width = 4),
            hoverinfo = 'none',
            type = 'scatter',
            mode = 'lines',
            showlegend = FALSE) |>
  # annual points and connecting lines 
  add_trace(data = sdat %>% filter(name == 'Total precipitation (mm)'),
            x = ~year,
            y = ~value,
            color = I(pointblue.palette[4]),
            line = list(width = 1),
            hoverinfo = 'text',
            text = ~paste0(bioyear, ': ', lab),
            type = 'scatter',
            mode = 'markers+lines',
            marker = list(size = 8),
            showlegend = FALSE) |>
  layout(yaxis = list(title = 'Total Precipitation (mm) by Bioyear (July-June)',
                       font = list(size = 14),
                       showline = TRUE,
                       ticks = 'outside',
                       tick0 = 0,
                       #ticksuffix = '  ',
                       range = c(0, 2000),
                       showgrid = FALSE,
                       automargin = TRUE,
                       hoverformat = '.2f'),
         xaxis = list(title = NA,
                      showline = TRUE,
                      showgrid = FALSE),
         hovermode = 'x',
         legend = list(x = 0.01, xanchor = 'left', y = 1, yanchor = 'top', 
                       bgcolor = NA),
         margin = list(r = 0, b = 0, t = 0, l = 50, pad = 0)) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d',
                                       'zoomIn2d', 'zoomOut2d',
                                       'pan2d', 'toggleSpikelines'))

graph2

graph2$dependencies <- c(graph2$dependencies,
                         list(
                           htmltools::htmlDependency(
                             name = 'plotly_style_nomargin',
                             version = '1.0.0',
                             src = 'docs/widget/lib',
                             stylesheet = 'plotly_style.css'
                           )
                         ))



htmlwidgets::saveWidget(graph2,
                        here::here(out2),
                        selfcontained = FALSE,
                        libdir = 'lib',
                        title = 'Annual precipitation')
