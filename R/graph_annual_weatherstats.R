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
  summarize(nrain = sum(!is.na(RAIN)),
            rain = sum(RAIN, na.rm = TRUE),
            .groups = 'drop') |>  
  filter(bioyearx >= 1975)
str(raindat)
summary(raindat)
raindat |> filter(nrain < 365)
# >> missing half of 1975-76 and half of 2023-24

raindat_clean = raindat |> 
  mutate(rain = if_else(nrain < 0.8*365, NA_real_, rain))

summary(lm(rain ~ bioyearx, raindat_clean)) 
# NS but negative trend overall

raindat_clean |> pull(rain) |> mean(na.rm = TRUE)
# 846mm

# annual (calendar) mean of daily average temperatures, low temps, high temps
tempdat <- dat |> 
  group_by(year) |> 
  summarize(navgtemp = sum(!is.na(AVGTEMP)),
            avgtemp = mean(AVGTEMP, na.rm = TRUE),
            low = mean(LOW, na.rm = TRUE),
            high = mean(HIGH, na.rm = TRUE),
            .groups = 'drop')
str(tempdat)
summary(tempdat)
hist(tempdat$navgtemp) # some years with a lot of missing data
tempdat |> filter(navgtemp > 0.9*365) |> nrow()
# 38 years (out of 48) with >90% of daily avg temp data
tempdat |> filter(navgtemp > 0.8*365) |> nrow()
# 42 years with >80%
tempdat |> filter(navgtemp < 0.8*365)
# 1978, 1983, 1984, 2002, 2003, 2006

tempdat_clean = tempdat |> 
  mutate(across(c(avgtemp, low, high),
                ~if_else(navgtemp < 0.8*365, NA_real_, .)))

summary(lm(avgtemp ~ year, tempdat_clean)) # 0 NS
summary(lm(high ~ year, tempdat_clean)) # 0 NS
summary(lm(low ~ year, tempdat_clean)) # + NS

ggplot(tempdat_clean, aes(year, avgtemp)) + geom_point() + geom_smooth()
ggplot(tempdat_clean, aes(year, high)) + geom_point() + geom_smooth()
ggplot(tempdat_clean, aes(year, low)) + geom_point() + geom_smooth()
# >> all have a strong warming trend 1980-2000, then flatter

summary(lm(avgtemp ~ year, tempdat_clean |> filter(year <= 2000))) 
summary(lm(high ~ year, tempdat_clean |> filter(year <= 2000))) 
summary(lm(low ~ year, tempdat_clean |> filter(year <= 2000))) 
# all sig+
summary(lm(avgtemp ~ year, tempdat_clean |> filter(year > 2000))) 
summary(lm(high ~ year, tempdat_clean |> filter(year > 2000))) 
summary(lm(low ~ year, tempdat_clean |> filter(year > 2000))) 
# all +NS

## monthly--------
dat |> group_by(year, month) |> count() |> 
  pivot_wider(names_from = month, values_from = n) |> print(n = Inf)
# all present and accounted for


temp_monthly = dat |> 
  group_by(year, month) |> 
  summarize(navgtemp = sum(!is.na(AVGTEMP)),
            avgtemp = mean(AVGTEMP, na.rm = TRUE),
            low = mean(LOW, na.rm = TRUE),
            high = mean(HIGH, na.rm = TRUE),
            .groups = 'drop')
summary(temp_monthly)
temp_monthly |> filter(is.na(avgtemp)) 
# missing all data in Nov-Dec 1978; Oct 1983; Jul-Aug 1984
# missing all low data in Nov-Dec 2002; Jan-Feb 2003
temp_monthly |> filter(navgtemp < 27 & navgtemp > 0)
# 65 months with < 90% of daily data
temp_monthly |> filter(navgtemp < 24 & navgtemp > 0)
# 31 months with < 80% of daily data

temp_monthly_clean = temp_monthly |> 
  mutate(
    across(c(avgtemp, low, high),
           ~if_else(navgtemp < 0.8*30, NA_real_, .))
  )

# check for significant trends
temp_monthly_clean |> split(temp_monthly_clean$month) |> 
  purrr::map(\(df) lm(avgtemp ~ year, data = df)) |> 
  purrr::map(summary)
# Jan: sig+
# Feb: 0, NS
# Mar: 0, NS
# Apr: -, NS
# May: -, NS
# Jun: -, NS
# Jul: -, NS
# Aug: 0, NS
# Sep: +, NS
# Oct: +, NS
# Nov: +, NS
# Dec: +, NS
# >> only significant change in january

# HIGH TEMPS
temp_monthly_clean |> split(temp_monthly_clean$month) |> 
  purrr::map(\(df) lm(high ~ year, data = df)) |> 
  purrr::map(summary)
# Jan: sig+ 
# Feb: +, NS
# Mar: 0, NS
# Apr: -, NS
# May: sig-
# Jun: sig-
# Jul: sig-
# Aug: -, p = 0.0812
# Sep: +, NS
# Oct: +, NS
# Nov: sig+
# Dec: +, NS

# >> most of the significant changes happening in high temps; increasing Nov-Jan
# (and trending positive in Sep-Oct and Feb too) and decreasing May-July (and
# trending negative in Apr and Aug)

# LOW TEMPS
temp_monthly_clean |> split(temp_monthly_clean$month) |> 
  purrr::map(\(df) lm(low ~ year, data = df)) |> 
  purrr::map(summary)
# Jan: sig+
# Feb: 0, NS
# Mar: 0, NS
# Apr: 0, NS
# May: +, NS
# Jun: 0, NS
# Jul: +, NS
# Aug: +, NS
# Sep: 0, NS
# Oct: +, NS
# Nov: 0, NS
# Dec: +, NS
# >> only significant change in January

ggplot(temp_monthly_clean, aes(year, avgtemp)) + geom_point() + 
  geom_smooth(method = 'lm') +
  facet_wrap(~month)
ggplot(temp_monthly_clean, aes(year, high)) + geom_point() + 
  geom_smooth(method = 'lm') +
  facet_wrap(~month)

## seasonal---------
# seasonal average temps (different season definitions than capture stats)

tempseasonal = dat |> 
  mutate(
    season = case_when(month >= 5 & month <= 7 ~ "Summer (May - Jul)", # 3 months
                       month >= 11 | month == 1 ~ 'Winter (Nov - Jan)', # 3 months
                       month >= 2 & month <= 4 ~ 'Spring (Feb - Apr)',
                       TRUE ~ "Fall (Aug - Oct)"),  # 3 months
    # group Jan with the previous Nov-Dec
    year = case_when(month <= 1 ~ year - 1,
                     TRUE ~ year))

tempseasonal |> group_by(year, season) |> count() |> 
  pivot_wider(names_from = season, values_from = n) |> print(n = Inf)
# >> missing most 1975 (includes only Jan 1976) and part of winter 2023 (Jan 2024 missing)

temp_seas = tempseasonal |> 
  filter(year > 1975) |> 
  filter(!(year == 2023 & season == 'Winter (Nov - Jan)')) |> 
  group_by(year, season) |> 
  summarize(navgtemp = sum(!is.na(AVGTEMP)),
            avgtemp = mean(AVGTEMP, na.rm = TRUE),
            low = mean(LOW, na.rm = TRUE),
            high = mean(HIGH, na.rm = TRUE),
            .groups = 'drop')
summary(temp_seas)
temp_seas |> filter(is.na(avgtemp)) # Winter 2002 missing all data

temp_seas |> group_by(season) |> summarize(n = max(navgtemp))
# fall: 92 (Aug, Sep, Oct)
# spring: 90 (Feb, Mar, Apr)
# summar: 92 (May, Jun, Jul)
# winter: 92 (Nov-Jan)

temp_seas |> group_by(season) |> 
  filter(navgtemp < 0.8*max(navgtemp))
# 19 seasons with < 80% of days accounted for

temp_seas_clean = temp_seas |> 
  mutate(
    across(c(avgtemp, low, high),
           ~if_else(navgtemp < 0.8*92, NA_real_, .))
  )

# check for seasonal trends
temp_seas_clean |> split(temp_seas_clean$season) |> 
  purrr::map(\(df) lm(avgtemp ~ year, data = df)) |> 
  purrr::map(summary)
# Fall: NS
# Spring: NS
# Summer: NS
# Winter: nearly sig +

temp_seas_clean |> split(temp_seas_clean$season) |> 
  purrr::map(\(df) lm(high ~ year, data = df)) |> 
  purrr::map(summary)
# Fall: NS
# Spring: NS
# Summer: sig -
# Winter: sig +
ggplot(temp_seas_clean, aes(year, high)) + geom_point() + 
  geom_smooth(method = 'gam') +
  facet_wrap(~season)
ggplot(temp_seas_clean, aes(year, high)) + geom_point() + 
  geom_smooth(method = 'lm') +
  facet_wrap(~season)


# PLOTLY ------------------------------------------------------------------
# build interactive plots


## Annual and seasonal temp------
alltemp = bind_rows(
  tempdat_clean |> mutate(season = 'Annual'),
  temp_seas_clean |> filter(grepl('Summer|Winter', season))) |> 
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
            #visible = 'legendonly',
            showlegend = TRUE) |>
  # annual points 
  add_markers(data = alltemp |> filter(season == 'Annual') |> filter(!is.na(avgtemp)),
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
  add_markers(data = alltemp |> filter(season != 'Annual') |> filter(!is.na(avgtemp)),
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
            #visible = 'legendonly',
            showlegend = FALSE) |>
  layout(
    yaxis = list(title = 'Average Temperature (\u00B0C)\n\n\n',
                 font = list(size = 14),
                 showline = TRUE,
                 ticks = 'outside',
                 tick0 = 0,
                 range = c(6, 18),
                 showgrid = FALSE,
                 automargin = TRUE,
                 hoverformat = '.2f'),
    xaxis = list(title = NA,
                 showline = TRUE,
                 showgrid = FALSE),
    hovermode = 'x',
    legend = list(x = 1, xanchor = 'right', y = 1, yanchor = 'top', 
                  bgcolor = NA)) |> 
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

allrain = raindat_clean |> 
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
  add_segments(x = 1975, xend = 2023, 
               y = mean(allrain$rain, na.rm = TRUE),
               yend = mean(allrain$rain, na.rm = TRUE),
               line = list(dash = 'dash'), 
               color = I(pointblue.palette[6])) |> 
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
  layout(
    yaxis = list(title = 'Total Precipitation (mm) by Bioyear (July-June)',
                 font = list(size = 14),
                 showline = TRUE,
                 ticks = 'outside',
                 tick0 = 0,
                 #ticksuffix = '  ',
                 range = c(0, 1800),
                 showgrid = FALSE,
                 automargin = TRUE,
                 hoverformat = '.2f'),
    xaxis = list(title = NA,
                 showline = TRUE,
                 showgrid = FALSE),
    hovermode = 'x'
  ) |> 
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
