
# READ ME -----------------------------------------------------------------

# graph annual capture stats for all of Palo

#PACKAGES
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

# OUTPUT
# file path of final interactive graphic widget
out <- "docs/widget/graph_total_capturestats.html"

# CALCULATE STATS ---------------------------------------------------------

## NET HOURS------
# net hours database: (drop extra columns labeled "X")
effort_raw <- foreign::read.dbf(nethrspath) |> select(PROJECT:DUPE)
str(effort_raw)

# filter to PN after 1979
unique(effort_raw$LOCATION) # PN, PGUP, MUHO, PIGU, LACR, RECR, G5, CT, PT, HUMP, PEXA, PALO
summary(effort_raw$DATE) # 1976-01-03 through 2025-12-31

effort = effort_raw |> 
  filter(LOCATION == 'PN') |> 
  filter(DATE >= '1979-01-01')
  
# summarize by year and month
effort_sum = effort |> 
  mutate(year = format(DATE, '%Y'),
         month = format(DATE, '%m')) |> 
  group_by(year, month) |> 
  summarize(nethours = sum(NETHOURS),
            .groups = 'drop')
# 564 obs of 3 vars

## CAPTURE STATS--------
# total captures per year

band_raw = foreign::read.dbf(bandpath) # slow because this is a large database!
str(band_raw)

# filter to PN after 1979 and no later than last net hours
# > NOTE: not limited to new captures
unique(band_raw$LOC)
summary(band_raw$DATE) # 1966-01-14 thruogh 2026-03-26 (and 1 NA)

band = band_raw |> select(INITIALS:COM) |> 
  filter(LOC == 'PN') |> 
  filter(DATE >= '1979-01-01') |> 
  filter(DATE <= max(effort$DATE))
# 151173 obs of 35 vars

# summarize number of captures by year and month
captures_sum = band |> 
  mutate(year = format(DATE, '%Y'),
         month = format(DATE, '%m')) |> 
  group_by(year, month) |> 
  count() |> 
  ungroup()
# 564 obs of 3 vars

testthat::test_that('test that net hours and captures match', {
  testthat::expect_true(nrow(effort_sum) == nrow(captures_sum))
  testthat::expect_true(all(unique(effort_sum$year) %in% captures_sum$year))
  testthat::expect_true(all(unique(captures_sum$year) %in% effort_sum$year))
})

## JOIN-----------
# join captures and net hours, and assign months to specific seasons
dat = full_join(captures_sum, effort_sum, by = c('year', 'month')) |> 
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         season = case_when(month >= 3 & month <= 7 ~ 'spring', # 5 months
                            month >= 8 & month <= 10 ~ 'fall', # 3 months
                            TRUE ~ 'winter'), # 4 months
         # group Jan and Feb with previous Nov and Dec
         year = case_when(month <= 2 ~ year - 1,
                          TRUE ~ year))

testthat::test_that('test that there are no mismatched months', {
  testthat::expect_false(any(is.na(dat$n)))
  testthat::expect_false(any(is.na(dat$nethours)))
})

summary(dat)
# n: mean = 268; 1st Q = 112; min = 24
# nethours: mean = 2396; 1st Q = 1561; min = 480

hist(dat$n) # long tail with few  months > 1000
dat |> filter(n > 1000) |> arrange(desc(n)) 
# 7 months with > 1000 captures: 
# >> 2 in October (also the two highest): 1981, 1980
# >> 5 in July (in descending order): 1985, 1980, 1982, 1979
# (all of these months had > 3000 net hours)

hist(dat$nethours, breaks = 16) # bimodal (b/c seasonal) with few months < 1000
dat |> filter(nethours < 1000)
# 3 months with < 1000 net hours: 12/1980; 01/1995; 09/2025

# check for complete years:
dat |> group_by(year) |> count() |> filter(n < 12)
# >> 1978 (because Jan/Feb 1979)
# >> 2025 (10 months because Jan/Feb not yet included)


rm(effort, effort_raw, band, band_raw)


## CAPTURE RATES----------

# seasonal totals
dat_sum = dat |> 
  # drop incomplete seasons
  filter(year > 1978) |> 
  filter(!(year == 2025 & season == 'winter')) |> 
  group_by(year, season) |> 
  summarize(n = sum(n),
            nethours = sum(nethours),
            .groups = 'drop')

summary(dat_sum)
hist(dat_sum$n) # 0 to 3000; peak 500-1000
hist(dat_sum$nethours, breaks = 16) # few < 5000
dat_sum |> filter(nethours < 5000)
# fall 2025!

# annual totals
dat_annual = dat_sum |> 
  # drop incomplete year (with incomplete winter 2025-26)
  filter(year != 2025) |> 
  group_by(year) |> 
  summarize(season = 'total',
            n = sum(n),
            nethours = sum(nethours))

# combine and calculate capture rate per 1000 net hours:
dat_all = bind_rows(dat_sum |> filter(year > 1978), 
                    dat_annual) |>
  arrange(year, season) |> 
  mutate(rate = n / nethours * 1000,
         ratelab = format(round(rate, digits = 1), nsmall = 1),
         season = factor(season, levels = c('total', 'spring', 'fall', 'winter')),
         season = recode(season, total = 'Total',
                         spring = 'Spring/Summer (Mar - Jul)',
                         fall = 'Fall (Aug - Oct)',
                         winter = 'Winter (Nov - Feb)')) |> 
  select(year, season, rate, ratelab)

summary(dat_all)


# FIT SMOOTHED LINES ------------------------------------------------------

g1 <- ggplot(dat_all, aes(year, rate, color = season)) +
  geom_smooth(method = "gam", 
              method.args = list(family = "quasipoisson"), 
              formula = y ~ s(x), se = FALSE)
g1

# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot
dat2 <- data.frame(x = gg1$data[[1]]$x,
                   smooth = gg1$data[[1]]$y,
                   season = rep(levels(dat_all$season), each = 80)) %>% 
  mutate(season = factor(season, levels = c('Total', 
                                            'Spring/Summer (Mar - Jul)', 
                                            'Fall (Aug - Oct)',
                                            'Winter (Nov - Feb)')))

# PLOTLY ------------------------------------------------------------------

# build interactive plot
pal <- setNames(pointblue.palette[1:4], 
                c("Winter (Nov - Feb)", "Spring/Summer (Mar - Jul)", 
                  "Fall (Aug - Oct)", "Total"))

graph1 <- plot_ly() |> 
  add_lines(data = dat2 |> filter(season == 'Total'),
            x = ~x,
            y = ~smooth,
            color = ~season,
            colors = pal,
            legendgroup = ~season,
            hoverinfo = 'none',
            line = list(width = 4)) |> 
  add_lines(data = dat2 |> filter(season != 'Total'),
            x = ~x,
            y = ~smooth,
            color = ~season,
            colors = pal,
            legendgroup = ~season,
            hoverinfo = 'none',
            visible = 'legendonly') |> 
  add_markers(data = dat_all |> filter(season == 'Total'),
              x = ~year,
              y = ~rate,
              marker = list(size = 8),
              color = ~season,
              legendgroup = ~season,
              showlegend = FALSE,
              hoverinfo = 'x+text',
              text = ~ratelab) |> 
  add_markers(data = dat_all |> filter(season != 'Total'),
            x = ~year,
            y = ~rate,
            marker = list(size = 8),
            color = ~season,
            legendgroup = ~season,
            showlegend = FALSE,
            hoverinfo = 'x+text',
            text = ~ratelab,
            visible = 'legendonly') |> 
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

htmlwidgets::saveWidget(graph1,
                        here::here(out),
                        selfcontained = FALSE,
                        libdir = 'lib',
                        title = 'Annual capture stats by season')

