# READ ME -----------------------------------------------------------------

# graph annual Doug fir tree height stats

#PACKAGES
library(tidyverse)
library(foreign)
library(plotly)

#INPUT DATA
# original:
# vegpath <- 'Z:/Terrestrial/programs_and_projects/palomarin/Palodata/Veg/VegTrans/Compiled/for _Palo_visualizations/allpsme_thru2018.dbf'

# local copy:
vegpath <- "rawdat/allpsme_thru2018.dbf"

# PALETTE
pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666')

#OUTPUT
out <- "docs/widget/graph_annual_vegheight.html"


# CALCULATE SUMMARY STATS -------------------------------------------------
# summarize mean and SE for tree height in each year

veg <- foreign::read.dbf(vegpath) %>% 
  mutate(HEIGHT_M = case_when(HEIGHT_M > 100 ~ HEIGHT_M / 100,
                              TRUE ~ HEIGHT_M)) %>% 
  filter(YEAR >= 1983) %>% 
  group_by(YEAR) %>% 
  summarize(n = length(HEIGHT_M),
            height = mean(HEIGHT_M),
            se = sd(HEIGHT_M)/sqrt(n))

# get values for smoothed trend line
mod <- loess(height ~ YEAR, data = veg)
veg$smooth <- predict(mod)

# FIT SMOOTHED LINES ------------------------------------------------------

g1 <- ggplot(veg, aes(YEAR, height)) +
  geom_smooth(method="gam", method.args = list(family="quasipoisson"), 
              formula = y ~ s(x, k = 7), se = FALSE)

# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot
dat2 <- data.frame(x = gg1$data[[1]]$x,
                   smooth = gg1$data[[1]]$y)



# PLOTLY ------------------------------------------------------------------
# build interactive plot

graph1 <- plot_ly() %>%
  add_lines(data = dat2,
            x = ~x,
            y = ~smooth,
            line = list(width = 3, color = pointblue.palette[3]),
            hoverinfo = 'none') %>% 
  add_markers(data = veg,
              x = ~YEAR,
              y = ~height,
              marker = list(size = 8,
                            color = pointblue.palette[4]),
              showlegend = F,
              hoverinfo = 'text+y',
              text = ~YEAR,
              error_y = list(type = 'data',
                             symmetric = TRUE,
                             array = ~se,
                             color = pointblue.palette[4])) %>%
  layout(yaxis = list(title = 'Douglas fir height (meters)',
                      font = list(size = 14),
                      showline = TRUE,
                      ticks = 'outside',
                      tick0 = 0,
                      range = c(0,13),
                      showgrid = FALSE,
                      automargin = TRUE,
                      hoverformat = '.2f'),
         xaxis = list(title = NA,
                      showline = TRUE,
                      ticks = 'outside',
                      # range = c(1980,maxyear+4),
                      showgrid = FALSE),
         hovermode = 'x',
         margin = list(r = 0, b = 10, t = 10)) %>%
  config(displaylogo = FALSE, showTips = FALSE,
         modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d',
                                       'zoomIn2d', 'zoomOut2d',
                                       'pan2d', 'toggleSpikelines'))

htmlwidgets::saveWidget(graph1,
                        here::here(out),
                        selfcontained = TRUE,
                        title = 'Douglas fir height')
