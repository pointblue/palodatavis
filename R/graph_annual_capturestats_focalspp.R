
# READ ME -----------------------------------------------------------------

# graph annual capture stats for focal species

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
wrenpath <- "docs/capturestats_WREN.html"

# CALCULATE STATS ---------------------------------------------------------
spplist <- c('WREN', 'SOSP', 'NWCS', 'SPTO', 'CASJ', 'WIWA', 'SWTH', 'FOSP',
             'GCSP', 'HETH')

# total net hours per year:
effort <- read_csv(here::here(nethrspath), 
                   col_types = cols_only(location = col_factor(), 
                                         date = col_date(format = '%m/%d/%Y'), 
                                         nethours = col_number())) %>% 
  filter(location == 'PN') %>% 
  mutate(year = format(date, '%Y')) %>% 
  filter(year >= 1979) %>% #cleaner data?
  # total by year:
  group_by(year) %>% 
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
  filter(loc == 'PN' & spec %in% spplist & year >= 1979) %>% 
  # count total captures for each species by year
  group_by(spec, year) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(spec = as.factor(as.character(spec))) %>% 
  complete(spec, year, fill = list(n = 0))

# calculate capture rate per 1000 net hours, and fit smoothed line
dat <- left_join(captures, effort, by = 'year') %>% 
  mutate(rate = n / nethours * 1000,
         year = as.numeric(year),
         # spec = factor(spec, levels = c('WREN', 'SWTH', 'WIWA', 'HETH', 'GCSP')),
         # speclab = recode(spec, 
         #                  GCSP = 'Golden-crowned Sparrow',
         #                  WIWA = "Wilson's Warbler",
         #                  SWTH = "Swainson's Thrush",
         #                  HETH = "Hermit Thrush",
         #                  WREN = 'Wrentit')
         ) %>% 
  select(spec, year, rate)




# CREATE PLOTS---------------------------------------------------------------

# residents
map(spplist[1:5], function(x) {
  dat %>% filter(spec == x) %>% 
    ggplot(aes(year, rate)) + 
    geom_smooth(method = 'gam', method.args = list(family = 'quasipoisson'),
                formula = y ~ s(x, k = 15), se = FALSE,
                size = 1.5, color = pointblue.palette[2]) +
    geom_line(color = pointblue.palette[6]) + 
    geom_point(size = 2, color = pointblue.palette[4]) +
    labs(x = NULL, y = 'Capture rate (per 1000 net hours)') +
    scale_x_continuous(breaks = seq(1980, max(dat$year), 5)) +
    ylim(0, 15) +
    theme_classic() 
  ggsave(paste0('fig/', x, '.jpg'), 
         width = 5, height = 3.5, units = 'in', dpi = 150)
})

# migrants
map(spplist[6:10], function(x) {
  dat %>% filter(spec == x) %>% 
    ggplot(aes(year, rate)) + 
    geom_smooth(method = 'gam', method.args = list(family = 'quasipoisson'),
                formula = y ~ s(x, k = 15), se = FALSE,
                size = 1.5, color = pointblue.palette[2]) +
    geom_line(color = pointblue.palette[6]) + 
    geom_point(size = 2, color = pointblue.palette[4]) +
    labs(x = NULL, y = 'Capture rate (per 1000 net hours)') +
    scale_x_continuous(breaks = seq(1980, max(dat$year), 5)) +
    ylim(0, 25) +
    theme_classic() 
  ggsave(paste0('fig/', x, '.jpg'), 
         width = 5, height = 3.5, units = 'in', dpi = 150)
})
