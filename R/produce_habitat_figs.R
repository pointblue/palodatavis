
# READ ME -----------------------------------------------------------------

# produce figures for the habitat & community change page

# PACKAGES
library(tidyverse)
library(foreign)
library(plotly)

# INPUTS
vegpath <- "rawdat/treestrans_compiled.dbf"
terrpath <- 'Z:/Terrestrial/programs_and_projects/palomarin/Palodata/territories/palo_breeding_territories.csv'

# local copy:
# terrpath <- "rawdat/palo_breeding_territories.csv"

# OUTPUTS
fig3 <- 'territory_trends_focal.html'


# VEGETATION HEIGHT TRENDS ------------------------------------------------

read.dbf(here::here(vegpath)) %>% 
  group_by(YEAR) %>% 
  summarize(n = length(HEIGHT_M),
            height = mean(HEIGHT_M),
            se = sd(HEIGHT_M)/sqrt(n))



# TERRITORIES OVER TIME ---------------------------------------------------

read_csv(here::here(terrpath), col_types = cols()) %>% 
  select(YEAR:nwcs) %>% 
  pivot_longer(-YEAR) %>% 
  ggplot(aes(name, value)) + geom_col() + facet_wrap(~YEAR)



# FOCAL SPECIES TERRITORIES -----------------------------------------------

plot_focal <- read_csv(terrpath, col_types = cols()) %>% 
  select(year = YEAR, wiwa, sosp, wren, nwcs) %>% 
  pivot_longer(-year, values_to = 'territories', names_to = 'species') %>% 
  mutate(species = recode(species,
                          nwcs = "Nuttall's White-crowned Sparrow",
                          sosp = "Song Sparrow",
                          wiwa = "Wilson's Warbler",
                          wren = "Wrentit")) %>% 
  ggplot(aes(year, territories, color = species)) + geom_line() + geom_point() +
  ylab('Number of territories') + xlab(NULL) + theme_classic()



# plot1 <- plot_ly(x = ~Year) %>%
#   add_trace(data = dat %>% filter(species == 'GRSP'), 
#             y = ~Estimate, 
#             type = 'scatter', 
#             mode = 'lines+markers',
#             error_y = list(type = 'data',
#                            symmetric = FALSE,
#                            arrayminus = ~Estimate-lcl,
#                            array = ~ucl-Estimate,
#                            color = pointblue.palette[2]),
#             line = list(color = pointblue.palette[2]),
#             marker = list(color = pointblue.palette[2], 
#                           size = 10),
#             text = ~text,
#             hoverinfo = 'x+text', 
#             name = 'Grasshopper Sparrow') %>%
#   add_trace(data = dat %>% filter(species == 'SAVS'), 
#             y = ~Estimate, 
#             type = 'scatter', 
#             mode = 'lines+markers',
#             error_y = list(type = 'data',
#                            symmetric = FALSE,
#                            arrayminus = ~Estimate-lcl,
#                            array = ~ucl-Estimate,
#                            color = pointblue.palette[3]),
#             line = list(color = pointblue.palette[3]),
#             marker = list(color = pointblue.palette[3], 
#                           size = 10),
#             text = ~text,
#             hoverinfo = 'x+text', 
#             name = 'Savannah Sparrow') %>%
#   layout(yaxis = list(title = 'Density (birds/10 acres)',
#                       font = list(size = 14),
#                       showline = TRUE,
#                       ticks = 'outside',
#                       tick0 = 0,
#                       range = c(0,10),
#                       showgrid = FALSE,
#                       automargin = TRUE),
#          xaxis = list(title = NA,
#                       showline = TRUE,
#                       ticks = 'outside',
#                       showgrid = FALSE),
#          legend = list(x = 0.01, xanchor = 'left', y = 1, yanchor = 'top',
#                        bordercolor = ~I('black'), borderwidth = 1),
#          hovermode = 'x',
#          margin = list(r = 0, b = 10, t = 10)) %>%
#   config(displaylogo = FALSE, showTips = FALSE,
#          modeBarButtonsToRemove = list('zoom2d', 'select2d', 'lasso2d', 
#                                        'zoomIn2d', 'zoomOut2d', 
#                                        'pan2d', 'toggleSpikelines'))

plotly_focal <- ggplotly(plot_focal)

htmlwidgets::saveWidget(plotly_focal,
                        here::here(fig3),
                        selfcontained = TRUE,
                        title = 'Territory trends')
