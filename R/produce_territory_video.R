
# READ ME -----------------------------------------------------------------

# produce animated figure for the habitat & community change page

# PACKAGES
library(tidyverse)
library(gganimate) # also requires "av" package for making mp4 (or "gifski" for a gif)
# options(gganimate.nframes = 20)

# INPUTS
# terrpath <- 'Z:/Terrestrial/programs_and_projects/palomarin/Palodata/territories/palo_breeding_territories.csv'

# local copy:
terrpath <- "rawdat/palo_breeding_territories.csv"

# OUTPUTS
video <- 'images/habitat/territory_trends_animated.gif'

pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666')

# TERRITORIES OVER TIME ---------------------------------------------------

terrdat_ranked <- read_csv(terrpath, col_types = cols()) %>% 
  select(year = YEAR, caqu:nwcs) %>% 
  pivot_longer(-year, values_to = 'territories', names_to = 'species') %>% 
  arrange(year, -territories, species) %>%  
  # assign ranking by year
  group_by(year) %>% 
  mutate(rank = 1:n()) %>% 
  ungroup()

# list of species that have ever cracked the top 5
top_species <- terrdat_ranked %>% filter(rank <= 5) %>% 
  pull(species) %>% unique()

terrdat_full <- terrdat_ranked %>% filter(species %in% top_species) %>% 
  complete(year, species, fill = list(territories = 0, rank = 999)) %>% 
  arrange(year, -territories, species) %>% 
  #reassign ranking by year
  group_by(year) %>% 
  mutate(rank = 1:n()) %>% 
  ungroup() %>% 
  mutate(species = recode(species,
                          sosp = "Song Sparrow",
                          wren = "Wrentit",
                          nwcs = "Nuttall's White-crowned Sparrow",
                          alhu = "Allen's Hummingbird",
                          spto = "Spotted Towhee",
                          bewr = "Bewick's Wren",
                          amgo = "American Goldfinch",
                          ocwa = "Orange-crowned Warbler",
                          # casj = "California Scrub-Jay",
                          # hofi = "House Finch",
                          # bush = "Bushtit",
                          # amro = "American Robin",
                          caqu = "California Quail",
                          anhu = "Anna's Hummingbird",
                          # pufi = "Purple Finch",
                          # bhco = "Brown-headed Cowbird",
                          wiwa = "Wilson's Warbler",
                          cbch = "Chestnut-backed Chickadee",
                          swth = "Swainson's Thrush",
                          pawr = "Pacific Wren",
                          gcki = "Golden-crowned Kinglet",
                          # huvi = "Hutton's Vireo"
                          ))

  # filter(rank <= 10) %>% 
  # mutate(habitat = case_when(species %in% c('nwcs', 'wiwa', 'wren',
  #                                           'bush', 'spto', 'casj', 
  #                                           'caqu') ~ 'scrub',
  #                            species %in% c('sosp', 'alhu', 'amgo', 
  #                                           'amro', 'anhu', 'bewr') ~ 'open woodland',
  #                            species %in% c('cbch', 'ocwa', 'swth', 
  #                                           'pufi', 'gcki', 'huvi', 
  #                                           'pawr') ~ 'forest',
  #                            species %in% c('bhco','hofi') ~ 'other'))
  
plot_terr <- terrdat_full %>%
  # filter(rank <= 15) %>% 
  # filter(year == 1990) %>% 
  ggplot(aes(x = -rank, y = territories, group = species)) +
  geom_tile(aes(y = territories/2, height = territories, fill = species), 
            width = 0.9) +
  geom_text(aes(label = species), hjust = 'left', nudge_y = 1, size = 3,
            color = 'black', fontface = 'bold') +
  geom_text(aes(label = scales::comma(territories)), hjust = 'right', 
            nudge_y = -1, size = 3, color = 'black') +
  # geom_text(x = 70 , y = -10, aes(label = as.character(year)),
  #           size = 18, col = "gray30") +
  coord_flip(clip = 'off') +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL) +
  scale_fill_viridis_d(option = 'D', alpha = 0.7) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none',
        # plot.margin = margin(1, 1, 1, 1, "cm"),
        axis.text.y = element_blank()) +
  # animate:
  transition_time(year) +
  ease_aes('quartic-in-out') +
  labs(title = 'Breeding Bird Territories at Palomarin in {round(frame_time, 0)}')

animate(plot_terr, nframes = 360, fps = 6, end_pause = 30, 
        height = 900, width = 1200, res = 220)
anim_save(filename = video)

# plot_terr <- terrdat_ranked %>% 
#   ggplot(aes(y = rank, xmin = 0, xmax = territories, 
#              ymin = rank - .45, ymax = rank + .45, fill = habitat)) +
#   facet_wrap(~ year) +  
#   geom_rect(alpha = .9) +
#   scale_x_continuous(limits = c(-50, 90),  breaks = c(0, 25, 50, 75)) +  
#   geom_text(col = "gray13",
#             hjust = "right",
#             aes(label = species),
#             x = -1, size = 5) +
#   scale_fill_manual(values = c(pointblue.palette[c(2, 1, 6, 3)])) +
#   scale_y_reverse() +
#   labs(x = 'Territories', y = '', fill = NULL) + 
#   theme_classic() +
#   theme(legend.position = 'right',
#         legend.text = element_text(size = 14),
#         axis.text.x = element_text(size = 14),
#         axis.title.x = element_text(size = 16),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.line.y = element_blank())
# 
# # make all facets overlap & add year label
# plot_terr_gif <- plot_terr +  
#   facet_null() +  
#   geom_text(x = 60 , y = -10,
#             aes(label = as.character(year)),
#             size = 22, col = "grey18") +
#   aes(group = species) +  
#   transition_states(year, state_length = 2)
# 
# animate(plot_terr_gif + ease_aes('quadratic-in-out'), 
#         nframes = 250, fps = 5, end_pause = 8,
#         height = 1000, width = 1600, res = 150,
#         renderer = av_renderer())
# anim_save(filename = video)
