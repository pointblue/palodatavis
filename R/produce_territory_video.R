
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
video <- 'images/territory_trends_animated.mp4'

pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666')

# TERRITORIES OVER TIME ---------------------------------------------------

terrdat_ranked <- read_csv(terrpath, col_types = cols()) %>% 
  select(year = YEAR, caqu:nwcs) %>% 
  pivot_longer(-year, values_to = 'territories', names_to = 'species') %>% 
  arrange(year, -territories, species) %>%  
  filter(territories >= 2) %>% 
  # assign ranking by year
  group_by(year) %>% 
  mutate(rank = 1:n()) %>% 
  filter(rank <= 10) %>% 
  mutate(habitat = case_when(species %in% c('nwcs', 'wiwa', 'wren',
                                            'bush', 'spto', 'casj', 
                                            'caqu') ~ 'scrub',
                             species %in% c('sosp', 'alhu', 'amgo', 
                                            'amro', 'anhu', 'bewr') ~ 'open woodland',
                             species %in% c('cbch', 'ocwa', 'swth', 
                                            'pufi', 'gcki', 'huvi', 
                                            'pawr') ~ 'forest',
                             species %in% c('bhco','hofi') ~ 'other'),
         species = recode(species,
                          sosp = "Song Sparrow",
                          wren = "Wrentit",
                          wiwa = "Wilson's Warbler",
                          spto = "Spotted Towhee",
                          bewr = "Bewick's Wren",
                          nwcs = "Nuttall's White-crowned Sparrow",
                          alhu = "Allen's Hummingbird",
                          amgo = "American Goldfinch",
                          caqu = "California Quail",
                          ocwa = "Orange-crowned Warbler",
                          swth = "Swainson's Thrush",
                          pufi = "Purple Finch",
                          anhu = "Anna's Hummingbird",
                          bush = "Bushtit",
                          casj = "California Scrub-Jay",
                          cbch = "Chestnut-backed Chickadee",
                          gcki = "Golden-crowned Kinglet",
                          hofi = "House Finch",
                          huvi = "Hutton's Vireo",
                          pawr = "Pacific Wren",
                          amro = "American Robin",
                          bhco = "Brown-headed Cowbird"))
  
plot_terr <- terrdat_ranked %>% 
  ggplot(aes(y = rank, xmin = 0, xmax = territories, 
             ymin = rank - .45, ymax = rank + .45, fill = habitat)) +
  facet_wrap(~ year) +  
  geom_rect(alpha = .9) +
  scale_x_continuous(limits = c(-50, 90),  breaks = c(0, 25, 50, 75)) +  
  geom_text(col = "gray13",
            hjust = "right",
            aes(label = species),
            x = -1, size = 5) +
  scale_fill_manual(values = c(pointblue.palette[c(2, 1, 6, 3)])) +
  scale_y_reverse() +
  labs(x = 'Territories', y = '', fill = NULL) + 
  theme_classic() +
  theme(legend.position = 'right',
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

# make all facets overlap & add year label
plot_terr_gif <- plot_terr +  
  facet_null() +  
  geom_text(x = 60 , y = -10,
            aes(label = as.character(year)),
            size = 22, col = "grey18") +
  aes(group = species) +  
  transition_states(year, state_length = 2)

animate(plot_terr_gif + ease_aes('quadratic-in-out'), 
        nframes = 250, fps = 5, end_pause = 8,
        height = 1000, width = 1600, res = 150,
        renderer = av_renderer())
anim_save(filename = video)
