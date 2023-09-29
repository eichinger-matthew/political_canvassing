library(tidyverse)
library(ggpubr)

# figures and tables
setwd("C:/Users/eichi/Desktop/political_canvassing")
# load data lists
load("canvassing_results_fast_multiperson.RData")
load("canvassing_results_random_assignment.RData")
load("canvassing_results_slow_singleperson.RData")


# gains -----

# get fast store results
fast_gains <- list()
slow_gains <- list()
rand_gains <- list()
for(i in 1:length(fast_store)){
  temp <- fast_store[[i]]$canvasser_gains %>% mutate(run = i)
  fast_gains[[i]] <- temp
  bemp <- slow_store[[i]]$canvasser_gains %>% mutate(run = i)
  slow_gains[[i]] <- bemp
  cemp <- random_store[[i]]$canvasser_gains %>% mutate(run = i)
  rand_gains[[i]] <- cemp
}

# rbind
fast <- do.call(rbind, fast_gains) %>% mutate(type = 'fast')
slow <- do.call(rbind, slow_gains) %>% mutate(type = 'slow')
rand <- do.call(rbind, rand_gains) %>% mutate(type = 'random')
# combine
df <- rbind(fast, slow, rand)

# gains per run, by group
gains_per_run <-
  df %>%
  group_by(type, run) %>%
  summarise(total_gain = sum(total_gains))

mean(gains_per_run$total_gain[gains_per_run$type == "fast"])
mean(gains_per_run$total_gain[gains_per_run$type == "slow"])
# gains per run
gains_per_run %>%
  filter(type != "random") %>%
  ggplot(aes(x = total_gain, group = type, linetype = type)) +
  geom_density()+
  scale_linetype_manual(name = "Method", values = c(1,2)) +
  labs(x = "Number of expected votes gained", y = "Density",
       title = "Performance Comparison Between Fast and Slow Assignment Methods",
       subtitle = "By total expected gain in votes, across 10 runs") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))

# avg gains
table_gains_by_method <-
  df %>%
  group_by(cid, type) %>%
  summarise(total_gain = sum(total_gains)) %>%
  pivot_wider(names_from = 'type', values_from = 'total_gain') %>%
  mutate(slow_adv = slow - fast) %>%
  arrange(desc(slow_adv))
  
  

# paths -----

# use the fifth run
fast_path <- fast_store[[5]]$path_map
slow_path <- slow_store[[5]]$path_map
rand_path <- random_store[[5]]$path_map

# show
fast_path +
  labs(x = "x coordinate", y = "y coordinate",
       title = "Path Trajectores",
       subtitle = "Fast mutliperson assignment")
# show
slow_path +
  labs(x = "x coordinate", y = "y coordinate",
       title = "Path Trajectores",
       subtitle = "Slow singleperson assignment")
# show
rand_path +
  labs(x = "x coordinate", y = "y coordinate",
       title = "Path Trajectores",
       subtitle = "Random trajectory assignment")


  
  
  
  
  
  
  
