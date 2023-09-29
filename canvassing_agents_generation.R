# ---------- generate canvasser and resident data ---------- 

# libraries
library(tidyverse)
library(ids)
library(truncnorm)


# hyperparameters -----------------------------

# define number of canvassers
# number of residents 
# time before election
# time budgets for canvassers

# number of canvassers
c <- 20

# number of residents
n <- 5000

# days before election
days <- 10

# random draws from poisson dist for time budgets
# mean/var set to 4
h <- days*rpois(n = c, lambda = 4)




# canvassers ------------------------------------

# generate unique ids
cid <- random_id(n = c, bytes = 5)

# ethnic group
eths <- c("native", "foreign")
c_ethnic <- sample(eths, size = c, replace = TRUE, prob = c(0.8, 0.2))

# ideology
# measure as continuous var between 0 and 1
# could collapse onto binary categories, but would lose variation within groups
# since dealing with liberal politician, assume a mean ideology of 0.35
c_views <- rtruncnorm(n = c, a = 0, b = 0.5, mean = 0.35, sd = 0.5)

# put canvasser info into df
c_df <- 
  cbind(cid, c_ethnic, c_views) %>%
  as.data.frame() %>%
  mutate(c_views = as.numeric(c_views))




# residents -------------------------------------

# generate resident ids
rid <- random_id(n = n, bytes = 5)

# ethnicity
r_ethnic <- sample(x = eths, size = n, replace = TRUE, prob = c(0.8, 0.2))

# ideology
# measure as slightly conservative with mean of 0.6
r_views <- rtruncnorm(n = n, a = 0, b = 1, mean = 0.6, sd = 0.15)

# make resident df
r_df <- 
  cbind(rid, r_ethnic, r_views) %>%
  as.data.frame() %>%
  mutate(r_views = as.numeric(r_views))





# politicians ---------------------------------

# generate characteristics for hypothetical liberal and conservative politicians

# conservative
cons_ideo <- 0.7
cons_eth <- c("foreign")

# liberal
lib_ideo <- 0.3
lib_eth <- c("native")




# merge -----------------------------------------

# create unique canvasser-resident pair for every resident
full_df <-
  expand.grid(r_df$rid, c_df$cid) %>%
  rename(rid = 1, cid = 2) %>%
  left_join(r_df, by = c("rid")) %>%
  left_join(c_df, by = c("cid"))

# create new characteristics that measure pol diffs between people
# canvasser-resident
# resident-politician
full_df <-
  full_df %>%
  mutate(cons_eth = cons_eth,
         cons_ideo = cons_ideo,
         lib_eth = lib_eth,
         lib_ideo = lib_ideo,
         ethdifflib = ifelse(r_ethnic != lib_eth, 1, 0),
         ethdiffcon = ifelse(r_ethnic != cons_eth, 1, 0),
         ethmatch_canvlib = ifelse(r_ethnic == c_ethnic, 1, 0),
         xdiffcon = cons_ideo-r_views,
         xdifflib = lib_ideo-r_views,
         xdiffconsq = xdiffcon^2,
         xdifflibsq = xdifflib^2,
         ethdifflibsq = ethdifflib^2,
         ethdiffconsq = ethdiffcon^2)








# end script