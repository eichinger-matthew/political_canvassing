library(ids)
library(truncnorm)
library(tidyverse)
library(arm)
library(RColorBrewer)

setwd("C:/Users/eichi/Desktop/political_canvassing")

select <- dplyr::select

# method ----

# number of canvassers
C <- 40
# number of residents
R <- 20000
# days before election
days <- 10
# unique time budgets for canvassers
h <- days*rpois(n = C, lambda = 20)
# canvasser ids
cid <- random_id(n = C, bytes = 5)
# resident ids
rid <- random_id(n = R, bytes = 6)

# write function to simulate neighborhood blocs on a 2d plane
sim_blocs <- function(num_blocs, upper_lim, lower_lim, num_res){
  # blocs is number of blocs
  # upper and lower lim are boundaries on the plane
  x <- runif(n = num_blocs, min = lower_lim, max = upper_lim); x <- rep(x, each = num_res)
  y <- runif(n = num_blocs, min = lower_lim, max = upper_lim); y <- rep(y, each = num_res)
  bloc <- seq(1, num_blocs, 1); bloc <- rep(bloc, each = num_res)
  # draw offsets
  xoff <- rnorm(n = num_blocs*num_res, 0, 1)
  yoff <- rnorm(n = num_blocs*num_res, 0, 1)
  # combine into df
  a <- data.frame(bloc, x, y, xoff, yoff, xpos = x + xoff, ypos = y + yoff)
  # return dataframe
  return(a)
}
# run
G <- sim_blocs(num_blocs = 50, upper_lim = 60, lower_lim = -60, num_res = 400)
# add resident ids
G$rid <- rid
G_blocs <- 
  G %>%
  select(bloc, x, y) %>%
  distinct()
# subset data to include only xy info
rid_spots <- select(G, rid, xpos, ypos)
# plot geography
plot_residents <-
  G %>%
  ggplot() +
  geom_point(aes(x = xpos, y = ypos)) +
  geom_point(aes(x = x, y = y), col = "red") +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  lims(x = c(-60, 60), y = c(-60, 60)) +
  labs(title = "Geographic Distribution of Constituents",
       subtitles = "Red dots indicate neighborhood anchors",
       x = "x coorindate", y = "y coordinate") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))



# people info --------------------------------

# ethnicity
c_ethnic <- rbinom(n = C, size = 1, prob = 0.2)

# ideology
c_views <- rtruncnorm(n = C, a = 0, b = 0.5, mean = 0.35, sd = 0.5)

# dataframe
c_df <- 
  cbind(cid, c_ethnic, c_views) %>%
  as.data.frame() %>%
  mutate(c_ethnic = as.numeric(c_ethnic),
         c_views = as.numeric(c_views))

# ethnicity
r_ethnic <- rbinom(n = R, size = 1, prob = 0.2)

# ideo prefs
r_views <- rtruncnorm(n = R, a = 0, b = 1, mean = 0.6, sd = 0.15)

# make resident df
r_df <- 
  cbind(rid, r_ethnic, r_views) %>%
  as.data.frame() %>%
  mutate(r_ethnic = as.numeric(r_ethnic),
         r_views = as.numeric(r_views))

# politicians
# conservative
cons_ideo <- 0.7
cons_eth <- 1
# liberal
lib_ideo <- 0.3
lib_eth <- 0

# data description --------

# get full data
full_df <- 
  expand.grid(r_df$rid, c_df$cid) %>%
  rename(rid = 1, cid = 2) %>%
  left_join(r_df, by = c("rid")) %>%
  left_join(c_df, by = c("cid")) %>%
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

# logistic regression model
log_reg <- function(xdifflibsq, xdiffconsq, ethdifflibsq, 
                    ethdiffconsq, Va, Vb, ethmatch_canvlib, 
                    ethmatch_canvcon){
  # define linear predictor
  z <- 0.1 + 2*(xdiffconsq - xdifflibsq) + 
    0.7*(ethdiffconsq - ethdifflibsq) +
    0.3*(Va-Vb) + 0.2*(Va*ethmatch_canvlib - Vb*ethmatch_canvcon)
  #  return linear predictor
  return(z)
}

# calculate predicted probs
full_df_withpprobs <-
  full_df %>%
  ungroup() %>%
  mutate(
    linpred_visitbyA = log_reg(
      xdifflibsq = xdifflibsq, xdiffconsq = xdiffconsq,
      ethdifflibsq = ethdifflibsq, ethdiffconsq = ethdiffconsq,
      Va = 1, Vb = 0, 
      ethmatch_canvlib = ethmatch_canvlib,
      ethmatch_canvcon = 0),
    linpred_neithervisit = log_reg(
      xdifflibsq = xdifflibsq, xdiffconsq = xdiffconsq,
      ethdifflibsq = ethdifflibsq, ethdiffconsq = ethdiffconsq,
      Va = 0, Vb = 0, 
      ethmatch_canvlib = ethmatch_canvlib,
      ethmatch_canvcon = 0),
    pprob_visitbyA = invlogit(x = linpred_visitbyA),
    pprob_neithervisit = invlogit(x = linpred_neithervisit),
    voteA_with_visit = 
      ifelse(pprob_visitbyA >= 0.5, 1, 0),
    voteA_without_visit = 
      ifelse(pprob_neithervisit >= 0.5, 1, 0),
    exp_votegain = 
      pprob_visitbyA-pprob_neithervisit) %>%
  rowwise() %>%
  mutate(actual_vote_visitA = 
           rbinom(n = 1, size = 1, prob = pprob_visitbyA),
         actual_vote_neithervisit = 
           rbinom(n = 1, size = 1, prob = pprob_neithervisit)) %>%
  ungroup()

# join neighbor data to df
pairs_df <-
  full_df_withpprobs %>%
  left_join(G, by = c("rid" = "rid")) %>%
  rename(pot_rid = rid)

# starting info
S <- 
  pairs_df %>%
  select(pot_rid, cid, exp_votegain, xpos, ypos) %>%
  rename(rid = pot_rid)

# how you want the matching to go
S %>%
  left_join(trackers, 'cid') %>%
  left_join(rid_spots, by = c('rid.now' = 'rid'), suffix = c("", ".now")) %>%
  mutate(tdistance = trav_dist(xpos, xpos.now, ypos, ypos.now))


# functions to be used -------------------------------------------

# "source" the functions from the algorithm script
source("algorithm_functions.R")


# fast multi-person assignment algorithm ------------------------------------------

# loop to get diagnostics over runs
nruns <- 10
fast_store <- list()

# start for-loop
for(q in 1:nruns){

# ---------------- preamble

# path history list
cid_hist <- list()
# for loop to start the list
for(i in unique(cid)){
  # make empty df
  temp <- 
    tibble(path_hist = character(),
           gains_hist = numeric(),
           budget_hist = numeric())
  # append to list
  cid_hist[[i]] <- temp
}

# create tracking list to store present locations
# populate with random sample of resident ids
trackers <-
  tibble(cid = cid, rid.now = NA, budget = h, moved_last = 0) %>%
  mutate(rid.now = sample(unique(rid), size = C, replace = FALSE))
# save initial spots
initial_spots <- trackers$rid.now
# create visited list
been_vis <-
  tibble(rid,  visited = 0) %>%
  mutate(visited = ifelse(rid %in% trackers$rid.now, 1, visited))



# --------------------------------- start while loop

# while time budgets are still positive
# while residents can still be visited
while(nrow(been_vis) > sum(been_vis$visited) | all(trackers$budget <= 0) == TRUE){
  
  # calculate ranks
  visit_ranks <- arrange_ranks(df = S, trackers = trackers, 
                               rid_spots = rid_spots, been_vis = been_vis)
  # filter to best ranks
  best_ranks <- optimal_ranks(df = visit_ranks, multi = TRUE)
  # resolve any matching issues
  fin_matches <- handle_conflicts(df = best_ranks)
  
  # if match winners
  if(nrow(fin_matches) > 0){
    # update counters for winners
    for(i in unique(fin_matches$cid)){
      # if time budget still positive...
      if(trackers$budget[trackers$cid == i] > 0){
        # then update path histories
        cid_hist[[i]] <-
          cid_hist[[i]] %>%
          add_row(path_hist = as.character(fin_matches$rid[fin_matches$cid == i]),  
                  gains_hist = fin_matches$penalized_gain[fin_matches$cid == i],
                  budget_hist = fin_matches$tdistance[fin_matches$cid == i])
        
        # then updater trackers with new positions
        trackers$rid.now[trackers$cid == i] <- fin_matches$rid[fin_matches$cid == i]
        trackers$budget[trackers$cid == i] <- 
          trackers$budget[trackers$cid == i]-
          fin_matches$tdistance[fin_matches$cid == i]
        
        # update visited
        been_vis$visited[been_vis$rid == trackers$rid.now[trackers$cid == i]] <- 1
      }
    }
    
    # identify canvassers with zero time
    exhausted <-
      visit_ranks %>%
      group_by(cid) %>%
      summarise(num_poss = sum(tpossible)) %>%
      filter(num_poss == 0) %>%
      pull(cid)
    
    # filter canvassers with zero time
    trackers <-
      trackers %>%
      filter(!cid %in% exhausted)
    # print current budget
    print(trackers$budget)
  } else{
    break
  }
} # end while-loop

# get results
res <- canvassing_postestimation(cid_hist)
fast_store[[q]] <- res
}
# save results
save(fast_store, file = "canvassing_results_fast_multiperson.RData")






# slow one-person assignment algorithm ------------------------------------------

# same thing as above, but going canvasser by canvasser
# can use the multi = 0 specification
nruns <- 10
slow_store <- list()

# loop
for(q in 1:nruns){

# ---------------- preabmle

# path history list
cid_hist.single <- list()
# for loop to start the list
for(i in unique(cid)){
  # make empty df
  temp <- 
    tibble(path_hist = character(),
           gains_hist = numeric(),
           budget_hist = numeric())
  # append to list
  cid_hist.single[[i]] <- temp
}

# create tracking list to store present locations
# populate with random sample of resident ids
trackers.single <-
  tibble(cid = cid, rid.now = NA, budget = h, moved_last = 0) %>%
  mutate(rid.now = sample(unique(rid), size = C, replace = FALSE))
# save initial spots
initial_spots.single <- trackers.single$rid.now
# create visited list
been_vis.single <-
  tibble(rid,  visited = 0) %>%
  mutate(visited = ifelse(rid %in% trackers.single$rid.now, 1, visited))



# ------------------------------- start while loop

# while time budgets are still positive
# while residents can still be visited
while(nrow(been_vis.single) > sum(been_vis.single$visited) | all(trackers.single$budget <= 0) == TRUE){
  
  # calculate ranks
  visit_ranks <- arrange_ranks(df = S, trackers = trackers.single,
                               rid_spots = rid_spots, been_vis = been_vis.single)
  # filter to best ranks
  fin_matches <- optimal_ranks(df = visit_ranks, multi = FALSE)
  #fin_matches <- handle_conflicts(df = best_ranks)
  
  # if match winners
  if(nrow(fin_matches) > 0){
    # update counters for winners
    for(i in unique(fin_matches$cid)){
      # if time budget still positive...
      if(trackers.single$budget[trackers.single$cid == i] > 0){
        # then update path histories
        cid_hist.single[[i]] <-
          cid_hist.single[[i]] %>%
          add_row(path_hist = as.character(fin_matches$rid[fin_matches$cid == i]),  
                  gains_hist = fin_matches$penalized_gain[fin_matches$cid == i],
                  budget_hist = fin_matches$tdistance[fin_matches$cid == i])
        
        # then updater trackers with new positions
        trackers.single$rid.now[trackers.single$cid == i] <- fin_matches$rid[fin_matches$cid == i]
        trackers.single$budget[trackers.single$cid == i] <- 
          trackers.single$budget[trackers.single$cid == i]-
          fin_matches$tdistance[fin_matches$cid == i]
        
        # update visited
        been_vis.single$visited[been_vis.single$rid == trackers.single$rid.now[trackers.single$cid == i]] <- 1
      }
    }
    
    # identify canvassers with zero time
    exhausted <-
      visit_ranks %>%
      group_by(cid) %>%
      summarise(num_poss = sum(tpossible)) %>%
      filter(num_poss == 0) %>%
      pull(cid)
    
    # filter canvassers with zero time
    trackers.single <-
      trackers.single %>%
      filter(!cid %in% exhausted)
    # print current budget
    print(trackers.single$budget)
  } else{
    break
  }
} # end while-loop

# get results
res <- canvassing_postestimation(cid_hist.single)
slow_store[[q]] <- res
}

# save
save(slow_store, file = "canvassing_results_slow_singleperson.RData")


# random assignment method ------------------------

nruns <- 10
random_store <- list()
for(q in 1:nruns){

# ---------------- preabmle

# path history list
cid_hist.rand <- list()
# for loop to start the list
for(i in unique(cid)){
  # make empty df
  temp <- 
    tibble(path_hist = character(),
           gains_hist = numeric(),
           budget_hist = numeric())
  # append to list
  cid_hist.rand[[i]] <- temp
}

# create tracking list to store present locations
# populate with random sample of resident ids
trackers.rand <-
  tibble(cid = cid, rid.now = NA, budget = h, moved_last = 0) %>%
  mutate(rid.now = sample(unique(rid), size = C, replace = FALSE))
# save initial spots
initial_spots.rand <- trackers.rand$rid.now
# create visited list
been_vis.rand <-
  tibble(rid, visited = 0) %>%
  mutate(visited = ifelse(rid %in% trackers.rand$rid.now, 1, visited))

# ----- while loop

# while time budgets are still positive
# while residents can still be visited
while(nrow(been_vis.rand) > sum(been_vis.rand$visited) | isTRUE(all(trackers.rand$budget <= 0))){
  
  # get possible visits
  ranks <- arrange_ranks(df = S, trackers = trackers.rand, 
                         rid_spots = rid_spots, been_vis = been_vis.rand)
  
  # for each person, randomly sample
  random_visits <-
    ranks %>%
    group_by(cid) %>%
    slice_sample(n = 1, replace = FALSE)
  
  # if match winners
  if(nrow(random_visits) > 0){
    # update counters for winners
    for(i in unique(random_visits$cid)){
      # if time budget still positive...
      if(trackers.rand$budget[trackers.rand$cid == i] > 0){
        # then update path histories
        cid_hist.rand[[i]] <-
          cid_hist.rand[[i]] %>%
          add_row(path_hist = as.character(random_visits$rid[random_visits$cid == i]),  
                  gains_hist = random_visits$penalized_gain[random_visits$cid == i],
                  budget_hist = random_visits$tdistance[random_visits$cid == i])
        
        # then updater trackers with new positions
        trackers.rand$rid.now[trackers.rand$cid == i] <- random_visits$rid[random_visits$cid == i]
        trackers.rand$budget[trackers.rand$cid == i] <- 
          trackers.rand$budget[trackers.rand$cid == i]-
          random_visits$tdistance[random_visits$cid == i]
        
        # update visited
        been_vis.rand$visited[been_vis.rand$rid == trackers.rand$rid.now[trackers.rand$cid == i]] <- 1
      }
    }
    
    # identify canvassers with zero time
    exhausted <-
      ranks %>%
      left_join(trackers.rand, by = c("cid")) %>%
      group_by(cid) %>%
      summarise(num_poss = sum(tpossible)) %>%
      filter(num_poss == 0) %>%
      pull(cid)
    
    # filter canvassers with zero time
    trackers.rand <-
      trackers.rand %>%
      filter(!cid %in% exhausted)
    # print current budget
    print(trackers.rand$budget)
  } else{
    break
  }
} # end while-loop

# postestimation
res <- canvassing_postestimation(cid_hist.rand)
random_store[[q]] <- res
}

# get results
save(random_store, file = "canvassing_results_random_assignment.RData")














