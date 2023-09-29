# ---------- optimal canvassing program

rm(list = ls())

# Load libraries
library(Matrix)
library(MASS)
library(truncnorm)
library(tidyverse)
library(ids)
library(arm)
library(janitor)
library(wesanderson)

# prefer select
select <- dplyr::select


# set seed
set.seed(47186)


# HYPERPARAMETERS ---------- 

# number of canvassers
c <- 40
# number of residents
n <- 5000
# days before election
days <- 10
# unique time budgets for canvassers
h <- days*rpois(n = c, lambda = 20)


# CANVASSER, RESIDENTS, AND POLITICIAN CHARACTERISTICS ----------

# ----- politicians

# conservative
cons_ideo <- 0.7
cons_eth <- 1
# liberal
lib_ideo <- 0.3
lib_eth <- 0


# ----- canvassers

# canvasser ids
cid <- random_id(n = c, bytes = 5)
# ethnicity
c_ethnic <- rbinom(n = c, size = 1, prob = 0.2)
# ideology
c_views <- rtruncnorm(n = c, a = 0, b = 0.5, mean = 0.35, sd = 0.5)
# dataframe
c_df <- 
  cbind(cid, c_ethnic, c_views) %>%
  as.data.frame() %>%
  mutate(c_ethnic = as.numeric(c_ethnic),
         c_views = as.numeric(c_views))


# ----- residents

# resident ids
rid <- random_id(n = n, bytes = 5)
# ethnicity
r_ethnic <- rbinom(n = n, size = 1, prob = 0.2)
# ideo prefs
r_views <- rtruncnorm(n = n, a = 0, b = 1, mean = 0.6, sd = 0.15)
# make resident df
r_df <- 
  cbind(rid, r_ethnic, r_views) %>%
  as.data.frame() %>%
  mutate(r_ethnic = as.numeric(r_ethnic),
         r_views = as.numeric(r_views))


# plot
r_df %>%
  ggplot(aes(x = r_views)) +
  geom_histogram(fill = "steelblue") +
  labs(x = "ideology") +
  theme_minimal()

r_df %>%
  ggplot(aes(x = r_ethnic)) +
  geom_bar(fill = "steelblue") +
  labs(x = "ethnicity") +
  theme_minimal()

# ----- combine residents and canvassers

# use expand.grid to make all possible combos
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




# GEOGRAPHY ---------------------------------------

# ----- anchors

# simulate neighborhood anchors
sim_blocs <- function(blocs, upper_lim, lower_lim){
  
  # simulate an x-y pair from city limits
  bloc_locs <- 
    matrix(data = runif(n = 2*blocs, min = lower_lim, max = upper_lim),
           ncol = 2, nrow = blocs)
  
  # assign to a dataframe
  bloc_df <-
    bloc_locs %>%
    as.data.frame() %>%
    rename(x_anchor = 1, y_anchor = 2) %>%
    mutate(bloc_id = seq(1, blocs, 1))
  
  # return
  return(bloc_df)
  
}

# get anchors
bloc_df <- sim_blocs(blocs = 40, upper_lim = 60, lower_lim = -60)

# plot anchors
bloc_df %>%
  ggplot(aes(x = x_anchor, y = y_anchor)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  lims(x = c(-60, 60), y = c(-60, 60)) +
  labs(title = "Distribution of Neighborhood Anchors") +
  theme_minimal()


# ----- neighbors

# define neighbor simulation function
sim_neighbors <- function(df, num_residents, lower_lim, upper_lim){
  
  # draw num_resident values with defined limits
  anchor_offsets <-
    matrix(data = rnorm(n = 2*num_residents, mean = 0, sd = 2),
           nrow = num_residents, ncol = 2) %>%
    as.data.frame() %>%
    rename(x_offset = 1, y_offset = 2)
  
  # combine with df
  df_long <- 
    df %>%
    slice(rep(1:n(), each = num_residents/nrow(df))) %>%
    cbind(anchor_offsets) %>%
    mutate(xspot = x_anchor + x_offset,
           yspot = y_anchor + y_offset)
  
  # get a simplified version
  neighbor_spots <- 
    df_long %>%
    select(bloc_id, xspot, yspot)
  
  # return
  return(neighbor_spots)
  
}

# evaluate
neighbor_spots <- 
  sim_neighbors(df = bloc_df, num_residents = n, lower_lim = -4, upper_lim = 4) %>%
  cbind(rid)

# plot
neighbor_node_plot
neighbor_node_plot <-
  neighbor_spots %>%
  ggplot(aes(x = xspot, y = yspot)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  lims(x = c(-65, 65), y = c(-65, 65)) +
  labs(title = "Distribution of Individual Neighbors") +
  theme_minimal()



# GET PREDICTION MODEL ----------

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
  left_join(neighbor_spots, by = c("rid" = "rid")) %>%
  rename(pot_rid = rid)






# LOOP SECTION BEGINS -------------------- 

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
tracklist <-
  tibble(cid = cid, cid_currpos = NA, 
         budget = h) %>%
  mutate(cid_currpos = 
           sample(unique(rid), size = c, replace = FALSE))

# save initial spots
initial_spots <- tracklist$cid_currpos

# create visited list
been_visited <-
  tibble(pot_rid = rid, 
         visited = 0) %>%
  mutate(visited = ifelse(pot_rid %in% tracklist$cid_currpos, 1, visited))

# compute travel distance given current spot and future spot
trav_dist <- function(cspot, fspot){
  
  # travel distance for x direction
  xdist <-
    sqrt((neighbor_spots$xspot[neighbor_spots$rid == cspot] -
            neighbor_spots$xspot[neighbor_spots$rid == fspot])^2)
  
  # travel distance for y direction
  ydist <-
    sqrt((neighbor_spots$yspot[neighbor_spots$rid == cspot] -
            neighbor_spots$yspot[neighbor_spots$rid == fspot])^2)
  
  # return total distance
  return(xdist + ydist)
  
}

# calculate ranks
arrange_ranks <- function(df){
  
  # get penalized gains
  # filter out current positions
  # filter out visited residents
  # filter canvassers with exhausted budgets
  # define whether a canv-res pair can be traveled
  ranks <-
    df %>%
    left_join(tracklist, by = c("cid")) %>%
    left_join(been_visited, by = c("pot_rid")) %>%
    filter(budget > 0, visited == 0, cid_currpos != pot_rid) %>%
    rowwise() %>%
    mutate(travel_distance = trav_dist(cspot = cid_currpos, fspot = pot_rid)) %>%
    mutate(travel_possible = ifelse(budget > travel_distance, 1, 0)) %>%
    group_by(cid) %>%
    mutate(penalized_gain = exp_votegain/travel_distance)
  
  return(ranks)
  
}

# filter rank to optimal ranks
optimal_ranks <- function(df){

  # get optimal travel spot by canvasser
  optim_ranks <-
    df %>%
    filter(travel_possible == 1) %>%
    group_by(cid) %>%
    mutate(visit_rank = min_rank(desc(penalized_gain))) %>%
    filter(visit_rank == 1) %>%
    ungroup()
  
  return(optim_ranks)
}

# deal with duplicates
handle_conflicts <- function(df){
  # identify matches by resident
  match_conflicts <-
    df %>%
    group_by(pot_rid) %>%
    mutate(num_optims = n()) %>%
    filter(num_optims > 1)
  
  # if there are match conflicts
  if(nrow(match_conflicts) > 0){
    
    # identify losers
    match_losers <- 
      match_conflicts %>%
      group_by(pot_rid) %>%
      filter(penalized_gain != max(penalized_gain, na.rm = TRUE)) %>%
      pull(cid)
    
    # filter losers
    match_winners <-
      df %>%
      filter(!cid %in% match_losers)
  } else{
    # if no match conflicts, continue
    match_winners <- df
  }
  return(match_winners)
}



# ALGORITHMS -----------------------------------

# global best ------------------

# take pairs df and arrange
gbest <- 
  pairs_df %>%
  ungroup() %>%
  arrange(desc(exp_votegain))

# make copy version for manipulating
gbest.copy <- gbest

# make list of gbest matches
l.gbest <- list()
# make unique list for each canvasser id
for(i in unique(gbest.copy$cid)){
  l.gbest[[i]] <- list()
}

# while loop
while(length(unique(gbest.copy$pot_rid)) >= 1){
  
  # assign global best to canvasser
  l.gbest[[gbest.copy$cid[1]]] <- 
    append(l.gbest[[gbest.copy$cid[1]]], gbest.copy$pot_rid[1])
  
  # remove resident from global best df
  gbest.copy <- gbest.copy[!(gbest.copy$pot_rid == gbest.copy$pot_rid[1]),]
}


# adjusted global best --------------

# make position list and randomly assign starting point and hours
counts <- tibble(
  "canvasser" = unique(pairs_df$cid),
  "position" = NA, "time" = NA,
)
# initialize positions
for(i in unique(counts$canvasser)){
  # positions
  counts$position[counts$canvasser == i] <- sample(rid, size = 1)
  # time
  counts$time[counts$canvasser == i] <- rpois(n = 1, lambda = 20)
}

# make path history list
phist <- list()
for(i in unique(pairs_df$cid)){
  phist[[i]] <- list()
}
# make resident visited list
visited <- tibble(r = rid, visit = 0)

# while loop
while(...){
  
  
  
  
}



# neighborhood -------------------------

# assign a single canvasser to every neighborhood
# for each canvasser, calculate how many votes would be gained from a neighborhood
# can be done with pairs dataframe easily
bloc_canv_payoffs <-
  pairs_df %>%
  group_by(cid, bloc_id) %>%
  summarise(totgain = sum(exp_votegain)) %>%
  arrange(desc(bloc_id))


# START LOOP --------------------

# ----- while loop

# while time budgets are still positive
# while residents can still be visited
while(nrow(been_visited) > sum(been_visited$visited) |
      all(tracklist$budget <= 0) == TRUE){
  
  # get calculate distances...
  visit_ranks <- arrange_ranks(df = pairs_df)
  best_ranks <- optimal_ranks(df = visit_ranks)
  
  
  # resolve matching issues
  fin_matches <- handle_conflicts(df = best_ranks)
  
  # if match winners
  if(nrow(fin_matches) > 0){
    
    # update counters
    for(i in unique(fin_matches$cid)){
      
      # if time budget still positive
      if(tracklist$budget[tracklist$cid == i] > 0){
        
        # update path histories
        cid_hist[[i]] <-
          cid_hist[[i]] %>%
          add_row(path_hist = as.character(fin_matches$cid_currpos[fin_matches$cid == i]),  
                  gains_hist = fin_matches$penalized_gain[fin_matches$cid == i],
                  budget_hist = fin_matches$travel_distance[fin_matches$cid == i])
        
        # update tracklist with current positions
        tracklist$cid_currpos[tracklist$cid == i] <-
          fin_matches$pot_rid[fin_matches$cid == i]
        tracklist$budget[tracklist$cid == i] <-
          tracklist$budget[tracklist$cid == i] -
          fin_matches$travel_distance[fin_matches$cid == i]
        
        # update visited
        been_visited$visited[
          been_visited$pot_rid == tracklist$cid_currpos[tracklist$cid == i]] <- 1
      }
    }
    
    # identify
    exhausted <-
      visit_ranks %>%
      group_by(cid) %>%
      summarise(num_poss = sum(travel_possible)) %>%
      filter(num_poss == 0) %>%
      pull(cid)
    
    # after update, remove anyone with exhausted time
    tracklist <-
      tracklist %>%
      filter(!cid %in% exhausted)
    
    # print current budget
    print(tracklist$budget)
    
  } else{
    
    break
    
  }
  
  
} # end while-loop


# POSTESTIMATION ----------

# get paths
paths_by_canvasser <-
  do.call(rbind, cid_hist) %>%
  mutate(cid = row.names(.),
         cid = str_sub(cid, start = 1, end = 10)) %>%
  select(path_hist, cid)

# get gains
gains_per_canvasser <-
  do.call(rbind, cid_hist) %>%
  mutate(cid = row.names(.),
         cid = str_sub(cid, start = 1, end = 10)) %>%
  group_by(cid) %>%
  summarise(total_gains = sum(gains_hist))

# plot gains
gains_per_canvasser %>%
  ggplot(aes(x = total_gains, y = reorder(cid, total_gains))) +
  geom_col(fill = "steelblue") +
  labs(x = "votes gained",
       y = "",
       title = "Advantage of Canvassing",
       subtitle = "Expected number of votes gained") +
  theme_minimal()

# combine rid paths with neighbor dataset
path_locs <-
  paths_by_canvasser %>%
  left_join(neighbor_spots, by = c("path_hist" = "rid")) %>%
  select(path_hist, cid, xspot, yspot)

# plot
neighbor_node_plot +
  geom_path(data = path_locs,
            aes(x = xspot, y = yspot, col = cid)) +
  scale_color_manual(values = distinctColorPalette(k = c)) +
  labs(x = "x location",
       y = "y location",
       title = "Path Map, Canvassers")

# plot ideology as function of visit gain
pairs_df %>%
  ggplot(aes(x = xdifflib, y = exp_votegain)) +
  geom_point() +
  theme_minimal()


# save results to list
res <-
  list(rid, cid, 
       pairs_df,
       initial_spots, cid_hist,
       been_visited,
       paths_by_canvasser,
       gains_per_canvasser,
       path_locs,
       neighbor_node_plot)

names(res) <-
  c("rid", "cid", "pairs_df",
    "initial_spots", "cid_hist",
    "been_visited",
    "paths_by_canvasser",
    "gains_per_canvasser",
    "path_locs",
    "neighbor_node_plot")


# save to rdata file
save(res,
     file = "optimal_canvassing_res.RData")






















