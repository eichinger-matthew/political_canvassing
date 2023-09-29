# functions used in the canvassing algorithm


# compute travel distance given current spot and future spot
trav_dist <- function(x1, x2, y1, y2){
  # travel distance
  x <- sqrt((x1-x2)^2)
  y <- sqrt((y1-y2)^2)
  return(x + y)
}

# calculate best visits on board
arrange_ranks <- function(df, trackers, rid_spots, been_vis){
  # get penalized gains
  # filter out current positions
  # filter out visited residents
  # filter canvassers with exhausted budgets
  # define whether a canv-res pair can be traveled
  ranks <-
    df %>%
    left_join(trackers, 'cid') %>%
    left_join(rid_spots, by = c('rid.now' = 'rid'), suffix = c("", ".now")) %>%
    left_join(been_vis, by = c('rid')) %>%
    filter(budget > 0, visited == 0, rid != rid.now) %>%
    mutate(tdistance = trav_dist(xpos, xpos.now, ypos, ypos.now) + 1,
           tpossible = ifelse(budget > tdistance, 1, 0),
           penalized_gain = exp_votegain/tdistance)
  return(ranks)
}

# get best ranks per canvassers
optimal_ranks <- function(df, multi){
  # use multi variable to determine if algo should be fast or slow
  if(isTRUE(multi)){
    # get best choice for each canvasser
    optim_ranks <-
      df %>%
      filter(tpossible == 1) %>%
      group_by(cid) %>%
      mutate(visit_rank = min_rank(desc(penalized_gain))) %>%
      filter(visit_rank == 1) %>%
      ungroup()
    return(optim_ranks)
  } else{
    # not sure why the ungroup is needed here - very annoying
    # get global best
    optim_ranks <-
      df %>%
      filter(tpossible == 1) %>%
      ungroup() %>%
      mutate(visit_rank = min_rank(desc(penalized_gain))) %>%
      filter(visit_rank == 1)
    return(optim_ranks)
  }
}

# deal with duplicates
handle_conflicts <- function(df){
  # identify matches by resident
  match_conflicts <-
    df %>%
    group_by(rid) %>%
    mutate(num_optims = n()) %>%
    filter(num_optims > 1)
  
  # if there are match conflicts
  if(nrow(match_conflicts) > 0){
    
    # identify losers
    match_losers <- 
      match_conflicts %>%
      group_by(rid) %>%
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

# postestimation function -----------------------------------------

# write function for analysis
canvassing_postestimation <- function(data_history){
  
  # get paths
  paths_by_canvasser <-
    do.call(rbind, data_history) %>%
    mutate(cid = row.names(.),
           cid = str_sub(cid, start = 1, end = 10)) %>%
    select(path_hist, cid)
  
  # get gains
  gains_per_canvasser <-
    do.call(rbind, data_history) %>%
    mutate(cid = row.names(.),
           cid = str_sub(cid, start = 1, end = 10)) %>%
    group_by(cid) %>%
    summarise(total_gains = sum(gains_hist))
  
  # plot gains
  barplot_gains <-
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
    left_join(rid_spots, by = c("path_hist" = "rid")) %>%
    select(path_hist, cid, xpos, ypos)
  
  # plot
  path_plot <-
    plot_residents +
    geom_path(data = path_locs,
              aes(x = xpos, y = ypos, col = cid)) +
    labs(x = "x location",
         y = "y location",
         title = "Path Map, Canvassers")
  
  # make list
  res <- list("df_original" = data_history,
              "df_canvasser_paths" = path_locs,
              "canvasser_gains" = gains_per_canvasser,
              "barplot_gains" = barplot_gains,
              "path_map" = path_plot)
  return(res)
}






























