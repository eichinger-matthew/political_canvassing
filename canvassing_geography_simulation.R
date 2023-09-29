# ---------- canvassing geography ---------- 

# libraries
library(tidyverse)


# simulate neighborhood anchors --------------------------

# write function to simulate neighborhood blocs on a 2d plane
sim_bloc_anchors <- function(num_blocs, upper_lim, lower_lim){
  # blocs is number of blocs
  # upper and lower lim are boundaries on the plane
  
  # simulate x-y positions of blocs 
  bloc_locs <- 
    matrix(data = runif(n = 2*num_blocs, min = lower_lim, max = upper_lim),
           ncol = 2, nrow = num_blocs)
  # format to a num_blocs X 3 dataframe
  bloc_df <-
    bloc_locs %>%
    as.data.frame() %>%
    rename(x_anchor = 1, y_anchor = 2) %>%
    mutate(bloc_id = seq(1, num_blocs, 1))
  # return dataframe
  return(bloc_df)
}

# simulate bloc anchors
bloc_df <- sim_bloc_anchors(num_blocs = 40, upper_lim = 60, lower_lim = -60)

# plot anchors
bloc_df %>%
  ggplot(aes(x = x_anchor, y = y_anchor)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  lims(x = c(-60, 60), y = c(-60, 60)) +
  labs(title = "Distribution of Neighborhood Anchors") +
  theme_minimal() +
  theme(panel.background = element_rect(linewidth = 1))




# simulate neighbors -------------------------------------

# define neighbor simulation function
sim_neighbors <- function(
    blocs_df, num_residents, lower_offset, upper_offset){
  
  # create storage matrix that will hold geographic locations of residents
  # for each resident, draw x-y offset values from random uniform distribution
  anchor_offsets <- 
    matrix(data = runif(n = 2*num_residents, min = lower_offset, max = upper_offset),
           nrow = num_residents, ncol = 2) %>%
    as.data.frame() %>%
    rename(x_offset = 1, y_offset = 2)
  
  # combine with df
  df_long <- 
    blocs_df %>%
    slice(rep(1:n(), each = num_residents/nrow(blocs_df))) %>%
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
  sim_neighbors(blocs_df = bloc_df, num_residents = n, lower_lim = -4, upper_lim = 4) %>%
  cbind(rid)

# plot
neighbor_node_plot <-
  neighbor_spots %>%
  ggplot(aes(x = xspot, y = yspot)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  lims(x = c(-65, 65), y = c(-65, 65)) +
  labs(title = "Distribution of Individual Neighbors") +
  theme_minimal()



# alternative method using for-loop --------------------

# generate storage matrix
neighbors <- matrix(data = NA, nrow = n, ncol = 2)
for(i in 1:nrow(neighbors)){
  
  
  
  
  
}







# end script