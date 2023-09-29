library(ids)


# define house getting function
home_gen <- function(n, phouse, avg_house_mems, avg_apt_units,
                     avg_apt_mems){
  
  # define list
  store <- list("houses" = list(), "apts" = list())
  
  # define types
  types <- c("house", "apt")
  
  # draw n samples from types, given phouse
  draws <- sample(x = types, size = n, prob = c(phouse, 1-phouse),
                  replace = TRUE)
  
  # get number of houses drawn
  num_houses <- length(draws[draws == "house"])
  
  # get number of apts drawn
  num_apts <- n - num_houses
  
  # for-loop
  for(i in 1:length(draws)){
    
    # draw coordinates of place
    x <- rnorm(n = 1, mean = 0, sd = 30)
    y <- rnorm(n = 1, mean = 0, sd = 30)
    
    # draw unique id
    id <- ids::random_id(n = 1)
    
    
    # if house
    if(draws[i] == "house"){
      
      # draw number of members
      membs <- rpois(n = 1, lambda = avg_house_mems)
      
      # draw characteristics (in more complicated model)
      # this would be based on empirical distributions obs. in population
      house_df <- data.frame(house_id = id, xcoord = x, ycoord = y,
                             num_membs = membs)
      
      # store information in list
      store[["houses"]][[id]] <- house_df
      
    } else if(draws[i] == "apt"){
      
      # draw number of units
      units <- rpois(n = 1, lambda = avg_apt_units)
      
      # make data frame
      apt_info <- 
        data.frame(apt_id = id, num_units = units,
                   xcoord = x, ycoord = y)
      
      # make another list for unit characteristics
      
      # add list
      store[["apts"]][[id]] <-
        list(apt_info = apt_info,
             units_info = list())
      
      # for-loop to add unit-specific info
      for(j in 1:units){
        
        # draw id
        unit_id <- random_id(n = 1)
        
        # draw number of members
        membs <- rpois(n = 1, lambda = avg_apt_mems)
        
        # df
        unit_df <- 
          data.frame(apt_id = id, unit_id = unit_id,
                     num_membs = membs)
        
        # add to list
        store[["apts"]][[id]][["units_info"]][[unit_id]] <-
          unit_df
      }
    }
  }
  # return
  return(store)
}


check <- home_gen(n = 20000, phouse = 0.9, avg_house_mems = 3,
                  avg_apt_units = 15, avg_apt_mems = 2)















