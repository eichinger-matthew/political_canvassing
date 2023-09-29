# simulate geography



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