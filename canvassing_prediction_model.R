# ---------- define data-generating process for voting ---------- 

# libraries
library(tidyverse)


# define vote model (see paper for theoretical explanation)
vote_model <- function(xdifflibsq, xdiffconsq, ethdifflibsq, 
                       ethdiffconsq, Va, Vb, ethmatch_canvlib, 
                       ethmatch_canvcon){
  
  # define linear predictor
  z <- 0.1 + 2*(xdiffconsq - xdifflibsq) + 
    0.7*(ethdiffconsq - ethdifflibsq) +
    0.3*(Va-Vb) + 0.2*(Va*ethmatch_canvlib - Vb*ethmatch_canvcon)
  
  #  return linear predictor
  return(z)
}
































