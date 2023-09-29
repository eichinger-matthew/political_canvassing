# voting models
library(tidyverse)
library(janitor)
library(arm)
library(truncnorm)
library(caret)

# load real data
# df <- read_csv("CES22_Common.csv")
# 
# # clean
# df.clean <-
#   df %>%
#   rename(vote20 = presvote20post,
#          income_bracket = faminc_new) %>%
#   mutate(did_vote = ifelse(vote20 == 6, 0, 1),
#          vote20 = 
#            case_when(vote20 == 1 ~ "Joe Biden",
#                      vote20 == 2 ~ "Donald Trump",
#                      vote20 == 3 ~ "Jo Joregensen",
#                      vote20 == 4 ~ "Howie Hawkens",
#                      vote20 == 5 ~ "Other",
#                      vote20 == 6 ~ "Did not vote"),
#          race = 
#            case_when(race == 1 ~ "White",
#                      race == 2 ~ "Black or African-American",
#                      race == 3 ~ "Hispanic or Latino",
#                      race == 4 ~ "Asian or Asian-American",
#                      race == 5 ~ "Native American",
#                      race == 8 ~ "Middle Eastern",
#                      race == 6 ~ "Two or more races",
#                      race == 7 ~ "Other"),
#          income_bracket = ifelse(income_bracket == 97, NA, income_bracket),
#          urbancity = 
#            case_when(urbancity == 1 ~ "City",
#                      urbancity == 2 ~ "Suburb",
#                      urbancity == 3 ~ "Town",
#                      urbancity == 4 ~ "Rural area",
#                      urbancity == 5 ~ "Other"),
#          pew_religimp = 
#            case_when(pew_religimp == 4 ~ 1,
#                      pew_religimp == 3 ~ 2,
#                      pew_religimp == 2 ~ 3,
#                      pew_religimp == 1 ~ 4)) %>%
#   select(did_vote, vote20, race, income_bracket, urbancity, pew_religimp) %>%
#   drop_na() %>%
#   mutate(vote20 = as.factor(vote20),
#          race = as.factor(race),
#          urbancity = as.factor(urbancity))
# temp <- 
#   df.clean %>%
#   dummyVars(" ~ .", data = df.clean, fullRank = TRUE)
# df.clean.onehot <- data.frame(predict(temp, newdata = df.clean)) %>%
#   clean_names()
# # get correlations
# corr_mat <- cor(df.clean.onehot, method = "pearson")


# get correlation matrix for variables of interest


# purpose ------

# two discriminative models

# 1. model to predict 0/1 vote
# 2. model to predict 0/1 party

# use z and x to denote model variables
# use a and b to denote parameters

# ideology of candidates
# conservative
cons_ideo <- 0.7
cons_eth <- 1
# liberal
lib_ideo <- 0.3
lib_eth <- 0


# ethnicity
c_ethnic <- rbinom(n = C, size = 1, prob = 0.2)
# ideology
c_pols <- rtruncnorm(n = C, a = 0, b = 0.5, mean = 0.35, sd = 0.5)
# income
c_income <- rnorm(n = C, mean = 60000, sd = 15000)

# dataframe
c_df <- 
  cbind(cid, c_ethnic, c_pols, c_income) %>%
  as.data.frame() %>%
  mutate(c_ethnic = as.numeric(c_ethnic),
         c_pols = as.numeric(c_pols),
         c_income = as.numeric(c_income))

# ethnicity
r_ethnic <- rbinom(n = R, size = 1, prob = 0.2)
# ideo prefs
r_pols <- rtruncnorm(n = R, a = 0, b = 1, mean = 0.6, sd = 0.15)
# income
r_income <- rtruncnorm(n = R, a = 1, b = 200000, mean = 65000, sd = 30000)

# make resident df
r_df <- 
  cbind(rid, r_ethnic, r_pols, r_income) %>%
  as.data.frame() %>%
  mutate(r_ethnic = as.numeric(r_ethnic),
         r_pols = as.numeric(r_pols),
         r_income = as.numeric(r_income),
         r_log_income = log(r_income))



# data description --------

# get full dataset
full_df <- 
  expand.grid(r_df$rid, c_df$cid) %>%
  rename(rid = 1, cid = 2) %>%
  left_join(r_df, by = c("rid")) %>%
  left_join(c_df, by = c("cid")) %>%
  mutate(cons_eth = cons_eth,
         cons_ideo = cons_ideo,
         r_extreme = abs(r_pols - 0.5),
         lib_eth = lib_eth,
         lib_ideo = lib_ideo,
         ethdifflib = ifelse(r_ethnic != lib_eth, 1, 0),
         ethdiffcon = ifelse(r_ethnic != cons_eth, 1, 0),
         ethmatch_canvlib = ifelse(r_ethnic == c_ethnic, 1, 0),
         xdiffcon = cons_ideo-r_pols,
         xdifflib = lib_ideo-r_pols,
         xdiffconsq = xdiffcon^2,
         xdifflibsq = xdifflib^2,
         ethdifflibsq = ethdifflib^2,
         ethdiffconsq = ethdiffcon^2,
         std_log_income = scale(r_log_income),
         std_extreme = scale(r_extreme))

# vote models --------

vote_mod <- function(r_ethnic, r_log_income, r_extreme, visit, ethnic_match){
  # lin pred
  z <- 0.2 -0.4*r_ethnic + 1.5*r_log_income + 0.7*r_extreme - 0.2*visit + 0.5*visit*ethnic_match
  return(z)
}

full_df %>%
  mutate(linpred_vote_1visit = vote_mod(r_ethnic, std_log_income, std_extreme, 
                                        visit = 1, ethmatch_canvlib),
         pprob_vote_1visit = invlogit(linpred_vote_1visit),
         linpred_vote_0visit = vote_mod(r_ethnic, std_log_income, std_extreme, 
                                        visit = 0, ethmatch_canvlib),
         pprob_vote_0visit = invlogit(linpred_vote_0visit),
         visit_gain = pprob_vote_1visit - pprob_vote_0visit) %>%
  select(r_ethnic, std_log_income, std_extreme, visit_gain)



lib_con_mod <- function(xdifflibsq, xdiffconsq, ethdifflibsq, 
                    ethdiffconsq, Va, Vb, ethmatch_canvlib, 
                    ethmatch_canvcon){
  # define linear predictor
  z <- 0.1 + 2*(xdiffconsq - xdifflibsq) + 
    0.7*(ethdiffconsq - ethdifflibsq) +
    0.3*(Va-Vb) + 0.2*(Va*ethmatch_canvlib - Vb*ethmatch_canvcon)
  #  return linear predictor
  return(z)
}






















