#######
## Summarizing LASSO results that came either from:
##    - Single fit and BIC
##    - 5-fold CV

### MAIN TAKEAWAYS:
##  1. For BIC, the "common denominator" is: -4 to +5 (although Bundesliga does skip "-4")
##  2. Both 5- and 10-fold CVs pick the FULL ESTIMATE (the LOWEST lambda),
##      and if one tries "lambda1.se", they SHRED THE ESTIMATE DOWN TO NO SCORING DIFFERENTIALS...
##      just red card diff, win probability and minute remain.
#######


library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(mpath)
library(zic)
library(pscl)


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")



###########
#####
## BIC
#####
###########


for (league in league.name){
  
  cat("\n")
  cat("\n")
  cat("\n")
  print(league)
  
  load(file=paste0("m1_object_", league, ".Robj"))

  best.BIC.ind <- which.min(BIC(m1))
  # m1$nlambda

  cat("\n")
  print("Count BIC:")
  print(m1$coefficients$count[,best.BIC.ind])
  cat("\n")
  print("Zero BIC:")
  print(m1$coefficients$zero[,best.BIC.ind])
  
  
  
  # our.df <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  # 
  # ## REMOVING NA betting data
  # our.df <- our.df %>% filter(!is.na(Weighted.Win.Prob))
  # dim(our.df)
  # 
  # ## Remove extra time data from first half (so anything with half_id=1, minute>45)
  # final.df <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
  # print(dim(final.df))
  
}




####
#############
#### MAIN TAKEAWAYS
###############
####

######
### For SHOTS:
######


## 1. All "Zero-Inflation" part coefficients are 0 besides the INTERCEPT across ALL LEAGUES
## 2. For the POISSON COUNT part:
##      - Bundesliga:     -5 through +7 (skipping -4)
##      - Serie A:        -5 through +7
##      - LaLiga:         -5 through +6
##      - Ligue1:         -4 through +5
##      - Premier League: -4 through +5
##
## COMMON DENOMINATOR:  -4 through +5 (although Bundesliga skipped -4)


######
### For CORNERS:
######


## 1. All "Zero-Inflation" part coefficients are 0 besides the INTERCEPT across ALL LEAGUES
## 2. For the POISSON COUNT part:
##      - Bundesliga:     -3 to +5
##      - Serie A:        -6 to +7
##      - LaLiga:         -6 to +7 (skipped -5)
##      - Ligue1:         -4 to +4
##      - Premier League: -7 to +7 (skipped -5/+5)
##
## COMMON DENOMINATOR:  -3 through +4 (although has several +-6, +7 cases.. 3/5)







# >   print("Count BIC:")
# [1] "Count BIC:"
# >   print(m1$coefficients$count[,best.BIC.ind])
# (Intercept)      X.Intercept.      Score.Diff.8      Score.Diff.7      Score.Diff.6 
# -1.809962030       0.000000000       0.000000000       0.000000000       0.000000000 
# Score.Diff.5      Score.Diff.4      Score.Diff.3      Score.Diff.2      Score.Diff.1 
# -0.011147374       0.000000000       0.064918560       0.123110199       0.113965390 
# Score.Diff1       Score.Diff2       Score.Diff3       Score.Diff4       Score.Diff5 
# -0.085667361      -0.150596023      -0.166744504      -0.156337046      -0.167036535 
# Score.Diff6       Score.Diff7       Score.Diff8      Minute.clean Weighted.Win.Prob 
# -0.166121385      -0.177099407       0.000000000       0.003660826       0.545983532 
# HomeAwayHome      RedCard.Diff 
# 0.041995713      -0.345804072 
# >   cat("\n")
# 
# >   print("Zero BIC:")
# [1] "Zero BIC:"
# >   print(m1$coefficients$zero[,best.BIC.ind])
# (Intercept)      X.Intercept.      Score.Diff.8      Score.Diff.7      Score.Diff.6 
# -0.6647951         0.0000000         0.0000000         0.0000000         0.0000000 
# Score.Diff.5      Score.Diff.4      Score.Diff.3      Score.Diff.2      Score.Diff.1 
# 0.0000000         0.0000000         0.0000000         0.0000000         0.0000000 
# Score.Diff1       Score.Diff2       Score.Diff3       Score.Diff4       Score.Diff5 
# 0.0000000         0.0000000         0.0000000         0.0000000         0.0000000 
# Score.Diff6       Score.Diff7       Score.Diff8      Minute.clean Weighted.Win.Prob 
# 0.0000000         0.0000000         0.0000000         0.0000000         0.0000000 
# HomeAwayHome      RedCard.Diff 
# 0.0000000         0.0000000 


### FORCING ZERO-INFL PART TO INTERCEPT-ONLY

# [1] "Bundesliga"
# [1] 814086     17
# 
# [1] "Count BIC:"
# (Intercept)      Score.Diff.8      Score.Diff.7      Score.Diff.6      Score.Diff.5      Score.Diff.4 
# -1.794859658       0.000000000       0.000000000       0.000000000      -0.021023737       0.000000000 
# Score.Diff.3      Score.Diff.2      Score.Diff.1       Score.Diff1       Score.Diff2       Score.Diff3 
# 0.067207375       0.124059712       0.114391047      -0.087008819      -0.152848943      -0.170193665 
# Score.Diff4       Score.Diff5       Score.Diff6       Score.Diff7       Score.Diff8      Minute.clean 
# -0.161673973      -0.175347502      -0.178826231      -0.197270145       0.000000000       0.003678781 
# Weighted.Win.Prob      HomeAwayHome      RedCard.Diff 
# 0.547269425       0.042449629      -0.347373870 
# 
# [1] "Zero BIC:"
# [1] -0.6197294




##########
####
## 5-fold CV
####
##########


for (league in league.name){
  
  cat("\n")
  cat("\n")
  cat("\n")
  print(league)
  
  load(file=paste0("m1_cv_object_", league, ".Robj"))
  
  m1.cv$cv
  m1.cv$lambda.which
  
  plot(m1.cv)
  m1.cv$lambda.optim$zero
  m1.cv$cv
  m1.cv$cv.error
  
  which.lambda.1se <- min(which(m1.cv$cv + m1.cv$cv.error >= max(m1.cv$cv)))
  lambda.1se <- m1.cv$lambda.which
  
  m1.cv$terms
  
  # our.df <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  # 
  # ## REMOVING NA betting data
  # our.df <- our.df %>% filter(!is.na(Weighted.Win.Prob))
  # dim(our.df)
  # 
  # ## Remove extra time data from first half (so anything with half_id=1, minute>45)
  # final.df <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
  # print(dim(final.df))
  
  
  #########
  ## Loading the actual fitted object
  #########
  
  load(file=paste0("m1_object_", league, ".Robj"))
  
  coef(m1, which = m1.cv$lambda.which)
  coef(m1, which = which.lambda.1se)
}



######
## For BUNDESLIGA:
##    * For lambda.min, just picks the lowest lambda (100/100)
##    * BUT, even that lowest lambda ONLY PICKS INTERCEPT FOR THE ZERO-INFL PART OF THE MODEL
##    * For lambda.1se, it just kills WAY TOO MUCH of the model (only retains red cards, win probability & minute)
##     => MIGHT BE DUE TO LOW DENOMINATOR OF STANDARD ERROR, with 5-FOLD CV... could try 10-FOLD INSTEAD
##


### ISSUE: RAN WITH MODEL MATRIX INCLUDING 2 INTERCEPT COLUMNS
