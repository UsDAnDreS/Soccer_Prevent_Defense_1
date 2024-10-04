library(mgcv)
library(tidyverse)
library(splines)
library(MASS)


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


stepwise.obj <- list()

  
for (league in league.name){
  
print(league)
  
# load("final_df_w_red_cards.csv")

final.df <- read.csv(paste0(league, "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))

#########
## Fitting the Neg Bin regression with:
##    * Score.Diff as DUMMY variables with "0" as reference level 
##      (achieved via mutate(Score.Diff = relevel(factor(Score.Diff), ref="0")))
##    * Making effects of Weighted.Win.Prob & RedCard.Diff linear
#########

glm.no.int.offset.obj <- glm.nb(Shots ~ Score.Diff + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
                                data=final.df %>% mutate(ID = factor(ID), Score.Diff = relevel(factor(Score.Diff), ref="0")))
glm.no.int.offset.obj

## Extracting the design matrix with dummy variables being treated as separate predictors
X <- model.matrix(glm.no.int.offset.obj)[,-1]
y <- glm.no.int.offset.obj$y

## Fitting the negative binomial
glm.model.mat <- glm.nb(y ~ . + offset(glm.no.int.offset.obj$offset), data=data.frame(X))
glm.model.mat

## Using stepwise selection with BIC as the criteria (leads to a simpler model than AIC)
step.obj <- step(glm.model.mat, k=log(length(y))
                , trace=F
                 )
print(step.obj)


stepwise.obj[[league]] <- step.obj

}



save(file="Shots_stepwise_obj.Robj", stepwise.obj)



## BUNDESLIGA
## Score Diff: -3 to 4

# Coefficients:
# (Intercept)       Score.Diff.3       Score.Diff.2       Score.Diff.1        Score.Diff1  
# -2.09399            0.16716            0.21220            0.17128           -0.07528  
# Score.Diff2        Score.Diff3        Score.Diff4  Weighted.Win.Prob       HomeAwayHome  
# -0.12047           -0.14688           -0.15389            0.56792            0.04810  
# RedCard.Diff  
# -0.40120


# Df Deviance    AIC
# <none>                    30330 110680
# - Score.Diff4        1    30345 110685
# - Score.Diff3        1    30371 110711
# - HomeAwayHome       1    30373 110713
# - Score.Diff.3       1    30376 110716
# - Score.Diff1        1    30391 110731
# - Score.Diff2        1    30402 110742
# - Score.Diff.2       1    30555 110895
# - Score.Diff.1       1    30664 111004
# - RedCard.Diff       1    31082 111422
# - Weighted.Win.Prob  1    33635 113975


## SERIE A
## Score Diff: -5 to 6

# Coefficients:
# (Intercept)       Score.Diff.5       Score.Diff.4       Score.Diff.3       Score.Diff.2  
# -2.05878            0.44350            0.32534            0.33600            0.34253  
# Score.Diff.1        Score.Diff1        Score.Diff2        Score.Diff3        Score.Diff4  
# 0.19652           -0.20451           -0.26289           -0.32221           -0.36130  
# Score.Diff5        Score.Diff6  Weighted.Win.Prob       HomeAwayHome       RedCard.Diff  
# -0.51029           -0.68662            0.63782            0.03215           -0.40439 


# Df Deviance    AIC
# <none>                    34749 128146
# - Score.Diff.5       1    34761 128146
# - Score.Diff5        1    34767 128153
# - HomeAwayHome       1    34776 128162
# - Score.Diff.4       1    34783 128169
# - Score.Diff4        1    34792 128178
# - Score.Diff3        1    34918 128304
# - Score.Diff.3       1    34933 128318
# - Score.Diff2        1    35073 128458
# - Score.Diff1        1    35215 128601
# - Score.Diff.1       1    35231 128617
# - Score.Diff.2       1    35371 128757
# - RedCard.Diff       1    35886 129271
# - Weighted.Win.Prob  1    39220 132605




## LA LIGA
##  Score Diff: -5 to 5

# Coefficients:
# (Intercept)       Score.Diff.5       Score.Diff.4       Score.Diff.3       Score.Diff.2       Score.Diff.1  
# -2.16603            0.33787            0.35765            0.32504            0.29623            0.18572  
# Score.Diff1        Score.Diff2        Score.Diff3        Score.Diff4        Score.Diff5  Weighted.Win.Prob  
# -0.17687           -0.21619           -0.19636           -0.22080           -0.26064            0.54341  
# HomeAwayHome       RedCard.Diff  
# 0.09165           -0.39422 


# Df Deviance    AIC
# <none>                    31437 114228
# - Score.Diff5        1    31450 114231
# - Score.Diff.5       1    31454 114234
# - Score.Diff4        1    31468 114248
# - Score.Diff.4       1    31501 114281
# - Score.Diff3        1    31506 114286
# - HomeAwayHome       1    31591 114371
# - Score.Diff.3       1    31603 114384
# - Score.Diff2        1    31658 114439
# - Score.Diff1        1    31777 114557
# - Score.Diff.1       1    31841 114622
# - Score.Diff.2       1    31863 114643
# - RedCard.Diff       1    32668 115448
# - Weighted.Win.Prob  1    34642 117422




## League 1
##    Score Diff: -4 through 5

# Coefficients:
# (Intercept)       Score.Diff.4       Score.Diff.3       Score.Diff.2       Score.Diff.1        Score.Diff1  
# -2.15925            0.28958            0.29854            0.23765            0.17056           -0.18027  
# Score.Diff2        Score.Diff3        Score.Diff4        Score.Diff5  Weighted.Win.Prob       HomeAwayHome  
# -0.18732           -0.22555           -0.27613           -0.30242            0.53508            0.06269  
# RedCard.Diff  
# -0.41724  

# Df Deviance    AIC
# <none>                    35045 126333
# - Score.Diff.4       1    35075 126352
# - Score.Diff4        1    35076 126353
# - Score.Diff3        1    35111 126388
# - HomeAwayHome       1    35129 126406
# - Score.Diff.3       1    35169 126446
# - Score.Diff2        1    35183 126460
# - Score.Diff.2       1    35296 126573
# - Score.Diff.1       1    35404 126681
# - Score.Diff1        1    35404 126681
# - RedCard.Diff       1    36463 127740
# - Weighted.Win.Prob  1    37444 128721



## PREMIER LEAGUE
##    Score Diff: -4 through 4

# Coefficients:
# (Intercept)       Score.Diff.4       Score.Diff.3       Score.Diff.2       Score.Diff.1        Score.Diff1  
# -2.10626            0.20108            0.21234            0.19768            0.15103           -0.17309  
# Score.Diff2        Score.Diff3        Score.Diff4  Weighted.Win.Prob       HomeAwayHome       RedCard.Diff  
# -0.18478           -0.24285           -0.22990            0.66889            0.05172           -0.48726  



#####
## -3 THROUGH 4 is the LOWEST COMMON DENOMINATOR;
##  -4 THROUGH 4 IS THE MOST COMMON SYMMETRIC PRESENCE (4 out of 5 leagues)
#####





############
###########
## CORNERS
###########
############



stepwise.obj <- list()

for (league in league.name){
  
  print(league)
  
  # load("final_df_w_red_cards.csv")
  
  final.df <- read.csv(paste0(league, "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  #########
  ## Fitting the Neg Bin regression with:
  ##    * Score.Diff as DUMMY variables with "0" as reference level 
  ##      (achieved via mutate(Score.Diff = relevel(factor(Score.Diff), ref="0")))
  ##    * Making effects of Weighted.Win.Prob & RedCard.Diff linear
  #########
  
  glm.no.int.offset.obj <- glm.nb(Corners ~ Score.Diff + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
                                  data=final.df %>% mutate(ID = factor(ID), Score.Diff = relevel(factor(Score.Diff), ref="0")))
  glm.no.int.offset.obj
  
  ## Extracting the design matrix with dummy variables being treated as separate predictors
  X <- model.matrix(glm.no.int.offset.obj)[,-1]
  y <- glm.no.int.offset.obj$y
  
  ## Fitting the negative binomial
  glm.model.mat <- glm.nb(y ~ . + offset(glm.no.int.offset.obj$offset), data=data.frame(X))
  glm.model.mat
  
  ## Using stepwise selection with BIC as the criteria (leads to a simpler model than AIC)
  step.obj <- step(glm.model.mat, k=log(length(y))
                   , trace=F
  )
  print(step.obj)
  
  
  stepwise.obj[[league]] <- step.obj
  
}

save(file="Corners_stepwise_obj.Robj", stepwise.obj)



## BUNDESLIGA
## Score Diff: -3 to +5

# Coefficients:
#   (Intercept)       Score.Diff.3       Score.Diff.2       Score.Diff.1        Score.Diff1  
# -3.0520             0.1662             0.2643             0.1986            -0.1755  
# Score.Diff2        Score.Diff3        Score.Diff4        Score.Diff5  Weighted.Win.Prob  
# -0.2871            -0.2945            -0.2817            -0.3624             0.6435  
# HomeAwayHome       RedCard.Diff  
# 0.0474            -0.3691



## SERIE A
## Score Diff: -3 to +5

# Coefficients:
#   (Intercept)       Score.Diff.3       Score.Diff.2       Score.Diff.1        Score.Diff1  
# -2.99268            0.37659            0.33274            0.24544           -0.31444  
# Score.Diff2        Score.Diff3        Score.Diff4        Score.Diff5  Weighted.Win.Prob  
# -0.46551           -0.52532           -0.63950           -0.80505            0.66125  
# HomeAwayHome       RedCard.Diff  
# 0.05529           -0.33520 





## LA LIGA
##  Score Diff: -4 to +5

# Coefficients:
#   (Intercept)       Score.Diff.4       Score.Diff.3       Score.Diff.2       Score.Diff.1  
# -3.0514             0.3367             0.3033             0.3257             0.2319  
# Score.Diff1        Score.Diff2        Score.Diff3        Score.Diff4        Score.Diff5  
# -0.2968            -0.4253            -0.4589            -0.3199            -0.4908  
# Weighted.Win.Prob       HomeAwayHome       RedCard.Diff  
# 0.5572             0.1382            -0.3363  




## League 1
##    Score Diff: -3 through +4

# Coefficients:
#   (Intercept)       Score.Diff.3       Score.Diff.2       Score.Diff.1        Score.Diff1  
# -3.08015            0.33100            0.30262            0.22965           -0.30671  
# Score.Diff2        Score.Diff3        Score.Diff4  Weighted.Win.Prob       HomeAwayHome  
# -0.39741           -0.48560           -0.38142            0.57235            0.09193  
# RedCard.Diff  
# -0.37159 




## PREMIER LEAGUE
##    Score Diff: -3 through 4

# Coefficients:
#   (Intercept)       Score.Diff.3       Score.Diff.2       Score.Diff.1        Score.Diff1  
# -2.98865            0.22330            0.22862            0.19237           -0.26188  
# Score.Diff2        Score.Diff3        Score.Diff4  Weighted.Win.Prob       HomeAwayHome  
# -0.29088           -0.38649           -0.33201            0.68217            0.05881  
# RedCard.Diff  
# -0.41526  


#####
## -3 THROUGH +4 is the LOWEST COMMON DENOMINATOR;
##  -3 THROUGH +3 IS THE MOST COMMON SYMMETRIC PRESENCE (all 5 leagues)
#####