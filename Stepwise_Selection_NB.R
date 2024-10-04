library(mgcv)
library(tidyverse)
library(splines)
library(MASS)

# load("final_df_w_red_cards.csv")

final.df <- read.csv("final_df_w_red_cards_WITH_BOOKING_ODDS.csv")

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
step.obj <- step(glm.model.mat, k=log(length(y)))
step.obj

# Df Deviance    AIC
# <none>                    30356 110757
# - Score.Diff4        1    30372 110762
# - Score.Diff3        1    30397 110788
# - HomeAwayHome       1    30398 110789
# - Score.Diff.3       1    30402 110793
# - Score.Diff1        1    30416 110807
# - Score.Diff2        1    30429 110819
# - Score.Diff.2       1    30579 110969
# - Score.Diff.1       1    30692 111083
# - RedCard.Diff       1    31109 111500
# - Weighted.Win.Prob  1    33654 114045


## BETTER THAN:

# df      BIC
# gam.no.int.offset.obj             23.00000 109133.4
# gam.no.int.low.df.offset.obj      10.00000 109142.9
# gam.no.int.offset.dummy.reduc.obj 17.60803 109081.3



# Coefficients:
#   (Intercept)       Score.Diff.3       Score.Diff.2       Score.Diff.1  
# -2.09371            0.16068            0.20798            0.17044  
# Score.Diff1        Score.Diff2        Score.Diff3        Score.Diff4  
# -0.07356           -0.11838           -0.14317           -0.14985  
# Weighted.Win.Prob       HomeAwayHome       RedCard.Diff  
# 0.56736            0.04718           -0.40067  