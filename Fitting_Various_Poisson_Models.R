library(mgcv)
library(tidyverse)
library(splines)

# load("final_df_w_red_cards.csv")

final.df <- read.csv("final_df_w_red_cards_WITH_BOOKING_ODDS.csv")


#####
## 1. Regular Neg Bin GAM, no interactions, with minutes.spent as a predictor.
#####

# NON-SCALED
gam.no.int.obj <- mgcv::gam(Shots ~ s(minutes.spent) + s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + RedCard.Diff, 
                        data=final.df,
                        family="nb")
plot(gam.no.int.obj)
abline(v=1); abline(v=0); abline(v=-1); abline(v=-2)

## !!! SCALED => NO DIFFERENCE !!!
# gam.no.int.obj.scaled <- mgcv::gam(Shots ~ s(minutes.spent) + s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + RedCard.Diff, 
#                             data=final.df %>% mutate(minutes.spent = scale(minutes.spent),
#                                                      Score.Diff = scale(Score.Diff),
#                                                      Weighted.Win.Prob = scale(Weighted.Win.Prob)),
#                             family="nb")
# plot(gam.no.int.obj.scaled)


#####
## 2. Regular Neg Bin GAM, no interactions, with minutes.spent AS AN OFFSET
##    - ALL GOOD when we use "log(...)" for the "offset" (doesn't matter if it's /90 or not)
##    PREFERABLY USE IT in the FORMULA ITSELF => so that one HAD to specify offset to make predictions
##
##   !!! MAYBE IT'LL EVEN BE BETTER IN REGARDS TO THE AIC COMPARISONS ???
##  "	Can be used to supply a model offset for use in fitting. 
##   Note that this offset will always be completely ignored when predicting, 
##  unlike an offset included in formula (this used to conform to the behaviour 
##  of lm and glm)."
##    
#####

gam.no.int.offset.obj <- mgcv::gam(Shots ~ s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
                                   data=final.df,
                                   # offset = log(minutes.spent+1),
                                   family="nb")
plot.gam(gam.no.int.offset.obj, select=1)
abline(v=2); abline(v=1); abline(v=0); abline(v=-1); abline(v=-2);


# gam.no.int.offset.obj <- mgcv::gam(Shots ~ s(Score.Diff)  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
#                                    data=final.df,
#                                    # offset = log(minutes.spent+1),
#                                    family="nb")
# plot.gam(gam.no.int.offset.obj, select=1)
# abline(v=2); abline(v=1); abline(v=0); abline(v=-1); abline(v=-2);


gam.reg.pois.offset.obj <- mgcv::gam(Shots ~ s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
                                     data=final.df,
                                     method="REML",
                                     # offset = log(minutes.spent+1),
                                     family="poisson")

anova(gam.reg.pois.offset.obj,
      gam.no.int.offset.obj, test = "Chisq")



#####
## With INT
####

gam.w.int.obj <- mgcv::gam(Shots ~ ti(minutes.spent, k=10) + ti(Score.Diff, k=10) + ti(minutes.spent, Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + RedCard.Diff,
                                   data=final.df,
                                   # offset = log(minutes.spent+1),
                                   family="nb")
plot(gam.w.int.obj)




######
## LINEAR (no splines)
######
library(MASS)
glm.no.int.obj <- glm.nb(Shots ~ Score.Diff + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)), 
                            data=final.df)
# plot(glm.no.int.obj)
glm.no.int.obj


######
## NO SCORE.DIFF
## !!! => REALLY BAD, CONFIRMING THE IMPORTANCE OF SCORE.DIFF !!!
##
##                             df      AIC
# gam.no.int.offset.obj 16.83817 108945.6
# glm.no.int.obj         6.00000 109220.6
# gam.no.score.diff.obj  5.00000 109808.5
######
gam.no.score.diff.obj <- mgcv::gam(Shots ~ HomeAway + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)), 
                                   data=final.df,
                                   # offset = log(minutes.spent+1),
                                   family="nb")



## !!!! OFFSET MODEL IS BETTER !!!
AIC( gam.no.int.obj,
     #gam.no.int.obj.scaled,
     gam.no.int.offset.obj,
     gam.w.int.obj,
     glm.no.int.obj,
     gam.no.score.diff.obj)



######
## DHARMa
######

##
# NATURAL CUBIC SPLINE
##
library(splines)
library(DHARMa)

# Making degrees of freedom the same as with smoothing splines model
glm.ns.obj <- glm.nb(Shots ~ ns(Score.Diff, 7) + ns(Weighted.Win.Prob,5)  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)), 
                         data=final.df)
glm.ns.obj

simulationOutput <- simulateResiduals(fittedModel = glm.ns.obj, plot = F)
plot(simulationOutput)

## To compare with regular linear
simulationOutput <- simulateResiduals(fittedModel = glm.no.int.obj, plot = F)
plot(simulationOutput)

