library(mgcv)
library(tidyverse)
library(splines)
library(MASS)


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

length(unique(final.df$gameId))

extr.score <- c(4,5)[2]

for (league in league.name){
  
 # print(league)
  
  # load("final_df_w_red_cards.csv")
  
  final.df <- read.csv(paste0(league, "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  #########
  ## Fitting the Neg Bin regression with:
  ##    * Score.Diff as DUMMY variables with "0" as reference level 
  ##      (achieved via mutate(Score.Diff = relevel(factor(Score.Diff), ref="0")))
  ##    * Merge the RIGHT-HAND extremes into MOST RIGHTHAND CATEGORY, while LEFT-HAND - into MOST LEFTHAND
  ##    * Making effects of Weighted.Win.Prob & RedCard.Diff linear
  #########
  
  
  # glm.no.int.offset.obj <- glm.nb(Shots ~ Score.Diff + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
  #                                 data=final.df %>% mutate(ID = factor(ID),
  #                                                          Score.Diff = relevel(factor(
  #                                                            ifelse(Score.Diff >= extr.score,
  #                                                                   paste0(extr.score, ".or.better"),
  #                                                                   ifelse(Score.Diff <= -extr.score,
  #                                                                          paste0(-extr.score, ".or.worse"),
  #                                                                          Score.Diff))), ref="0")))
  # glm.no.int.offset.obj
  
  
  
  #########
  ## Fitting the Neg Bin regression with:
  ##    * Score.Diff as DUMMY variables with "0" as reference level 
  ##      (achieved via mutate(Score.Diff = relevel(factor(Score.Diff), ref="0")))
  ##    * MERGE THE EXTREMES into BASELINE
  ##    * Making effects of Weighted.Win.Prob & RedCard.Diff linear
  #########
  
  glm.no.int.offset.obj <- glm.nb(Shots ~ Score.Diff + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
                                  data=final.df %>% mutate(ID = factor(ID),
                                                           Score.Diff = relevel(factor(
                                                             ifelse(abs(Score.Diff) >= extr.score,
                                                                    "0",
                                                                    Score.Diff)),
                                                             ref="0")))
  glm.no.int.offset.obj
  
  
  
  
#  print("BIC:")
   print(AIC(glm.no.int.offset.obj, k=log(length(glm.no.int.offset.obj$y))))
  
  print(glm.no.int.offset.obj$coefficients)
  
}

## 4.or.more
##  110687.9
##  128125
##  114213.1
##  126349
##  113693.1

## 5.or.more
# [1] 110707.7
# [1] 128143.3
# [1] 114233.2
# [1] 126357
# [1] 113707.5

## 5.or.more into THE BASELINE
# [1] 110700.3
# [1] 128163.9
# [1] 114247.1
# [1] 126343
# [1] 113692.7


## BIC (best model picked, be it -5:5, -3:4, etc)
##  110680
##  128146
##  114228
##  126333
##  113693.1



#########
## ALL-IN-ALL:
##    * The "+4.or.better" and "-4.or.worse"  seems to do best for the most part: 
##      clearly best on 3/5, and equal or a little worse on other 2/5
#########



######
## For the "+4.or.better" and "-4.or.worse"
######

# "Bundesliga"
# (Intercept)          Score.Diff-1          Score.Diff-2          Score.Diff-3 Score.Diff-4.or.worse           Score.Diff1 
# -2.092404694           0.168679927           0.207953782           0.160139132           0.008877526          -0.076098915 
# Score.Diff2           Score.Diff3 Score.Diff4.or.better     Weighted.Win.Prob          HomeAwayHome          RedCard.Diff 
# -0.120164408          -0.145223876          -0.167088217           0.570415807           0.047461851          -0.401519294 
# 
# "SerieA"
# (Intercept)          Score.Diff-1          Score.Diff-2          Score.Diff-3 Score.Diff-4.or.worse           Score.Diff1 
# -2.0595055             0.1901381             0.3330151             0.3104705             0.2666995            -0.1951849 
# Score.Diff2           Score.Diff3 Score.Diff4.or.better     Weighted.Win.Prob          HomeAwayHome          RedCard.Diff 
# -0.2497000            -0.2917573            -0.3166507             0.6250005             0.0357684            -0.3861778 
# 
# "LaLiga"
# (Intercept)          Score.Diff-1          Score.Diff-2          Score.Diff-3 Score.Diff-4.or.worse           Score.Diff1 
# -2.1662881             0.1861254             0.2968751             0.3259233             0.3457899            -0.1769552 
# Score.Diff2           Score.Diff3 Score.Diff4.or.better     Weighted.Win.Prob          HomeAwayHome          RedCard.Diff 
# -0.2165127            -0.1968633            -0.2294158             0.5451070             0.0916279            -0.3946903 
# 
# "Ligue1"
# (Intercept)          Score.Diff-1          Score.Diff-2          Score.Diff-3 Score.Diff-4.or.worse           Score.Diff1 
# -2.15911434            0.16270007            0.22009088            0.27690461            0.18596837           -0.17123665 
# Score.Diff2           Score.Diff3 Score.Diff4.or.better     Weighted.Win.Prob          HomeAwayHome          RedCard.Diff 
# -0.16613651           -0.20089679           -0.22560180            0.51653638            0.06563454           -0.39882464 
# 
# "PremierLeague"
# (Intercept)          Score.Diff-1          Score.Diff-2          Score.Diff-3 Score.Diff-4.or.worse           Score.Diff1 
# -2.12826436            0.17354533            0.22102516            0.23788881            0.17449521           -0.15104983 
# Score.Diff2           Score.Diff3 Score.Diff4.or.better     Weighted.Win.Prob          HomeAwayHome          RedCard.Diff 
# -0.16424566           -0.22376255           -0.19027060            0.67536508            0.05223114           -0.49496809 




######
## For the "5 or more extreme INTO BASELINE"
######

# "Bundesliga"
# (Intercept)      Score.Diff-1      Score.Diff-2      Score.Diff-3      Score.Diff-4       Score.Diff1       Score.Diff2       Score.Diff3 
# -2.09413154        0.17027199        0.20926492        0.16121813        0.02273997       -0.07388493       -0.11765847       -0.14249372 
# Score.Diff4 Weighted.Win.Prob      HomeAwayHome      RedCard.Diff 
# -0.15013646        0.56771081        0.04749045       -0.40021202 
# 
# "SerieA"
# (Intercept)      Score.Diff-1      Score.Diff-2      Score.Diff-3      Score.Diff-4       Score.Diff1       Score.Diff2       Score.Diff3 
# -2.06032844        0.19053044        0.33291216        0.30989660        0.27079309       -0.19361137       -0.24766287       -0.28925416 
# Score.Diff4 Weighted.Win.Prob      HomeAwayHome      RedCard.Diff 
# -0.27974679        0.62112826        0.03609653       -0.38334180 
# 
# "LaLiga"
# (Intercept)      Score.Diff-1      Score.Diff-2      Score.Diff-3      Score.Diff-4       Score.Diff1       Score.Diff2       Score.Diff3 
# -2.16655204        0.18551025        0.29538890        0.32353363        0.35545057       -0.17567837       -0.21438040       -0.19388480 
# Score.Diff4 Weighted.Win.Prob      HomeAwayHome      RedCard.Diff 
# -0.21782828        0.53829757        0.09245533       -0.39306053 
# 
# "Ligue1"
# (Intercept)      Score.Diff-1      Score.Diff-2      Score.Diff-3      Score.Diff-4       Score.Diff1       Score.Diff2       Score.Diff3 
# -2.16007664        0.16349133        0.22071790        0.27739814        0.25999350       -0.17007376       -0.16480321       -0.19941159 
# Score.Diff4 Weighted.Win.Prob      HomeAwayHome      RedCard.Diff 
# -0.24376075        0.51499948        0.06580079       -0.39798141 
# 
# "PremierLeague"
# (Intercept)      Score.Diff-1      Score.Diff-2      Score.Diff-3      Score.Diff-4       Score.Diff1       Score.Diff2       Score.Diff3 
# -2.12908872        0.17430821        0.22147459        0.23802144        0.22517787       -0.14951205       -0.16236983       -0.22160580 
# Score.Diff4 Weighted.Win.Prob      HomeAwayHome      RedCard.Diff 
# -0.20932422        0.67301844        0.05201755       -0.49241004 