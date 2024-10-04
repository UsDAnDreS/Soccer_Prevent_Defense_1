library(tidyverse)
library(mgcv)
library(MASS)
library(lme4)
library(splines)

# our.df <- read.csv("Bundesliga_Complete_MinuteByMinute_Dataset.csv")
# print(dim(our.df))

our.df <- read.csv("minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv")
# final.df <- read.csv("intermed_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv")

## Remove extra time data from first half (so anything with half_id=1, minute>45)
## => TO AVOID "POINT MASS"/"OUT-OF-ORDER MINUTE" ISSUES...
##
## !!! BUUUT... the >90 IS ALSO AN ISSUE... data is way too sporadic over there !!!
our.df.cut <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
print(dim(our.df.cut))

View(our.df.cut)



############
############

our.df.cut  <- our.df.cut  %>%
  mutate(abs.Score.Diff = abs(Score.Diff),
         abs.RedCard.Diff = abs(RedCard.Diff))

our.df.cut  <- our.df.cut  %>%
  mutate(Period.ID = group_indices(our.df.cut, .dots=c("ID", "Minute.clean", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
  arrange(Period.ID)


View(head(our.df.cut, 100))
table(table(our.df.cut$Period.ID))



#########
### Using GAMMs, with corAR1, and "minute | gameId" formula
#########

library(mgcv)

# corAR1()

# gamm.obj <- gamm(ShotAttempt ~ s(ScoreDiff) + s(Minute.clean) + s(Weighted.Win.Prob)  + Home.Away + s(RedCardDiff, k=5),
#      correlation = corAR1(form=~Minute.clean|gameId/Home.Away),
#      data = our.df.cut,
#      family = "binomial")


gamm.obj <- gamm(Shots ~ s(Score.Diff) + s(Minute.clean) 
                 + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=3) 
       # + (1 | gameId/Period.ID)
                 ,
                  random = list(Period.ID = ~1),
     #  random = list(gameId=~1| gameId),
                 #correlation = corAR1(form=~Minute.clean|gameId/factor(Score.Diff)/factor(RedCard.Diff)/HomeAway),
                 data = our.df.cut[1:10000, ] %>% mutate(gameId=factor(gameId), Period.ID = factor(Period.ID)),
                 family = "nb")

gamm.obj$lme
coef(gamm.obj$lme)
fixef(gamm.obj$lme)

#######
## list(gameId=~1| Period.ID)
#######

# Maximum number of PQL iterations:  20 
# iteration 1
# iteration 2
# iteration 3
# iteration 4
# iteration 5
# Error: cannot allocate vector of size 27.0 Gb








































##########
########
## Using SAME PERIOD ID (Period.ID)
########
##########

final.df <- our.df.cut[,]


## LINEAR


glm.regular <- glm(ShotAttempt ~ ScoreDiff + Weighted.Win.Prob  + Home.Away + RedCardDiff,
                   family = "binomial",
                   data= final.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID),
                                             # final.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID),
                                             #, Score.Diff = scale(Score.Diff), Weighted.Win.Prob = scale(Weighted.Win.Prob)
                   ))

glmer.regular <- glmer(ShotAttempt ~ ScoreDiff + Weighted.Win.Prob  + Home.Away + RedCardDiff 
                       + (1 | Period.ID)
                       ,
                       family = "binomial",
                       data= final.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID),
                                                 # final.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID),
                                                 #, Score.Diff = scale(Score.Diff), Weighted.Win.Prob = scale(Weighted.Win.Prob)
                       ))


## SPLINES

gam.regular <- glm(ShotAttempt ~ ns(ScoreDiff, df=4) + Weighted.Win.Prob  + Home.Away + RedCardDiff,
                   family = "binomial",
                   data= final.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID),
                                             # final.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID),
                                             #, Score.Diff = scale(Score.Diff), Weighted.Win.Prob = scale(Weighted.Win.Prob)
                   ))

gamer.regular <- glmer(ShotAttempt ~ ns(ScoreDiff, df=4) + Weighted.Win.Prob  + Home.Away + RedCardDiff 
                       + (1 | Period.ID)
                       ,
                       family = "binomial",
                       data= final.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID),
                                                 # final.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID),
                                                 #, Score.Diff = scale(Score.Diff), Weighted.Win.Prob = scale(Weighted.Win.Prob)
                       ))

