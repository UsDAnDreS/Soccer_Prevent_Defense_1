library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(plotrix)
library(gridExtra)
library(ggpubr)


##########
##########
### WITH MINUTE
##########
##########


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we use models where "Minute.clean" was included
include.minute <- TRUE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

# load(file="gam_ziP_obj_Shots.Robj")

load(file=paste0(ifelse(include.minute,
                        "",
                        "NO_MINUTES_TEST_"),
                 #"gam_ziP_obj",  
                 "gam_nb_obj",  
                 ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
                 "_Shots.Robj"))



max.scorediff <- 3
score.diff.names <- c(paste0("Score Diff = -",c(max.scorediff:1)), "Score Diff =  0", paste0("Score Diff = +",c(1:max.scorediff)))
redcard.diff.names <- c(paste0("RedCard Diff = -", c(2:1)), "RedCard Diff =  0", paste0("RedCard Diff = +", c(1:2)))
all.coef.names <- c(score.diff.names, redcard.diff.names, "Away")


coef.matrix.with.minute <- matrix(0, nrow=5, ncol=length(all.coef.names))
colnames(coef.matrix.with.minute) <- all.coef.names
proper.league.names <- c("Bundesliga", "Serie A", "La Liga", "Ligue 1", "Premier League")
rownames(coef.matrix.with.minute) <- proper.league.names

coef.matrix.no.minute <- coef.matrix.with.minute


######
## WITH MINUTE calculations
######

for (league in league.name){
  
  print(league)
  
  cat("\n")
  
  #######
  ### LOADING, SETTING UP DATA
  #######
  
  #  our.df <- read.csv(paste0(league, "_NO_EXTRA_TIME_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  ## To be used for prediction (including everything, so that totals correspond to full data)
  our.df.pred <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  # FOR DATA FORMAT PLOTTING in SLIDE PRESENTATIONS
  # View(our.df %>% rename(Win.Prob.Diff = Weighted.Win.Prob, Home=HomeAway, mins.spent=minutes.spent) %>% dplyr::select(gameId, mins.spent, Score.Diff, RedCard.Diff, Home, Win.Prob.Diff, Shots))
  
  
  
  # ## REMOVING NA betting data
  # our.df <- our.df %>% filter(!is.na(Weighted.Win.Prob))
  # dim(our.df)
  # 
  # ## For EXTRA TIME: JUST FORCE IT TO BE EQUAL TO THE LAST MINUTE (e.g. for extra time in 1st half - make it =45, in 2nd half - =90)
  # 
  # # our.df$Minute.clean[our.df$half_id == 1 & our.df$Minute.clean > 45] <- 45
  # # our.df$Minute.clean[our.df$Minute.clean > 90] <- 90
  # 
  # 
  # our.df  <- our.df  %>%
  #   mutate(abs.Score.Diff = abs(Score.Diff),
  #          abs.RedCard.Diff = abs(RedCard.Diff))
  # 
  # our.df  <- our.df  %>%
  #   mutate(Period.ID = group_indices(our.df, .dots=c("ID", "Minute.clean", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
  #   arrange(Period.ID) %>%
  #   mutate(Period.ID = factor(Period.ID))
  # 
  # our.df <- our.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
  
  
  #######
  ### GETTING ADJUSTMENTS
  #######
  
  
  
  ### 1. SCORE DIFFERENTIAL
  
  # Getting the DIFFERENTIALS (log-scale "linear" effects)
  # all.score.diffs <- c(min(our.df.pred$Score.Diff):max(our.df.pred$Score.Diff))
  all.score.diffs <- c(-max.scorediff:max.scorediff)
  log.score.diff.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                               newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                                                    Score.Diff = all.score.diffs))) -
    as.numeric(predict(gam.nb.obj[[league]],
                       newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                            Score.Diff = rep(0, length(all.score.diffs)))))
  
  names(log.score.diff.effects) <- all.score.diffs
  log.score.diff.effects
  
  response.score.diff.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                                    newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                                                         Score.Diff = all.score.diffs),
                                                    type = "response")) -
    as.numeric(predict(gam.nb.obj[[league]],
                       newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                            Score.Diff = rep(0, length(all.score.diffs))),
                       type = "response"))
  response.score.diff.effects
  exp(log.score.diff.effects)
  
  
  
  
  ### 2. RED CARD DIFF
  
  # all.redcard.diffs <- c(min(our.df.pred$RedCard.Diff):max(our.df.pred$RedCard.Diff))
  all.redcard.diffs <- c(-2:2)
  log.redcard.diff.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                                 newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                                                      RedCard.Diff = all.redcard.diffs))) -
    as.numeric(predict(gam.nb.obj[[league]],
                       newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                            RedCard.Diff = rep(0, length(all.redcard.diffs)))))
  
  names(log.redcard.diff.effects) <- all.redcard.diffs
  log.redcard.diff.effects
  
  
  ### 3. HOME/AWAY FACTOR
  
  all.homeaway <- unique(our.df.pred$HomeAway)
  log.homeaway.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                             newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                                                  HomeAway = all.homeaway))) -
    as.numeric(predict(gam.nb.obj[[league]],
                       newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                            HomeAway = rep("Home", length(all.homeaway)))))
  
  names(log.homeaway.effects) <- all.homeaway
  log.homeaway.effects
  
  
  # league.coefs <- stepwise.obj[[league.name[j]]]$coefficients[-1]
  
  # coef.names.league <- names(league.coefs)
  coef.matrix.with.minute[which(league.name == league), ] <- c(exp(log.score.diff.effects), 
                                                               exp(log.redcard.diff.effects),
                                                               exp(log.homeaway.effects)[2])
  
  
}

##########
### WITHOUT MINUTE calculations
##########

# gam.nb.obj <- list()
# 
# for (league in league.name){
#   
#   print(league)
#   
#   cat("\n")
#   
#   
#   ## To be used for modeling (excluding bad minute entries)
#   our.df <- read.csv(paste0(league, "_NO_EXTRA_TIME_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
#   
#   
#   # FOR DATA FORMAT PLOTTING in SLIDE PRESENTATIONS
#   # View(our.df %>% rename(Win.Prob.Diff = Weighted.Win.Prob, Home=HomeAway, mins.spent=minutes.spent) %>% dplyr::select(gameId, mins.spent, Score.Diff, RedCard.Diff, Home, Win.Prob.Diff, Shots))
#   
#   
#   
#   ## REMOVING NA betting data
#   our.df <- our.df %>% filter(!is.na(Weighted.Win.Prob))
#   dim(our.df)
#   
#   ## For EXTRA TIME: JUST FORCE IT TO BE EQUAL TO THE LAST MINUTE (e.g. for extra time in 1st half - make it =45, in 2nd half - =90)
#   
#   # our.df$Minute.clean[our.df$half_id == 1 & our.df$Minute.clean > 45] <- 45
#   # our.df$Minute.clean[our.df$Minute.clean > 90] <- 90
#   
#   
#   our.df  <- our.df  %>%
#     mutate(abs.Score.Diff = abs(Score.Diff),
#            abs.RedCard.Diff = abs(RedCard.Diff))
#   
#   our.df  <- our.df  %>%
#     mutate(Period.ID = group_indices(our.df, .dots=c("ID", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
#     arrange(Period.ID) %>%
#     mutate(Period.ID = factor(Period.ID))
#   
#   our.df <- our.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
#   
#   
#   ######
#   ### Fitting the models
#   ######
#   
#   gam.nb.obj[[league]] <- gam( Shots ~
#                                  s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5) + log(minutes.spent + 1),
#                                family = "nb", data= our.df)
#   
#   
#   
#   #######
#   ### GETTING ADJUSTMENTS
#   #######
#   
#   
#   ### 1. SCORE DIFFERENTIAL
#   
#   # Getting the DIFFERENTIALS (log-scale "linear" effects)
#   all.score.diffs <- c(-max.scorediff:max.scorediff)
#   log.score.diff.effects <- as.numeric(predict(gam.nb.obj[[league]],
#                                                newdata = data.frame(minutes.spent=90, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
#                                                                     Score.Diff = all.score.diffs))) -
#     as.numeric(predict(gam.nb.obj[[league]],
#                        newdata = data.frame(minutes.spent=90, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
#                                             Score.Diff = rep(0, length(all.score.diffs)))))
#   
#   names(log.score.diff.effects) <- all.score.diffs
#   log.score.diff.effects
#   
#   
#   
#   ### 2. RED CARD DIFF
#   
#   all.redcard.diffs <- c(-2:2)
#   log.redcard.diff.effects <- as.numeric(predict(gam.nb.obj[[league]],
#                                                  newdata = data.frame(minutes.spent=90, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
#                                                                       RedCard.Diff = all.redcard.diffs))) -
#     as.numeric(predict(gam.nb.obj[[league]],
#                        newdata = data.frame(minutes.spent=90, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
#                                             RedCard.Diff = rep(0, length(all.redcard.diffs)))))
#   
#   names(log.redcard.diff.effects) <- all.redcard.diffs
#   log.redcard.diff.effects
#   
#   
#   
#   ### 3. HOME/AWAY FACTOR
#   
#   all.homeaway <- unique(our.df.pred$HomeAway)
#   log.homeaway.effects <- as.numeric(predict(gam.nb.obj[[league]],
#                                              newdata = data.frame(minutes.spent=90, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
#                                                                   HomeAway = all.homeaway))) -
#     as.numeric(predict(gam.nb.obj[[league]],
#                        newdata = data.frame(minutes.spent=90, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
#                                             HomeAway = rep("Home", length(all.homeaway)))))
#   
#   names(log.homeaway.effects) <- all.homeaway
#   log.homeaway.effects
#   
#   
#   coef.matrix.no.minute[which(league.name == league), ] <- c(exp(log.score.diff.effects), 
#                                                              exp(log.redcard.diff.effects),
#                                                              exp(log.homeaway.effects)[2])
#   
#   
# }



########
### ALL THE VARIABLES
########

par(mar = c(2.1, 10, 4.0, 2.1))

color2D.matplot(1/t(coef.matrix.with.minute),
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)
par(las=1)
axis(2,at=c(ncol(coef.matrix.with.minute):1)-0.5,labels=colnames(coef.matrix.with.minute))
# staxlab(2,at=rev(c(1:ncol(coef.matrix.with.minute)))-0.25,labels=colnames(coef.matrix.with.minute))
par(las=1)
axis(3,at=c(nrow(coef.matrix.with.minute):1)-0.5,labels=rownames(coef.matrix.with.minute))
title("Min-by-min, WITH MINUTE\n\n")
par(mar = c(5.1, 4.1, 4.1, 2.1))




# par(mar = c(2.1, 10, 4.0, 2.1))
# 
# color2D.matplot(1/t(coef.matrix.no.minute),
#                 cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
#                 show.legend=F,
#                 show.values=2,
#                 xlab='',
#                 ylab='',
#                 axes=F)
# par(las=1)
# axis(2,at=c(ncol(coef.matrix.no.minute):1)-0.5,labels=colnames(coef.matrix.no.minute))
# # staxlab(2,at=rev(c(1:ncol(coef.matrix.no.minute)))-0.25,labels=colnames(coef.matrix.no.minute))
# par(las=1)
# axis(3,at=c(nrow(coef.matrix.no.minute):1)-0.5,labels=rownames(coef.matrix.no.minute))
# title("No Minute\n\n")
# par(mar = c(5.1, 4.1, 4.1, 2.1))







########
### ONLY SCORE DIFFERENTIALS
########



coef.matrix.with.minute.only.score <- coef.matrix.with.minute[, str_detect(colnames(coef.matrix.with.minute), "Score")]
coef.matrix.no.minute.only.score <- coef.matrix.no.minute[, str_detect(colnames(coef.matrix.no.minute), "Score")]


## Width: 817; Height: 222

par(mar = c(2.1, 10, 2.0, 2.1))

color2D.matplot(1/t(coef.matrix.with.minute.only.score),
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)
par(las=1)
axis(2,at=c(ncol(coef.matrix.with.minute.only.score):1)-0.5,labels=colnames(coef.matrix.with.minute.only.score))
# staxlab(2,at=rev(c(1:ncol(coef.matrix.with.minute.only.score)))-0.25,labels=colnames(coef.matrix.with.minute.only.score))
par(las=1)
axis(3,at=c(nrow(coef.matrix.with.minute.only.score):1)-0.5,labels=rownames(coef.matrix.with.minute.only.score))
#title("Min-by-min, WITH MINUTE\n\n")
par(mar = c(5.1, 4.1, 4.1, 2.1))




# par(mar = c(2.1, 10, 4.0, 2.1))
# 
# color2D.matplot(1/t(coef.matrix.no.minute.only.score),
#                 cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
#                 show.legend=F,
#                 show.values=2,
#                 xlab='',
#                 ylab='',
#                 axes=F)
# par(las=1)
# axis(2,at=c(ncol(coef.matrix.no.minute.only.score):1)-0.5,labels=colnames(coef.matrix.no.minute.only.score))
# # staxlab(2,at=rev(c(1:ncol(coef.matrix.no.minute.only.score)))-0.25,labels=colnames(coef.matrix.no.minute.only.score))
# par(las=1)
# axis(3,at=c(nrow(coef.matrix.no.minute.only.score):1)-0.5,labels=rownames(coef.matrix.no.minute.only.score))
# title("No Minute\n\n")
# par(mar = c(5.1, 4.1, 4.1, 2.1))








########
### ONLY RED CARD DIFFERENTIALS
########

coef.matrix.with.minute.only.redcard <- coef.matrix.with.minute[, str_detect(colnames(coef.matrix.with.minute), "RedCard")]
coef.matrix.no.minute.only.redcard <- coef.matrix.no.minute[, str_detect(colnames(coef.matrix.no.minute), "RedCard")]


## Width: 817; Height: 222

par(mar = c(2.1, 10, 2.0, 2.1))

color2D.matplot(1/t(coef.matrix.with.minute.only.redcard),
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)
par(las=1)
axis(2,at=c(ncol(coef.matrix.with.minute.only.redcard):1)-0.5,labels=colnames(coef.matrix.with.minute.only.redcard))
# staxlab(2,at=rev(c(1:ncol(coef.matrix.with.minute.only.redcard)))-0.25,labels=colnames(coef.matrix.with.minute.only.redcard))
par(las=1)
axis(3,at=c(nrow(coef.matrix.with.minute.only.redcard):1)-0.5,labels=rownames(coef.matrix.with.minute.only.redcard))
#title("Min-by-min, WITH MINUTE\n\n")
par(mar = c(5.1, 4.1, 4.1, 2.1))




# par(mar = c(2.1, 10, 4.0, 2.1))
# 
# color2D.matplot(1/t(coef.matrix.no.minute.only.redcard),
#                 cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
#                 show.legend=F,
#                 show.values=2,
#                 xlab='',
#                 ylab='',
#                 axes=F)
# par(las=1)
# axis(2,at=c(ncol(coef.matrix.no.minute.only.redcard):1)-0.5,labels=colnames(coef.matrix.no.minute.only.redcard))
# # staxlab(2,at=rev(c(1:ncol(coef.matrix.no.minute.only.redcard)))-0.25,labels=colnames(coef.matrix.no.minute.only.redcard))
# par(las=1)
# axis(3,at=c(nrow(coef.matrix.no.minute.only.redcard):1)-0.5,labels=rownames(coef.matrix.no.minute.only.redcard))
# title("No Minute\n\n")
# par(mar = c(5.1, 4.1, 4.1, 2.1))
