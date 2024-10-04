library(mgcv)
library(tidyverse)
library(splines)
library(MASS)


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


# our.df <- read.csv("A7_Bundesliga_FINAL.csv")

length(unique(our.df$gameId))
length(unique(final.df$gameId))
  
  print(league)
  
  # load("final_df_w_red_cards.csv")
  
  
#######
### MINE PART
#######
  
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
                   , trace=T
  )
  print(step.obj)
  
###########
#### SAMI'S PART
###########
  
  our.df <- read.csv("A7_Bundesliga_FINAL.csv")
  
  glm.no.int.offset.obj <- glm.nb(ShotAttempt ~ ScoreDiff + WeightedWinProb  + H_A + RedCardDiff + offset(log(timeSpent+1)),
                                  data=our.df %>% mutate(gameId = factor(gameId), ScoreDiff = relevel(factor(ScoreDiff), ref="0")))
  glm.no.int.offset.obj
  
  ## Extracting the design matrix with dummy variables being treated as separate predictors
  X <- model.matrix(glm.no.int.offset.obj)[,-1]
  y <- glm.no.int.offset.obj$y
  
  ## Fitting the negative binomial
  glm.model.mat <- glm.nb(y ~ . + offset(glm.no.int.offset.obj$offset), data=data.frame(X))
  glm.model.mat
  
  ## Using stepwise selection with BIC as the criteria (leads to a simpler model than AIC)
  step.obj <- step(glm.model.mat, k=log(length(y))
                   , trace=T
  )
  print(step.obj)
  

  #####################
  ######################

our.ids <- unique(our.df$gameId)[!unique(our.df$gameId) %in% unique(final.df$gameId)]
final.ids <- unique(final.df$gameId)[!unique(final.df$gameId) %in% unique(our.df$gameId)]

View(our.df[our.df$gameId %in% our.ids,])
View(final.df[final.df$gameId %in% final.ids,])

summary(final.df)



###

our.df.common <- our.df[!our.df$gameId %in% our.ids, ]
final.df.common <- final.df[!final.df$gameId %in% final.ids, ]
dim(our.df[!our.df$gameId %in% our.ids, ])
dim(final.df[!final.df$gameId %in% final.ids, ])

View(head(our.df.common,10))
View(head(final.df.common,10))

class(our.df.common$gameId)
class(final.df.common$gameId)

our.df.common <- our.df.common %>% arrange(gameId)
final.df.common <- final.df.common %>% arrange(gameId)

all(our.df.common$gameId == final.df.common$gameId)
all(our.df.common$ScoreDiff == final.df.common$Score.Diff)
all(our.df.common$RedCardDiff == final.df.common$RedCard.Diff)
all(our.df.common$Goal == final.df.common$Goals)
all(our.df.common$Corner == final.df.common$Corners)
all(our.df.common$ShotAttempt == final.df.common$Shots)


hist(our.df.common$timeSpent - final.df.common$minutes.spent)
length(our.df.common$timeSpent - final.df.common$minutes.spent)
sum(abs(our.df.common$timeSpent - final.df.common$minutes.spent)>20)

