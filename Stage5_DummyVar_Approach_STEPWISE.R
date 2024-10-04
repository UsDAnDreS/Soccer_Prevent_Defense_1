#######
## 1. DO A FORWARD SELECTION INSTEAD OF BACKWARDS... TAKES WAY TOO LONG
## 2. EVEN FORWARD SELECTION TAKES IMPOSSIBLY LONG (models with just the intercept, or just 1 predictor - take TOO LONG TO RUN..)
#######


library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(mpath)
library(zic)
library(pscl)


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")



for (league in league.name){
  
  print(league)
  
  our.df <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  ## REMOVING NA betting data
  our.df <- our.df %>% filter(!is.na(Weighted.Win.Prob))
  dim(our.df)
  
  ## Remove extra time data from first half (so anything with half_id=1, minute>45)
  ## => TO AVOID "POINT MASS"/"OUT-OF-ORDER MINUTE" ISSUES...
  ##
  ## !!! BUUUT... the >90 IS ALSO AN ISSUE... data is way too sporadic over there !!!
  final.df <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
  print(dim(final.df))
  
  
  #########
  ## Fitting the Neg Bin regression with:
  ##    * Score.Diff as DUMMY variables with "0" as reference level 
  ##      (achieved via mutate(Score.Diff = relevel(factor(Score.Diff), ref="0")))
  ##    * Making effects of Weighted.Win.Prob & RedCard.Diff linear
  #########
  
  # gam.no.int.offset.obj <- gam(Shots ~ Score.Diff + Minute.clean + Weighted.Win.Prob  + HomeAway + RedCard.Diff,
  #                              family="ziP",
  #                              data=final.df %>% mutate(ID = factor(ID), Score.Diff = relevel(factor(Score.Diff), ref="0")))
  # 
  # ## Extracting the design matrix with dummy variables being treated as separate predictors
  # X <- model.matrix(gam.no.int.offset.obj)[,-1]
  # y <- gam.no.int.offset.obj$y
  
  
  ### TO AVOID FITTING THE FULL MODEL:
  
  X <- model.matrix(~Score.Diff + Minute.clean + Weighted.Win.Prob  + HomeAway + RedCard.Diff,
               data=final.df %>% mutate(ID = factor(ID), Score.Diff = relevel(factor(Score.Diff), ref="0")))
  y <- final.df$Shots
  
  ## FITTING THE NULL MODEL:
  
  # gam.null.obj <- gam(Shots ~ 1, 
  #                     family="ziP",
  #                     data=final.df)
  #                   
  # save(gam.null.obj, file="gam_null_obj.Robj") 
  load("gam_null_obj.Robj")
  
  ####
  ## BACKWARD SELECTION TAKES WAY TOO MUCH TIME;
  ##  DO FORWARD SELECTION INSTEAD
  ####
  
  best.curr.BIC <- BIC(gam.null.obj)
  colnames(X) <- str_replace(colnames(X), "-", ".")
  all.vars <- colnames(X)
  
  # curr.set <- all.vars
  curr.set <- c()
  remaining.set <- all.vars
  
  
  while (TRUE){
  
    curr.BICs <- c()
    
  for (j in 1:length(remaining.set)){
    gam.no.int.offset.obj <- gam(formula(paste("y ~ ",paste(c(curr.set, remaining.set[j]), collapse =" + "))),
                                 family="ziP",
                                 data=data.frame(y, X))
    
    this.BIC <- BIC(gam.no.int.offset.obj)
    curr.BICs <- c(curr.BICs, this.BIC)
    print(remaining.set[j])
    print(this.BIC)
  }
    
    if (min(curr.BICs) <= best.curr.BIC){
      cat("\n")
      cat("\n")
      print(paste0("VARIABLE TO ADD: ", remaining.set[which.min(curr.BICs)]))
      cat("\n")
      cat("\n")
      
      curr.set <- c(curr.set, remaining.set[which.min(curr.BICs)])
      remaining.set <- remaining.set[-which.min(curr.BICs)]
      best.curr.BIC <- min(curr.BICs)
      
    } else {
      break;
    }
    
  }
  
  

  
  
  
  m1 <- zeroinfl(y ~ Score.Diff.8 + Score.Diff.7 + Score.Diff.6 + Score.Diff.5 + Score.Diff.4 + 
                   Score.Diff.3 + Score.Diff.2 + Score.Diff.1 + Score.Diff1 + Score.Diff2 + 
                   Score.Diff3 + Score.Diff4 + Score.Diff5 + Score.Diff6 + Score.Diff7 + 
                   Score.Diff8 + Weighted.Win.Prob + HomeAwayHome + RedCard.Diff + offset(gam.no.int.offset.obj$offset), 
                 data=data.frame(X), dist="poisson")
  
  m1
  be.zeroinfl(m1, data=data.frame(X))
  
  
  
  
  ## Fitting the negative binomial
  gam.model.mat <- gam(y ~ Score.Diff.8 + Score.Diff.7 + Score.Diff.6 + Score.Diff.5 + Score.Diff.4 + 
                         Score.Diff.3 + Score.Diff.2 + Score.Diff.1 + Score.Diff1 + Score.Diff2 + 
                         Score.Diff3 + Score.Diff4 + Score.Diff5 + Score.Diff6 + Score.Diff7 + 
                         Score.Diff8 + Weighted.Win.Prob + HomeAwayHome + RedCard.Diff + offset(gam.no.int.offset.obj$offset), 
                       family="ziP",
                       data=data.frame(X), select=TRUE)
  gam.model.mat
  
  ## Using stepwise selection with BIC as the criteria (leads to a simpler model than AIC)
  step.obj <- step.gam(gam.model.mat, k=log(length(y))
                   , trace=T
  )
  print(step.obj)
  
}
