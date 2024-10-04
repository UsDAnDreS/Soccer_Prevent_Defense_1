#######
## RESORTING TO LASSO, as STEPWISE TAKES TOO LONG
#######


library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(mpath)
library(zic)
library(pscl)


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")



for (league in league.name[2:5]){
  
  cat("\n")
  cat("\n")
  cat("\n")
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
                    data=final.df %>% mutate(ID = factor(ID), Score.Diff = relevel(factor(Score.Diff), ref="0")))[,-1]
  y <- final.df$Corners

  
  m1 <- zipath(y~.|.,data = data.frame(y,X), family = "poisson"
         #       , nlambda=100,
         # lambda.zero.min.ratio=0.001, maxit.em=300, maxit.theta=25,
         # theta.fixed=FALSE, trace=FALSE, penalty="enet", rescale=FALSE
         )

  save(m1, file=paste0("m1_object_", league, ".Robj"))

  best.BIC.ind <- which.min(BIC(m1))
  # m1$nlambda

  cat("\n")
  print("Count BIC:")
  print(m1$coefficients$count[,best.BIC.ind])
  cat("\n")
  print("Zero BIC:")
  print(m1$coefficients$zero[,best.BIC.ind])
  
  
  # m1.cv <- cv.zipath(y~.|.,data = data.frame(y,X), family = "poisson",
  #                   nfolds=10, trace=T)
  # 
  # save(m1.cv, file=paste0("m1_cv_object_", league, ".Robj"))
}


######
## CORNERS, BUNDESLIGA:
######


# [1] "Count BIC:"
# (Intercept)      Score.Diff.8      Score.Diff.7      Score.Diff.6      Score.Diff.5 
# -2.52756956        0.00000000        0.00000000        0.00000000        0.00000000 
# Score.Diff.4      Score.Diff.3      Score.Diff.2      Score.Diff.1       Score.Diff1 
# 0.00000000        0.10847054        0.21206121        0.16924144       -0.15295311 
# Score.Diff2       Score.Diff3       Score.Diff4       Score.Diff5       Score.Diff6 
# -0.24682319       -0.20575350       -0.15375585       -0.13993503        0.00000000 
# Score.Diff7       Score.Diff8      Minute.clean Weighted.Win.Prob      HomeAwayHome 
# 0.00000000        0.00000000        0.00183152        0.60722602        0.03996291 
# RedCard.Diff 
# -0.32808240 
# 
# [1] "Zero BIC:"
# (Intercept)      Score.Diff.8      Score.Diff.7      Score.Diff.6      Score.Diff.5 
# -0.2450933         0.0000000         0.0000000         0.0000000         0.0000000 
# Score.Diff.4      Score.Diff.3      Score.Diff.2      Score.Diff.1       Score.Diff1 
# 0.0000000         0.0000000         0.0000000         0.0000000         0.0000000 
# Score.Diff2       Score.Diff3       Score.Diff4       Score.Diff5       Score.Diff6 
# 0.0000000         0.0000000         0.0000000         0.0000000         0.0000000 
# Score.Diff7       Score.Diff8      Minute.clean Weighted.Win.Prob      HomeAwayHome 
# 0.0000000         0.0000000         0.0000000         0.0000000         0.0000000 
# RedCard.Diff 
# 0.0000000 



# [1] "SerieA"
# [1] 947226     17
# 
# [1] "Count BIC:"
# (Intercept)      Score.Diff.7      Score.Diff.6      Score.Diff.5      Score.Diff.4 
# -2.407511175       0.000000000       0.303437850       0.155096138       0.118191559 
# Score.Diff.3      Score.Diff.2      Score.Diff.1       Score.Diff1       Score.Diff2 
# 0.323563687       0.295073951       0.238404747      -0.298721714      -0.454558461 
# Score.Diff3       Score.Diff4       Score.Diff5       Score.Diff6       Score.Diff7 
# -0.517542424      -0.501076789      -0.706047183      -0.581719267      -1.578634501 
# Minute.clean Weighted.Win.Prob      HomeAwayHome      RedCard.Diff 
# 0.001501687       0.655426054       0.059579882      -0.331097168 
# 
# [1] "Zero BIC:"
# (Intercept)      Score.Diff.7      Score.Diff.6      Score.Diff.5      Score.Diff.4 
# -0.1085833         0.0000000         0.0000000         0.0000000         0.0000000 
# Score.Diff.3      Score.Diff.2      Score.Diff.1       Score.Diff1       Score.Diff2 
# 0.0000000         0.0000000         0.0000000         0.0000000         0.0000000 
# Score.Diff3       Score.Diff4       Score.Diff5       Score.Diff6       Score.Diff7 
# 0.0000000         0.0000000         0.0000000         0.0000000         0.0000000 
# Minute.clean Weighted.Win.Prob      HomeAwayHome      RedCard.Diff 
# 0.0000000         0.0000000         0.0000000         0.0000000 
# 
# 
# 
# [1] "LaLiga"
# [1] 863564     17
# 
# [1] "Count BIC:"
# (Intercept)      Score.Diff.8      Score.Diff.7      Score.Diff.6      Score.Diff.5 
# -2.471689959       0.000000000       0.000000000       0.571085560       0.000000000 
# Score.Diff.4      Score.Diff.3      Score.Diff.2      Score.Diff.1       Score.Diff1 
# 0.233141637       0.216261707       0.284822334       0.213644203      -0.256249545 
# Score.Diff2       Score.Diff3       Score.Diff4       Score.Diff5       Score.Diff6 
# -0.390920298      -0.393434549      -0.217402201      -0.286706697      -0.090620982 
# Score.Diff7       Score.Diff8      Minute.clean Weighted.Win.Prob      HomeAwayHome 
# -0.182766990       0.000000000       0.001062555       0.532171032       0.141353609 
# RedCard.Diff 
# -0.299438653 
# 
# [1] "Zero BIC:"
# (Intercept)      Score.Diff.8      Score.Diff.7      Score.Diff.6      Score.Diff.5 
# -0.1632627         0.0000000         0.0000000         0.0000000         0.0000000 
# Score.Diff.4      Score.Diff.3      Score.Diff.2      Score.Diff.1       Score.Diff1 
# 0.0000000         0.0000000         0.0000000         0.0000000         0.0000000 
# Score.Diff2       Score.Diff3       Score.Diff4       Score.Diff5       Score.Diff6 
# 0.0000000         0.0000000         0.0000000         0.0000000         0.0000000 
# Score.Diff7       Score.Diff8      Minute.clean Weighted.Win.Prob      HomeAwayHome 
# 0.0000000         0.0000000         0.0000000         0.0000000         0.0000000 
# RedCard.Diff 
# 0.0000000 
# 
# 
# 
# [1] "Ligue1"
# [1] 979818     17
# 
# [1] "Count BIC:"
# (Intercept)      Score.Diff.9      Score.Diff.8      Score.Diff.7      Score.Diff.6 
# -2.433973607       0.000000000       0.000000000       0.000000000       0.000000000 
# Score.Diff.5      Score.Diff.4      Score.Diff.3      Score.Diff.2      Score.Diff.1 
# 0.000000000       0.052331576       0.213161453       0.233320232       0.197779604 
# Score.Diff1       Score.Diff2       Score.Diff3       Score.Diff4       Score.Diff5 
# -0.252589830      -0.336490353      -0.361503541      -0.217285295       0.000000000 
# Score.Diff6       Score.Diff7       Score.Diff8       Score.Diff9      Minute.clean 
# 0.000000000       0.000000000       0.000000000       0.000000000       0.001429369 
# Weighted.Win.Prob      HomeAwayHome      RedCard.Diff 
# 0.525405559       0.079543686      -0.320806877 
# 
# [1] "Zero BIC:"
# (Intercept)      Score.Diff.9      Score.Diff.8      Score.Diff.7      Score.Diff.6 
# -0.01889068        0.00000000        0.00000000        0.00000000        0.00000000 
# Score.Diff.5      Score.Diff.4      Score.Diff.3      Score.Diff.2      Score.Diff.1 
# 0.00000000        0.00000000        0.00000000        0.00000000        0.00000000 
# Score.Diff1       Score.Diff2       Score.Diff3       Score.Diff4       Score.Diff5 
# 0.00000000        0.00000000        0.00000000        0.00000000        0.00000000 
# Score.Diff6       Score.Diff7       Score.Diff8       Score.Diff9      Minute.clean 
# 0.00000000        0.00000000        0.00000000        0.00000000        0.00000000 
# Weighted.Win.Prob      HomeAwayHome      RedCard.Diff 
# 0.00000000        0.00000000        0.00000000 
# 
# 
# 
# [1] "PremierLeague"
# [1] 850236     17
# 
# [1] "Count BIC:"
# (Intercept)      Score.Diff.9      Score.Diff.8      Score.Diff.7      Score.Diff.6 
# -2.435116384       0.000000000       0.000000000       0.488420878      -0.382071892 
# Score.Diff.5      Score.Diff.4      Score.Diff.3      Score.Diff.2      Score.Diff.1 
# 0.000000000       0.075267130       0.201837804       0.201398860       0.190431208 
# Score.Diff1       Score.Diff2       Score.Diff3       Score.Diff4       Score.Diff5 
# -0.208418612      -0.238040512      -0.351686113      -0.262130352       0.000000000 
# Score.Diff6       Score.Diff7       Score.Diff8       Score.Diff9      Minute.clean 
# -0.387755582      -0.076351479       0.000000000       0.000000000       0.002178155 
# Weighted.Win.Prob      HomeAwayHome      RedCard.Diff 
# 0.659914023       0.061278823      -0.387714356 
# 
# [1] "Zero BIC:"
# (Intercept)      Score.Diff.9      Score.Diff.8      Score.Diff.7      Score.Diff.6 
# -0.08092225        0.00000000        0.00000000        0.00000000        0.00000000 
# Score.Diff.5      Score.Diff.4      Score.Diff.3      Score.Diff.2      Score.Diff.1 
# 0.00000000        0.00000000        0.00000000        0.00000000        0.00000000 
# Score.Diff1       Score.Diff2       Score.Diff3       Score.Diff4       Score.Diff5 
# 0.00000000        0.00000000        0.00000000        0.00000000        0.00000000 
# Score.Diff6       Score.Diff7       Score.Diff8       Score.Diff9      Minute.clean 
# 0.00000000        0.00000000        0.00000000        0.00000000        0.00000000 
# Weighted.Win.Prob      HomeAwayHome      RedCard.Diff 
# 0.00000000        0.00000000        0.00000000