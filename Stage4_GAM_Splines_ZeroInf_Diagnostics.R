######
## "LeagueName_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"
######

library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")



for (league in league.name){
  
  print(league)
  
  our.df.cut <- read.csv(paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  ## REMOVING NA betting data
  our.df.cut <- our.df.cut %>% filter(!is.na(Weighted.Win.Prob))
  # dim(our.df.cut)
  
  ## Remove extra time data from first half (so anything with half_id=1, minute>45)
  ## => TO AVOID "POINT MASS"/"OUT-OF-ORDER MINUTE" ISSUES...
  ##
  ## !!! BUUUT... the >90 IS ALSO AN ISSUE... data is way too sporadic over there !!!
  # our.df.cut <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
  # print(dim(our.df.cut))
  
  
  
  ############
  ############
  
  our.df.cut  <- our.df.cut  %>%
    mutate(abs.Score.Diff = abs(Score.Diff),
           abs.RedCard.Diff = abs(RedCard.Diff))
  
  our.df.cut  <- our.df.cut  %>%
    mutate(Period.ID = group_indices(our.df.cut, .dots=c("ID", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
    arrange(Period.ID) %>%
    mutate(Period.ID = factor(Period.ID))
  
  
  
  
  ######
  ## Fitting regular Poisson, testing for overdispersion
  ######
  
  glm.obj <- glm(Shots ~ 
                   # Corners ~
                   ns(Score.Diff,df=4) + ns(Weighted.Win.Prob,df=4)  + HomeAway + ns(RedCard.Diff,df=4) + offset(log(minutes.spent+1)),
                 family = "poisson",
                 data= our.df.cut %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID)))
  
  simulationOutput <- simulateResiduals(fittedModel = glm.obj)
  # plot(simulationOutput)
  
  ZI.test <- testZeroInflation(simulationOutput)
  print(ZI.test$p.value)
}

######
## COMPARING:
##    Regular Poisson,
##    Negative binomial,
##    Zero-Inflated Poisson
######

glm.obj <- glm.nb.obj <- glm.ziP.obj <- list()
gam.obj <- gam.nb.obj <- gam.ziP.obj <- list()
# 
# load("gam_obj_Corners.Robj")
# load("gam_nb_obj_Corners.Robj")
# load("gam_ziP_obj_Corners.Robj")



for (league in league.name){
  
  print(league)
  
  our.df.cut <- read.csv(paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  ## REMOVING NA betting data
  our.df.cut <- our.df.cut %>% filter(!is.na(Weighted.Win.Prob))
  #dim(our.df)
  
  ## Remove extra time data from first half (so anything with half_id=1, minute>45)
  ## => TO AVOID "POINT MASS"/"OUT-OF-ORDER MINUTE" ISSUES...
  ##
  ## !!! BUUUT... the >90 IS ALSO AN ISSUE... data is way too sporadic over there !!!
  # our.df <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
  # print(dim(our.df))
  
  
  
  ############
  ############
  
  our.df.cut  <- our.df.cut  %>%
    mutate(abs.Score.Diff = abs(Score.Diff),
           abs.RedCard.Diff = abs(RedCard.Diff))
  
  our.df.cut  <- our.df.cut  %>%
    mutate(Period.ID = group_indices(our.df.cut, .dots=c("ID", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
    arrange(Period.ID) %>%
    mutate(Period.ID = factor(Period.ID))
  
  our.df.cut <- our.df.cut %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
  
  
  
  #####
  ## Fitting LINEAR models 
  #####
  
  # glm.obj[[league]] <- gam(Shots ~ 
  #                         # Corners ~ 
  #                            Score.Diff + Minute.clean + Weighted.Win.Prob  + HomeAway + RedCard.Diff,
  #                          family = "poisson",
  #                          data= our.df.cut)
  # 
  # glm.nb.obj[[league]] <- gam(Shots ~ 
  #                            #  Corners ~
  #                               Score.Diff + Minute.clean + Weighted.Win.Prob  + HomeAway + RedCard.Diff                            ,
  #                             family = "nb", data= our.df.cut)
  # 
  # glm.ziP.obj[[league]] <- gam(Shots ~ 
  #                               # Corners ~
  #                                Score.Diff + Minute.clean + Weighted.Win.Prob  + HomeAway + RedCard.Diff,
  #                              family="ziP", data= our.df.cut)
  
  
  
  
  ######
  ## Fitting SPLINE models
  ######
  
  gam.obj[[league]] <- gam(Shots ~ 
    #Corners ~
      s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5) + log(minutes.spent + 1),
    family = "poisson", data= our.df.cut)
  
  gam.nb.obj[[league]] <- gam(Shots ~ 
    #Corners ~
      s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5) + log(minutes.spent + 1),
    family = "nb", data= our.df.cut)
  
  gam.ziP.obj[[league]] <- gam(Shots ~ 
   # Corners ~
      s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5) + log(minutes.spent + 1),
    family="ziP", data= our.df.cut)
  
  # save(gam.obj, file="gam_obj_Corners.Robj")
  # save(gam.nb.obj, file="gam_nb_obj_Corners.Robj")
  # save(gam.ziP.obj, file="gam_ziP_obj_Corners.Robj")
  
  
  print(league)
  print(BIC(#glm.obj[[league]], glm.nb.obj[[league]], glm.ziP.obj[[league]],
    gam.obj[[league]], gam.nb.obj[[league]], gam.ziP.obj[[league]]))
  
  cat("\n")
}


# for (league in league.name){
#   print(league)
#   print(BIC(#glm.obj[[league]], glm.nb.obj[[league]], glm.ziP.obj[[league]],
#             gam.obj[[league]], gam.nb.obj[[league]], gam.ziP.obj[[league]]))
# 
#   cat("\n")
# }


### P-values for ZERO INFLATION TESTS:
#
# [1] "Bundesliga"
# [1] 0.576
# [1] "SerieA"
# [1] 0.08
# [1] "LaLiga"
# [1] 0.04
# [1] "Ligue1"
# [1] 0.144
# [1] "PremierLeague"
# [1] 0



######
## BIC
######

# [1] "Bundesliga"
# [1] "Bundesliga"
# df      BIC
# gam.obj[[league]]     17.40212 109742.6
# gam.nb.obj[[league]]  19.51889 109190.2
# gam.ziP.obj[[league]] 21.40417 109740.7
# 
# [1] "SerieA"
# [1] "SerieA"
# df      BIC
# gam.obj[[league]]     19.28949 124902.8
# gam.nb.obj[[league]]  20.50672 124226.4
# gam.ziP.obj[[league]] 22.59358 124871.2
# 
# [1] "LaLiga"
# [1] "LaLiga"
# df      BIC
# gam.obj[[league]]     19.40150 112349.0
# gam.nb.obj[[league]]  20.42820 111835.3
# gam.ziP.obj[[league]] 22.59872 112363.3
# 
# [1] "Ligue1"
# [1] "Ligue1"
# df      BIC
# gam.obj[[league]]     20.33397 123039.9
# gam.nb.obj[[league]]  18.22056 122565.2
# gam.ziP.obj[[league]] 20.47151 123006.0
# 
# [1] "PremierLeague"
# [1] "PremierLeague"
# df      BIC
# gam.obj[[league]]     23.88425 110926.6
# gam.nb.obj[[league]]  22.46106 110065.9
# gam.ziP.obj[[league]] 24.02702 110828.6


### NEGATIVE BINOMIAL IS BEST BY FAR ACROSS THE BOARD.