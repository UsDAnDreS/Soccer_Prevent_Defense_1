######
## "LeagueName_ifelse(remove.extra, "_NO_EXTRA_TIME", "")_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"
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


### EXPLICITLY TESTING FOR ZERO INFLATION (BARELY NEEDED, ALWAYS HIGHLY SIGNIFICANT)
for (league in league.name){

  print(league)

  our.df.cut <- read.csv(paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))


  ## REMOVING NA betting data
  our.df.cut <- our.df.cut %>% filter(!is.na(Weighted.Win.Prob))

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
    mutate(Period.ID = group_indices(our.df.cut, .dots=c("ID", "Minute.clean", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
    arrange(Period.ID) %>%
    mutate(Period.ID = factor(Period.ID))




  ######
  ## Fitting regular Poisson, testing for overdispersion
  ######

  glm.obj <- glm(# Shots ~
                  Corners ~
                   ns(Score.Diff,df=4) + ns(Minute.clean,df=4) + ns(Weighted.Win.Prob,df=4)  + HomeAway + ns(RedCard.Diff,df=4),
                     family = "poisson",
                     data= our.df.cut %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID)))

  simulationOutput <- simulateResiduals(fittedModel = glm.obj)
  # plot(simulationOutput)

  ZI.test <- testZeroInflation(simulationOutput)
  print(ZI.test$p.value)
}

  #
  # DHARMa zero-inflation test via comparison to expected zeros
  # with simulation under H0 = fitted model
  #
  # data:  simulationOutput
  # ratioObsSim = 1.0039, p-value < 2.2e-16
  # alternative hypothesis: two.sided
  
  
  
  
  ######
  ## COMPARING:
  ##    Regular Poisson,
  ##    Negative binomial,
  ##    Zero-Inflated Poisson
  ######

glm.obj <- glm.nb.obj <- glm.ziP.obj <- list()
gam.obj <- gam.nb.obj <- gam.ziP.obj <- list()


load(file=paste0("gam_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))
load(file=paste0("gam_nb_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))
load(file=paste0("gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))



for (league in league.name){
  
  print(league)
  
  our.df.cut <- read.csv(paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  ## REMOVING NA betting data
  our.df.cut <- our.df.cut %>% filter(!is.na(Weighted.Win.Prob))
  
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
    mutate(Period.ID = group_indices(our.df.cut, .dots=c("ID", "Minute.clean", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
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
  
  gam.obj[[league]] <- gam(#Shots ~ 
                          Corners ~
                             s(Score.Diff) + s(Minute.clean) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5),
                           family = "poisson", data= our.df.cut)
  
  gam.nb.obj[[league]] <- gam(#Shots ~ 
                              Corners ~
                                s(Score.Diff) + s(Minute.clean) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5),
                              family = "nb", data= our.df.cut)
  
  gam.ziP.obj[[league]] <- gam(#Shots ~ 
                               Corners ~
                                 s(Score.Diff) + s(Minute.clean) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5),
                               family="ziP", data= our.df.cut)
  
  save(gam.obj, file=paste0("gam_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))
  save(gam.nb.obj, file=paste0("gam_nb_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))
  save(gam.ziP.obj, file=paste0("gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))
  
  
  #print(league)
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



##########
######
## For SHOTS
######
##########

## P-VALUES of ZERO-INFLATION TESTS:
#
# [1] "Bundesliga"
# [1] 0
# [1] "SerieA"
# [1] 0
# [1] "LaLiga"
# [1] 0
# [1] "Ligue1"
# [1] 0
# [1] "PremierLeague"
# [1] 0


### BIC

##
# [1] "Bundesliga"
# df      BIC
# gam.obj[[league]]     24.21102 696429.2
# gam.nb.obj[[league]]  27.26974 694705.8
# gam.ziP.obj[[league]] 29.97438 694642.5

# [1] "SerieA"
# df      BIC
# gam.obj[[league]]     28.22657 815362.7
# gam.nb.obj[[league]]  29.25018 813972.3
# gam.ziP.obj[[league]] 31.38287 813739.8


# [1] "LaLiga"
# df      BIC
# gam.obj[[league]]     27.69454 705471.9
# gam.nb.obj[[league]]  28.65356 704325.9
# gam.ziP.obj[[league]] 31.12784 704221.2
# 
# [1] "Ligue1"
# df      BIC
# gam.obj[[league]]     28.49415 791814.1
# gam.nb.obj[[league]]  29.47558 790067.8
# gam.ziP.obj[[league]] 31.19913 790046.1
# 
# [1] "PremierLeague"
# df      BIC
# gam.obj[[league]]     32.08188 733928.9
# gam.nb.obj[[league]]  31.93393 730675.4
# gam.ziP.obj[[league]] 33.21137 730650.8

# [1] "PremierLeague"
# df      BIC
# gam.obj[[league]]     31.75249 724601.4
# gam.nb.obj[[league]]  31.74661 721825.8
# gam.ziP.obj[[league]] 33.09799 721644.5



##########
######
## For CORNERS
######
##########


## P-VALUES of ZERO-INFLATION TESTS:
#
# [1] "Bundesliga"
# [1] 0
# [1] "SerieA"
# [1] 0
# [1] "LaLiga"
# [1] 0
# [1] "Ligue1"
# [1] 0
# [1] "PremierLeague"
# [1] 0



### BIC

# [1] "Bundesliga"
# df      BIC
# gam.obj[[league]]     26.34169 345011.5
# gam.nb.obj[[league]]  27.26944 344560.5
# gam.ziP.obj[[league]] 29.67102 344543.3

# 
# [1] "SerieA"
# df      BIC
# gam.obj[[league]]     27.88299 415113.3
# gam.nb.obj[[league]]  27.94827 414363.9
# gam.ziP.obj[[league]] 29.69538 414260.9
# 

# [1] "LaLiga"
# df      BIC
# gam.obj[[league]]     26.36813 374351.2
# gam.nb.obj[[league]]  25.08563 373815.1
# gam.ziP.obj[[league]] 26.46663 373780.7

# 
# [1] "Ligue1"
# df      BIC
# gam.obj[[league]]     26.42468 407094.6
# gam.nb.obj[[league]]  27.09602 406348.0
# gam.ziP.obj[[league]] 29.51177 406308.1
# 
# [1] "PremierLeague"
# df      BIC
# gam.obj[[league]]     27.96073 381504.7
# gam.nb.obj[[league]]  30.45516 380899.5
# gam.ziP.obj[[league]] 31.37359 380806.1




######
## TAKEAWAYS
######

### FOR SHOTS:

# 1. ZIP wins over NegBin everywhere
# 2. Splines always beat linear

### FOR CORNERS:

# 1. ZIP wins over NegBin across the board
# 2. Splines always beat linear