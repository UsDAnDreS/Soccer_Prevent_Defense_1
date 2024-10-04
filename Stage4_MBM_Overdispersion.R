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


### EXPLICITLY TESTING FOR OVERDISPERSION (BARELY NEEDED, ALWAYS HIGHLY SIGNIFICANT)

# for (league in league.name){
#   
#   print(league)
#   
#   our.df.cut <- read.csv(paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
#   
#   
#   ## REMOVING NA betting data
#   our.df.cut <- our.df.cut %>% filter(!is.na(Weighted.Win.Prob))
#   
#   ## Remove extra time data from first half (so anything with half_id=1, minute>45)
#   ## => TO AVOID "POINT MASS"/"OUT-OF-ORDER MINUTE" ISSUES...
#   ##
#   ## !!! BUUUT... the >90 IS ALSO AN ISSUE... data is way too sporadic over there !!!
#   # our.df.cut <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
#   # print(dim(our.df.cut))
#   
#   
#   ############
#   ############
#   
#   our.df.cut  <- our.df.cut  %>%
#     mutate(abs.Score.Diff = abs(Score.Diff),
#            abs.RedCard.Diff = abs(RedCard.Diff))
#   
#   our.df.cut  <- our.df.cut  %>%
#     mutate(Period.ID = group_indices(our.df.cut, .dots=c("ID", "Minute.clean", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
#     arrange(Period.ID) %>%
#     mutate(Period.ID = factor(Period.ID))
#   
#   
#   
#   
#   ######
#   ## Fitting regular Poisson, testing for overdispersion
#   ######
#   
#   glm.obj <- glm(# Corners ~
#     Corners ~
#       ns(Score.Diff,df=4) + ns(Minute.clean,df=4) + ns(Weighted.Win.Prob,df=4)  + HomeAway + ns(RedCard.Diff,df=4),
#     family = "poisson",
#     data= our.df.cut %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID)))
#   
#   simulationOutput <- simulateResiduals(fittedModel = glm.obj)
#   # plot(simulationOutput)
#   
#   ZI.test <- testDispersion(simulationOutput)
#   print(ZI.test$p.value)
# }

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
# load(file=paste0("gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))



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
  
  # glm.obj[[league]] <- gam(Corners ~ 
  #                         # Shots ~ 
  #                            Score.Diff + Minute.clean + Weighted.Win.Prob  + HomeAway + RedCard.Diff,
  #                          family = "poisson",
  #                          data= our.df.cut)
  # 
  # glm.nb.obj[[league]] <- gam(Corners ~ 
  #                            #  Shots ~
  #                               Score.Diff + Minute.clean + Weighted.Win.Prob  + HomeAway + RedCard.Diff                            ,
  #                             family = "nb", data= our.df.cut)
  # 
  # glm.ziP.obj[[league]] <- gam(Corners ~ 
  #                               # Shots ~
  #                                Score.Diff + Minute.clean + Weighted.Win.Prob  + HomeAway + RedCard.Diff,
  #                              family="ziP", data= our.df.cut)
  
  
  
  
  ######
  ## Fitting SPLINE models
  ######
  
  gam.obj[[league]] <- gam(Corners ~ 
      s(Score.Diff) + s(Minute.clean) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5),
    family = "poisson", data= our.df.cut)
  
  gam.nb.obj[[league]] <- gam(Corners ~ 
      s(Score.Diff) + s(Minute.clean) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5),
    family = "nb", data= our.df.cut)
  
  # gam.ziP.obj[[league]] <- gam(#Corners ~ 
  #   Shots ~
  #     s(Score.Diff) + s(Minute.clean) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5),
  #   family="ziP", data= our.df.cut)
  
  save(gam.obj, file=paste0("gam_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))
  save(gam.nb.obj, file=paste0("gam_nb_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))
  # save(gam.ziP.obj, file=paste0("gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))
  
  
  #print(league)
  print(BIC(#glm.obj[[league]], glm.nb.obj[[league]], glm.ziP.obj[[league]],
    gam.obj[[league]], gam.nb.obj[[league]]
   # , gam.ziP.obj[[league]]
   ))
  
  cat("\n")
}


for (league in league.name){
  print(league)
  print(BIC(#glm.obj[[league]], glm.nb.obj[[league]], glm.ziP.obj[[league]],
            gam.obj[[league]], gam.nb.obj[[league]]
            #, gam.ziP.obj[[league]]
            ))

  cat("\n")
}



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
# gam.obj[[league]]    24.36757 687608.0
# gam.nb.obj[[league]] 27.21346 685933.3
# 
# [1] "SerieA"
# df      BIC
# gam.obj[[league]]    28.47489 803982.3
# gam.nb.obj[[league]] 29.33493 802637.4
# 
# [1] "LaLiga"
# df      BIC
# gam.obj[[league]]    27.50457 696788.6
# gam.nb.obj[[league]] 28.56009 695665.7
# 
# [1] "Ligue1"
# df      BIC
# gam.obj[[league]]    28.53918 780522.1
# gam.nb.obj[[league]] 29.60467 778826.6
# 
# [1] "PremierLeague"
# df      BIC
# gam.obj[[league]]    32.03175 708828.9
# gam.nb.obj[[league]] 31.35430 706149.8



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
# gam.obj[[league]]    24.36757 687608.0
# gam.nb.obj[[league]] 27.21346 685933.3
# 
# [1] "SerieA"
# df      BIC
# gam.obj[[league]]    28.47489 803982.3
# gam.nb.obj[[league]] 29.33493 802637.4
# 
# [1] "LaLiga"
# df      BIC
# gam.obj[[league]]    27.50457 696788.6
# gam.nb.obj[[league]] 28.56009 695665.7
# 
# [1] "Ligue1"
# df      BIC
# gam.obj[[league]]    28.53918 780522.1
# gam.nb.obj[[league]] 29.60467 778826.6
# 
# [1] "PremierLeague"
# df      BIC
# gam.obj[[league]]    32.03175 708828.9
# gam.nb.obj[[league]] 31.35430 706149.8


######
## TAKEAWAYS
######

### FOR SHOTS:

# 1. ZIP wins over NegBin everywhere
# 2. Splines always beat linear

### FOR CORNERS:

# 1. ZIP wins over NegBin across the board
# 2. Splines always beat linear