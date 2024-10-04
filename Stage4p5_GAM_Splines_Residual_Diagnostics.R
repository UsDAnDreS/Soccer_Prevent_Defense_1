######
## "LeagueName_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"
######

library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)
library(mgcViz)


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


# pdf("Time_Interval_DHARMa_Diagnostics.pdf")
# 
# ### TIME INTERVAL model
# 
# for (league in league.name){
#   
#   print(league)
#   
#   our.df.cut <- read.csv(paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
#   
#   
#   ## REMOVING NA betting data
#   our.df.cut <- our.df.cut %>% filter(!is.na(Weighted.Win.Prob))
#   # dim(our.df.cut)
#   
#   ## Remove extra time data from first half (so anything with half_id=1, minute>45)
#   ## => TO AVOID "POINT MASS"/"OUT-OF-ORDER MINUTE" ISSUES...
#   ##
#   ## !!! BUUUT... the >90 IS ALSO AN ISSUE... data is way too sporadic over there !!!
#   # our.df.cut <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
#   # print(dim(our.df.cut))
#   
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
#     mutate(Period.ID = group_indices(our.df.cut, .dots=c("ID", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
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
#   
#   # glm.obj <- glm.nb(Shots ~ 
#   #                  # Corners ~
#   #                  ns(Score.Diff,df=6) + ns(Weighted.Win.Prob,df=6)  + HomeAway + ns(RedCard.Diff,df=5) + offset(log(minutes.spent+1)),
#   #                # family = "poisson",
#   #                data= our.df.cut)
#   
#   glm.obj <- gam(Shots ~ 
#                       # Corners ~
#                      # ns(Score.Diff,df=6) + ns(Weighted.Win.Prob,df=6)  + HomeAway + ns(RedCard.Diff,df=5) + offset(log(minutes.spent+1)),
#                       s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff,k=5) + offset(log(minutes.spent+1)),
#                  family="nb",
#                       # family = "poisson",
#                     data= our.df.cut)
#   
#   
#   # Width: 850; Height: 490	
#   
#   simulationOutput <- simulateResiduals(fittedModel = glm.obj)
#   plot(simulationOutput)
#   
#   # testOutliers(simulationOutput,
#   #              type = 'bootstrap')
#   # ZI.test <- testZeroInflation(simulationOutput)
#   # print(ZI.test$p.value)
# }
# 
# dev.off()




### MINUTE-BY-MINUTE

## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we use models where "Minute.clean" was included
include.minute <- TRUE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

for (league in league.name[5]){
  
  pdf(paste0("MinuteByMinute_DHARMa_Diagnostics_",league,".pdf"))
  
  load(file=paste0(ifelse(include.minute,
                          "",
                          "NO_MINUTES_TEST_"),
                   "gam_ziP_obj",  
                   ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
                   "_Shots.Robj"))
  
  print(league)
  
  gam.ziP.obj <- gam.ziP.obj[[league]]
  
  
  # Width: 850; Height: 490	
  
  # simulationOutput <- simulateResiduals(fittedModel = gam.ziP.obj)
  # plot(simulationOutput)
  
  simulateResiduals(fittedModel = gam.ziP.obj, n=175, plot=TRUE)
  
  # testOutliers(simulationOutput,
  #              type = 'bootstrap')
  # ZI.test <- testZeroInflation(simulationOutput)
  # print(ZI.test$p.value)
  
  dev.off()
}

