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


######
## COMPARING:
##    Regular Poisson,
##    Negative binomial,
##    Zero-Inflated Poisson
######

glm.obj <- glm.nb.obj <- glm.ziP.obj <- list()
gam.obj <- gam.nb.obj <- gam.ziP.obj <- list()


# load(file=paste0("gam_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))
# load(file=paste0("gam_nb_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))
# load(file=paste0("gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))



for (league in league.name){
  
  print(league)
  
  
  ############
  ### TIME INTERVAL stuff
  ############

  our.df.cut <- read.csv(paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  ## REMOVING NA betting data
  our.df.cut <- our.df.cut %>% filter(!is.na(Weighted.Win.Prob))
  #dim(our.df)
  
  our.df.cut  <- our.df.cut  %>%
    mutate(abs.Score.Diff = abs(Score.Diff),
           abs.RedCard.Diff = abs(RedCard.Diff))
  
  our.df.cut  <- our.df.cut  %>%
    mutate(Period.ID = group_indices(our.df.cut, .dots=c("ID", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
    arrange(Period.ID) %>%
    mutate(Period.ID = factor(Period.ID))
  
  our.df.cut <- our.df.cut %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
  
  ####
  ## PICKING A SUBSET OF 1000 GAMES (takes too much time to run all games)
  ####
  
  set.seed(1)
  picked.gameId <- sample(unique(our.df.cut$gameId), 1000)
  
  
  ######
  ## Fitting SPLINE models
  ######
  
  gam.nb.obj <- gam(Shots ~ 
                      #Corners ~
                      s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5) + log(minutes.spent + 1),
                    family = "nb", data= our.df.cut %>% filter(gameId %in% picked.gameId))
  
 # plot(gam.nb.obj, pages=1)

  
  
  
  
  #########
  #########
  ## MINUTE-BY-MINUTE
  #########
  #########
  
  
  our.df.cut.mbm <- read.csv(paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  ## REMOVING NA betting data
  our.df.cut.mbm <- our.df.cut.mbm %>% filter(!is.na(Weighted.Win.Prob))

  
  ############
  ############
  
  our.df.cut.mbm  <- our.df.cut.mbm  %>%
    mutate(abs.Score.Diff = abs(Score.Diff),
           abs.RedCard.Diff = abs(RedCard.Diff))
  
  our.df.cut.mbm  <- our.df.cut.mbm  %>%
    mutate(Period.ID = group_indices(our.df.cut.mbm, .dots=c("ID", "Minute.clean", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
    arrange(Period.ID) %>%
    mutate(Period.ID = factor(Period.ID))
  
  our.df.cut.mbm <- our.df.cut.mbm %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
  

  ######
  ## Fitting SPLINE models
  ######
  
  
  
  gam.ziP.obj <- gam(Shots ~ 
   # Corners ~
      s(Score.Diff)  + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5)
   + s(Minute.clean)
   ,
    family="ziP", data= our.df.cut.mbm %>% filter(gameId %in% picked.gameId))
  
  gam.ziP.obj.no.bad.min <- gam(Shots ~ 
                       # Corners ~
                       s(Score.Diff)  + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5)
                     + s(Minute.clean)
                     ,
                     family="ziP", data= our.df.cut.mbm %>% filter(gameId %in% picked.gameId) %>%
                       filter(!(half_id == 1 & Minute.clean > 45))
  )
  
  gam.nb.obj.no.bad.min <- gam(Shots ~ 
                                  # Corners ~
                                  s(Score.Diff)  + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5)
                                + s(Minute.clean)
                                ,
                                family="nb", data= our.df.cut.mbm %>% filter(gameId %in% picked.gameId) %>%
                                  filter(!(half_id == 1 & Minute.clean > 45))
  )
  
  
  
 # plot(gam.ziP.obj, pages=1, ylim=c(-1,1), xlim=c(-2,2))
 # plot(gam.nb.obj, pages=1, ylim=c(-1,1),xlim=c(-2,2))
  
  plot(gam.ziP.obj, pages=1)
  plot(gam.ziP.obj.no.bad.min, pages=1, ylim=c(-1,1))
  plot(gam.nb.obj.no.bad.min, pages=1, ylim=c(-1,1))
  
  predict(gam.ziP.obj)
  predict(gam.ziP.obj, type="response")
  
 
 
 # Formula:
 #   Shots ~ s(Score.Diff) + s(Minute.clean) + s(Weighted.Win.Prob) + 
 #   HomeAway + s(RedCard.Diff, k = 5)
 # 
 # Estimated degrees of freedom:
 #   5.35 8.69 4.76 3.02  total = 23.83 
 # 
 # REML score: 77324.84   
 
 # Deviance explained = 94.1%
 # -REML =  77325  Scale est. = 1         n = 189610
  
 
 
  gam.ziP.ds.obj <- gam(Shots ~ 
                          # Corners ~
                       s(Score.Diff, bs="ds") + s(Minute.clean,bs="ds") + s(Weighted.Win.Prob, bs="ds")  + HomeAway + s(RedCard.Diff, bs="ds", k=5),
                     family="ziP", data= our.df.cut.mbm %>% filter(gameId %in% picked.gameId))
  
  plot(gam.ziP.ds.obj, pages=1)
  
  
  gam.ziP.cr.obj <- gam(Shots ~ 
                          # Corners ~
                          s(Score.Diff, bs="cr") + s(Minute.clean,bs="cr") + s(Weighted.Win.Prob, bs="cr")  + HomeAway + s(RedCard.Diff, bs="cr", k=5),
                        family="ziP", data= our.df.cut.mbm %>% filter(gameId %in% picked.gameId))
  
  plot(gam.ziP.cr.obj, pages=1)
  
  gam.ziP.bs.obj <- gam(Shots ~ 
                          # Corners ~
                          s(Score.Diff, bs="bs") + s(Minute.clean,bs="bs") + s(Weighted.Win.Prob, bs="bs")  + HomeAway + s(RedCard.Diff, bs="bs", k=5),
                        family="ziP", data= our.df.cut.mbm %>% filter(gameId %in% picked.gameId))
  
  plot(gam.ziP.bs.obj, pages=1)
  
  
  
  #print(league)
 # print(BIC(#glm.obj[[league]], glm.nb.obj[[league]], glm.ziP.obj[[league]],
#    gam.obj[[league]], gam.nb.obj[[league]], gam.ziP.obj[[league]]))
  
  cat("\n")
}

