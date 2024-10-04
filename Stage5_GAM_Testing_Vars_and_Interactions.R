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


######
## COMPARING:
##    Regular Poisson,
##    Negative binomial,
##    Zero-Inflated Poisson
######

 TEST_gam.ziP.obj <- list()

# load("TEST_gam_ziP_obj_Shots.Robj")

load(file=paste0("TEST_gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Shots.Robj"))



for (league in league.name){
  
  print(league)
  
  #our.df <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  our.df.cut <- read.csv(paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  ## REMOVING NA betting data
  our.df.cut <- our.df.cut %>% filter(!is.na(Weighted.Win.Prob))
  
  
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
  

  
  ######
  ## Fitting SPLINE models
  ######
  
  TEST_gam.ziP.obj[[league]] <- gam(Shots ~ 
                                 #s(Score.Diff) + 
                                   s(Weighted.Win.Prob)  + HomeAway # + s(RedCard.Diff, k=5) 
                                 + s(Minute.clean)
                                # + te(Score.Diff, Minute.clean)
                               # + te(RedCard.Diff, Minute.clean)
                               + te(Score.Diff, RedCard.Diff)
                                 ,
                               family="ziP", data= our.df.cut)
  
  save(file=paste0("TEST_gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Shots.Robj"),
       TEST_gam.ziP.obj)
  
  
  print(league)
  print(BIC(TEST_gam.ziP.obj[[league]]))
  
  cat("\n")
}



load(paste0("gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Shots.Robj"))


for (league in league.name){
  print(BIC(gam.ziP.obj[[league]],
            TEST_gam.ziP.obj[[league]]))
}


#####
## REGULAR MODEL (gam.ziP.obj) vs
##  EXCLUDING MINUTE
#####


## SHOTS

## SHOTS

# df      BIC
# gam.ziP.obj[[league]]      29.97438 694642.5
# TEST_gam.ziP.obj[[league]] 21.26645 696523.3
# df      BIC
# gam.ziP.obj[[league]]      31.38287 813739.8
# TEST_gam.ziP.obj[[league]] 21.69489 815728.7
# df      BIC
# gam.ziP.obj[[league]]      31.12784 704221.2
# TEST_gam.ziP.obj[[league]] 22.06830 705832.9
# df      BIC
# gam.ziP.obj[[league]]      31.19913 790046.1
# TEST_gam.ziP.obj[[league]] 19.68975 791840.9
# df      BIC
# gam.ziP.obj[[league]]      33.09799 721644.5
# TEST_gam.ziP.obj[[league]] 21.35337 723433.2


## CORNERS

# df      BIC
# gam.ziP.obj[[league]]      29.67102 344543.3
# TEST_gam.ziP.obj[[league]] 20.73331 344856.4
# df      BIC
# gam.ziP.obj[[league]]      29.69538 414260.9
# TEST_gam.ziP.obj[[league]] 20.48441 414625.0
# df      BIC
# gam.ziP.obj[[league]]      26.46663 373780.7
# TEST_gam.ziP.obj[[league]] 16.47717 374012.3
# df      BIC
# gam.ziP.obj[[league]]      29.51177 406308.1
# TEST_gam.ziP.obj[[league]] 19.83214 406694.6
# df      BIC
# gam.ziP.obj[[league]]      31.37359 380806.1
# TEST_gam.ziP.obj[[league]] 22.12213 381182.8


#####
## REGULAR MODEL (gam.ziP.obj) vs
##  EXCLUDING SCORE DIFF
#####

#...



#####
## REGULAR MODEL (gam.ziP.obj) vs
##  INTERACTION SCORE.DIFF x MINUTE.CLEAN
#####

### SHOTS

# df      BIC
# gam.ziP.obj[[league]]      29.97438 694642.5
# TEST_gam.ziP.obj[[league]] 31.85849 695238.2
# df      BIC
# gam.ziP.obj[[league]]      31.38287 813739.8
# TEST_gam.ziP.obj[[league]] 33.83410 814590.7
# df      BIC
# gam.ziP.obj[[league]]      31.12784 704221.2
# TEST_gam.ziP.obj[[league]] 32.36936 704812.6
# df      BIC
# gam.ziP.obj[[league]]      31.19913 790046.1
# TEST_gam.ziP.obj[[league]] 32.20244 790729.6
# df      BIC
# gam.ziP.obj[[league]]      33.09799 721644.5
# TEST_gam.ziP.obj[[league]] 34.33297 722218.8


#####
## REGULAR MODEL (gam.ziP.obj) vs
##  INTERACTION REDCARD.DIFF x MINUTE.CLEAN
#####

# df      BIC
# gam.ziP.obj[[league]]      29.97438 694642.5
# TEST_gam.ziP.obj[[league]] 31.16130 695268.8
# df      BIC
# gam.ziP.obj[[league]]      31.38287 813739.8
# TEST_gam.ziP.obj[[league]] 33.77948 814661.8
# df      BIC
# gam.ziP.obj[[league]]      31.12784 704221.2
# TEST_gam.ziP.obj[[league]] 32.59980 704821.5
# df      BIC
# gam.ziP.obj[[league]]      31.19913 790046.1
# TEST_gam.ziP.obj[[league]] 33.23701 790783.8
# df      BIC
# gam.ziP.obj[[league]]      33.09799 721644.5
# TEST_gam.ziP.obj[[league]] 36.30211 722283.2



#####
## REGULAR MODEL (gam.ziP.obj) vs
##  INTERACTION SCORE.DIFF x REDCARD.DIFF 
#####

# df      BIC
# gam.ziP.obj[[league]]      29.97438 694642.5
# TEST_gam.ziP.obj[[league]] 31.16130 694730

# df      BIC
# gam.ziP.obj[[league]]      31.38287 813739.8
# TEST_gam.ziP.obj[[league]] 33.77948 813858.6

# df      BIC
# gam.ziP.obj[[league]]      31.12784 704221.2
# TEST_gam.ziP.obj[[league]] 32.59980 704352.4
