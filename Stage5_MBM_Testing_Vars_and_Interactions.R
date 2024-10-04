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

# load("TEST_gam_ziP_obj_Corners.Robj")

load(file=paste0("TEST_gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))



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
  
  TEST_gam.ziP.obj[[league]] <- gam(Corners ~ 
                                 s(Score.Diff) + 
                                   s(Weighted.Win.Prob)  + HomeAway # + s(RedCard.Diff, k=5) 
                                 #+ s(Minute.clean)
                                # + te(Score.Diff, Minute.clean)
                                + te(RedCard.Diff, Minute.clean)
                                 ,
                               family="ziP", data= our.df.cut)
  
  save(file=paste0("TEST_gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"),
       TEST_gam.ziP.obj)
  
  
  print(league)
  print(BIC(TEST_gam.ziP.obj[[league]]))
  
  cat("\n")
}


#load(paste0("NO_MINUTES_TEST_gam_ziP_obj_NO_EXTRA_TIME_Corners.Robj"))

load(paste0("gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))

## 
print("BICs:")
for (league in league.name){
  print(BIC(gam.ziP.obj[[league]],
            TEST_gam.ziP.obj[[league]]))
}


print("AICs:")
for (league in league.name){
print(AIC(gam.ziP.obj[[league]],
          TEST_gam.ziP.obj[[league]]))
}

rm(gam.ziP.obj)

#####
## REGULAR MODEL (gam.ziP.obj) vs
##  EXCLUDING MINUTE
#####


## SHOTS


# BICs:
#
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

# AICs:
#
# df      AIC
# gam.ziP.obj[[league]]      29.97438 694293.1
# TEST_gam.ziP.obj[[league]] 21.26645 696275.4
# df      AIC
# gam.ziP.obj[[league]]      31.38287 813369.0
# TEST_gam.ziP.obj[[league]] 21.69489 815472.4
# df      AIC
# gam.ziP.obj[[league]]      31.12784 703856.3
# TEST_gam.ziP.obj[[league]] 22.06830 705574.2
# df      AIC
# gam.ziP.obj[[league]]      31.19913 789676.5
# TEST_gam.ziP.obj[[league]] 19.68975 791607.7
# df      AIC
# gam.ziP.obj[[league]]      33.09799 721256.6
# TEST_gam.ziP.obj[[league]] 21.35337 723183.0


## CORNERS

# [1] "BICs:"
# 
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
# 

# [1] "AICs:"
# 
# df      AIC
# gam.ziP.obj[[league]]      29.67102 344197.4
# TEST_gam.ziP.obj[[league]] 20.73331 344614.6
# df      AIC
# gam.ziP.obj[[league]]      29.69538 413910.1
# TEST_gam.ziP.obj[[league]] 20.48441 414383.0
# df      AIC
# gam.ziP.obj[[league]]      26.46663 373470.5
# TEST_gam.ziP.obj[[league]] 16.47717 373819.2
# df      AIC
# gam.ziP.obj[[league]]      29.51177 405958.4
# TEST_gam.ziP.obj[[league]] 19.83214 406459.6
# df      AIC
# gam.ziP.obj[[league]]      31.37359 380438.4
# TEST_gam.ziP.obj[[league]] 22.12213 380923.5


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

## BIC
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

## AIC
# df      AIC
# gam.ziP.obj[[league]]      29.97438 694293.1
# TEST_gam.ziP.obj[[league]] 31.85849 694866.8
# df    AIC
# gam.ziP.obj[[league]]      31.38287 813369
# TEST_gam.ziP.obj[[league]] 33.83410 814191
# df      AIC
# gam.ziP.obj[[league]]      31.12784 703856.3
# TEST_gam.ziP.obj[[league]] 32.36936 704433.2
# df      AIC
# gam.ziP.obj[[league]]      31.19913 789676.5
# TEST_gam.ziP.obj[[league]] 32.20244 790348.1
# df      AIC
# gam.ziP.obj[[league]]      33.09799 721256.6
# TEST_gam.ziP.obj[[league]] 34.33297 721816.4



## CORNERS


# BICs
#
# df      BIC
# gam.ziP.obj[[league]]      29.67102 344543.3
# TEST_gam.ziP.obj[[league]] 30.45713 344798.3
# df      BIC
# gam.ziP.obj[[league]]      29.69538 414260.9
# TEST_gam.ziP.obj[[league]] 29.89675 414585.7
# df      BIC
# gam.ziP.obj[[league]]      26.46663 373780.7
# TEST_gam.ziP.obj[[league]] 25.53214 374031.9
# df      BIC
# gam.ziP.obj[[league]]      29.51177 406308.1
# TEST_gam.ziP.obj[[league]] 29.51672 406627.8
# df      BIC
# gam.ziP.obj[[league]]      31.37359 380806.1
# TEST_gam.ziP.obj[[league]] 31.77490 381063.6


# AICs
# 
# df      AIC
# gam.ziP.obj[[league]]      29.67102 344197.4
# TEST_gam.ziP.obj[[league]] 30.45713 344443.2
# df      AIC
# gam.ziP.obj[[league]]      29.69538 413910.1
# TEST_gam.ziP.obj[[league]] 29.89675 414232.5
# df      AIC
# gam.ziP.obj[[league]]      26.46663 373470.5
# TEST_gam.ziP.obj[[league]] 25.53214 373732.6
# df      AIC
# gam.ziP.obj[[league]]      29.51177 405958.4
# TEST_gam.ziP.obj[[league]] 29.51672 406278.1
# df      AIC
# gam.ziP.obj[[league]]      31.37359 380438.4
# TEST_gam.ziP.obj[[league]] 31.77490 380691.2



#####
## REGULAR MODEL (gam.ziP.obj) vs
##  INTERACTION REDCARD.DIFF x MINUTE.CLEAN
#####

## SHOTS

# BICs:
#
# df      BIC
# gam.ziP.obj[[league]]      29.97438 694642.5
# TEST_gam.ziP.obj[[league]] 32.78570 694730.0
# df      BIC
# gam.ziP.obj[[league]]      31.38287 813739.8
# TEST_gam.ziP.obj[[league]] 34.45953 813858.6
# df      BIC
# gam.ziP.obj[[league]]      31.12784 704221.2
# TEST_gam.ziP.obj[[league]] 34.39893 704352.4
# df      BIC
# gam.ziP.obj[[league]]      31.19913 790046.1
# TEST_gam.ziP.obj[[league]] 33.23701 790783.8
# df      BIC
# gam.ziP.obj[[league]]      33.09799 721644.5
# TEST_gam.ziP.obj[[league]] 36.30211 722283.2

# AICs:
#
# df      AIC
# gam.ziP.obj[[league]]      29.97438 694293.1
# TEST_gam.ziP.obj[[league]] 32.78570 694347.8
# df      AIC
# gam.ziP.obj[[league]]      31.38287 813369.0
# TEST_gam.ziP.obj[[league]] 34.45953 813451.5
# df      AIC
# gam.ziP.obj[[league]]      31.12784 703856.3
# TEST_gam.ziP.obj[[league]] 34.39893 703949.1
# df      AIC
# gam.ziP.obj[[league]]      31.19913 789676.5
# TEST_gam.ziP.obj[[league]] 33.23701 790390.0
# df      AIC
# gam.ziP.obj[[league]]      33.09799 721256.6
# TEST_gam.ziP.obj[[league]] 36.30211 721857.7



## CORNERS

# "BICs:"
# 
# df      BIC
# gam.ziP.obj[[league]]      29.67102 344543.3
# TEST_gam.ziP.obj[[league]] 29.47429 344764.6
# df      BIC
# gam.ziP.obj[[league]]      29.69538 414260.9
# TEST_gam.ziP.obj[[league]] 30.23327 414560.6
# df      BIC
# gam.ziP.obj[[league]]      26.46663 373780.7
# TEST_gam.ziP.obj[[league]] 26.36996 373969.2
# df      BIC
# gam.ziP.obj[[league]]      29.51177 406308.1
# TEST_gam.ziP.obj[[league]] 30.32363 406598.8
# df      BIC
# gam.ziP.obj[[league]]      31.37359 380806.1
# TEST_gam.ziP.obj[[league]] 31.93404 381018.4

# AICs:
#   
# df      AIC
# gam.ziP.obj[[league]]      29.67102 344197.4
# TEST_gam.ziP.obj[[league]] 29.47429 344420.9
# df      AIC
# gam.ziP.obj[[league]]      29.69538 413910.1
# TEST_gam.ziP.obj[[league]] 30.23327 414203.4
# df      AIC
# gam.ziP.obj[[league]]      26.46663 373470.5
# TEST_gam.ziP.obj[[league]] 26.36996 373660.1
# df      AIC
# gam.ziP.obj[[league]]      29.51177 405958.4
# TEST_gam.ziP.obj[[league]] 30.32363 406239.6
# df      AIC
# gam.ziP.obj[[league]]      31.37359 380438.4
# TEST_gam.ziP.obj[[league]] 31.93404 380644.1