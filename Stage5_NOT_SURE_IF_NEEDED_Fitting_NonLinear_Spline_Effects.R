######
######

library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)



league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


######
## COMPARING:
##   1. One run was to OBTAIN STATISTICAL SIGNIFICANCE
##      (breaking it into linear and non-linear terms)
##   2. Next run was to actually 
######

# gam.ziP.obj <- list()

# load(file="gam_ziP_obj.Robj")


## FIRST RUN: DOING THE TESTING of NON-LINEAR PARTS

# for (league in league.name){
#   
#   print(league)
#   
#   our.df <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
#   
#   
#   ## REMOVING NA betting data
#   our.df <- our.df %>% filter(!is.na(Weighted.Win.Prob))
#   dim(our.df)
#   
#   ## Remove extra time data from first half (so anything with half_id=1, minute>45)
#   ## => TO AVOID "POINT MASS"/"OUT-OF-ORDER MINUTE" ISSUES...
#   ##
#   ## !!! BUUUT... the >90 IS ALSO AN ISSUE... data is way too sporadic over there !!!
#   our.df.cut <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
#   print(dim(our.df.cut))
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
#     mutate(Period.ID = group_indices(our.df.cut, .dots=c("ID", "Minute.clean", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
#     arrange(Period.ID) %>%
#     mutate(Period.ID = factor(Period.ID))
#   
#   our.df.cut <- our.df.cut %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
#   
#   
#   gam.ziP.obj[[league]] <- gam(#Shots ~ 
#      Corners ~
#       Score.Diff + s(Score.Diff, m=c(2,0)) + 
#       Minute.clean + s(Minute.clean, m=c(2,0)) + 
#       Weighted.Win.Prob + s(Weighted.Win.Prob, m=c(2,0))  + HomeAway + 
#       RedCard.Diff + s(RedCard.Diff, k=5, m=c(2,0)),
#     family="ziP", data= our.df.cut)
#     
#  print(summary(gam.ziP.obj[[league]]))
#   
# }






## SECOND RUN - to ACTUALLY SAVE THE FULLY NON-LINEAR SPLINE OBJECTS
##  (otherwise, plot.gam becomes messed up)

# gam.ziP.obj <- list()


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
  our.df.cut <- our.df %>% filter(!(half_id == 1 & Minute.clean > 45) & Minute.clean <90)
  print(dim(our.df.cut))
  
  
  
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
  
  
  gam.ziP.obj[[league]] <- gam(#Shots ~ 
    Corners ~
      s(Score.Diff) +
      s(RedCard.Diff, k=5) +
      s(Minute.clean) +  
      s(Weighted.Win.Prob) + HomeAway,
    family="ziP", data= our.df.cut)
  
  # print(summary(gam.ziP.obj[[league]]))
  
  save(gam.ziP.obj, file="gam_ziP_obj_Corners.Robj")
}



########
########
##  SHOTS 
########
########


## SerieA
#
# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq  p-value    
#   s(Score.Diff)        5.795      8  280.9  < 2e-16 ***
#   s(Minute.clean)      7.946      8 1381.4  < 2e-16 ***
#   s(Weighted.Win.Prob) 4.444      8   70.5  < 2e-16 ***
#   s(RedCard.Diff)      1.895      3   26.2 9.75e-07 ***

## LaLiga
# 
# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq  p-value    
#   s(Score.Diff)        5.630      8 242.66  < 2e-16 ***
#   s(Minute.clean)      7.914      8 976.25  < 2e-16 ***
#   s(Weighted.Win.Prob) 4.797      8  87.38  < 2e-16 ***
#   s(RedCard.Diff)      1.625      3  17.49 2.92e-05 ***

## Ligue1

# Approximate significance of smooth terms:
#   edf Ref.df  Chi.sq p-value    
#   s(Score.Diff)        6.561      8  251.01  <2e-16 ***
#   s(Minute.clean)      7.939      8 1068.15  <2e-16 ***
#   s(Weighted.Win.Prob) 5.685      8   57.34  <2e-16 ***
#   s(RedCard.Diff)      2.154      3   58.44  <2e-16 ***

## Bundesliga

# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq  p-value    
#   s(Score.Diff)        4.901      8 193.79  < 2e-16 ***
#   s(Minute.clean)      7.929      8 927.47  < 2e-16 ***
#   s(Weighted.Win.Prob) 6.342      8  65.64  < 2e-16 ***
#   s(RedCard.Diff)      1.671      3  13.31 0.000286 ***



########
########
##  CORNERS
########
########

#[1] "Bundesliga"
#
# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq  p-value    
#   s(Score.Diff)        5.174      8 200.56  < 2e-16 ***
#   s(Minute.clean)      7.821      8 293.49  < 2e-16 ***
#   s(Weighted.Win.Prob) 4.010      8  24.67 6.77e-06 ***
#   s(RedCard.Diff)      1.710      3  22.55 2.58e-06 ***


# [1] "SerieA"
# 
# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq  p-value    
#   s(Score.Diff)        5.341      8 274.19  < 2e-16 ***
#   s(Minute.clean)      7.826      8 389.72  < 2e-16 ***
#   s(Weighted.Win.Prob) 3.949      8  24.46 6.85e-06 ***
#   s(RedCard.Diff)      2.265      3  29.40 1.08e-06 ***

# [1] "LaLiga"
#
# Approximate significance of smooth terms:
#   edf Ref.df  Chi.sq p-value    
#   s(Score.Diff)        5.385      8 264.182  <2e-16 ***
#   s(Minute.clean)      7.828      8 356.905  <2e-16 ***
#   s(Weighted.Win.Prob) 1.962      8   5.453  0.0372 *  
#   s(RedCard.Diff)      1.673      3  33.925  <2e-16 ***

# [1] "Ligue1"
# 
# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq p-value    
#   s(Score.Diff)        5.111      8 267.44  <2e-16 ***
#   s(Minute.clean)      7.853      8 380.52  <2e-16 ***
#   s(Weighted.Win.Prob) 4.387      8  29.56  <2e-16 ***
#   s(RedCard.Diff)      1.803      3  34.27  <2e-16 ***
  
  
  
# [1] "PremierLeague"
# 
# Approximate significance of smooth terms:
#   edf Ref.df Chi.sq  p-value    
#   s(Score.Diff)        4.723      8 176.50  < 2e-16 ***
#   s(Minute.clean)      7.832      8 314.05  < 2e-16 ***
#   s(Weighted.Win.Prob) 6.181      8  33.34  < 2e-16 ***
#   s(RedCard.Diff)      1.367      3  10.31 0.000899 ***