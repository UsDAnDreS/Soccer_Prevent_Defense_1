######
## Use the "REMOVE.EXTRA" to FIT THE MODEL, BUT...
##  follow it up by using REGULAR dataset for PREDICTIONS
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

gam.nb.obj <- list()


for (league in league.name){
  
  print(league)
  
  cat("\n")
  
  #######
  ### LOADING, SETTING UP DATA
  #######
  
  ## To be used for modeling (excluding bad minute entries)
  our.df <- read.csv(paste0(league, "_NO_EXTRA_TIME_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  ## To be used for prediction (including everything, so that totals correspond to full data)
  our.df.pred <- read.csv(paste0(league, "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  ## REMOVING NA betting data
  our.df <- our.df %>% filter(!is.na(Weighted.Win.Prob))
  dim(our.df)
  
  ## For EXTRA TIME: JUST FORCE IT TO BE EQUAL TO THE LAST MINUTE (e.g. for extra time in 1st half - make it =45, in 2nd half - =90)
  
  # our.df$Minute.clean[our.df$half_id == 1 & our.df$Minute.clean > 45] <- 45
  # our.df$Minute.clean[our.df$Minute.clean > 90] <- 90
  
  
  our.df  <- our.df  %>%
    mutate(abs.Score.Diff = abs(Score.Diff),
           abs.RedCard.Diff = abs(RedCard.Diff))
  
  our.df  <- our.df  %>%
    mutate(Period.ID = group_indices(our.df, .dots=c("ID", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
    arrange(Period.ID) %>%
    mutate(Period.ID = factor(Period.ID))
  
  our.df <- our.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
  
  
  ######
  ### Fitting the models
  ######
  
  gam.nb.obj[[league]] <- gam( Shots ~
                                s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5) + log(minutes.spent + 1),
                              family = "nb", data= our.df)
  
  
  
  #######
  ### GETTING ADJUSTMENTS
  #######
  
  
  ### 1. SCORE DIFFERENTIAL
  
  # Getting the DIFFERENTIALS (log-scale "linear" effects)
  all.score.diffs <- c(min(our.df.pred$Score.Diff):max(our.df.pred$Score.Diff))
  log.score.diff.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                               newdata = data.frame(minutes.spent=90, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                                                    Score.Diff = all.score.diffs))) -
    as.numeric(predict(gam.nb.obj[[league]],
                       newdata = data.frame(minutes.spent=90, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                            Score.Diff = rep(0, length(all.score.diffs)))))
  
  names(log.score.diff.effects) <- all.score.diffs
  log.score.diff.effects
  
  
  # Multiplicative effects
  exp(log.score.diff.effects)
  
  # Doing the multiplication
  our.df.pred$Adj.Coef.ScoreDiff <- 
    exp(-log.score.diff.effects)[sapply(our.df.pred$Score.Diff, function(x) which(names(log.score.diff.effects) == x))]
  
  
  
  
  ### 2. RED CARD DIFF
  
  all.redcard.diffs <- c(min(our.df.pred$RedCard.Diff):max(our.df.pred$RedCard.Diff))
  log.redcard.diff.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                                 newdata = data.frame(minutes.spent=90, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                                                      RedCard.Diff = all.redcard.diffs))) -
    as.numeric(predict(gam.nb.obj[[league]],
                       newdata = data.frame(minutes.spent=90, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                            RedCard.Diff = rep(0, length(all.redcard.diffs)))))
  
  names(log.redcard.diff.effects) <- all.redcard.diffs
  log.redcard.diff.effects
  
  # Multiplicative effects
  exp(log.redcard.diff.effects)
  
  # Doing the multiplication
  our.df.pred$Adj.Coef.RedCardDiff <- 
    exp(-log.redcard.diff.effects)[sapply(our.df.pred$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
  
  
  
  
  ### 3. HOME/AWAY FACTOR
  
  all.homeaway <- unique(our.df.pred$HomeAway)
  log.homeaway.effects <- as.numeric(predict(gam.nb.obj[[league]],
                                             newdata = data.frame(minutes.spent=90, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                                                  HomeAway = all.homeaway))) -
    as.numeric(predict(gam.nb.obj[[league]],
                       newdata = data.frame(minutes.spent=90, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                            HomeAway = rep("Home", length(all.homeaway)))))
  
  names(log.homeaway.effects) <- all.homeaway
  log.homeaway.effects
  
  # Multiplicative effects
  exp(log.homeaway.effects)
  
  # Doing the multiplication
  our.df.pred$Adj.Coef.HomeAway <- 
    exp(-log.homeaway.effects)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]

  
  ####
  ## DOING THE ADJUSTMENT
  ####
  
  our.df.pred <- our.df.pred %>%
    mutate(Shots.Adj = Shots*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway)
  
  
  ### 
  ## IF WE JUST WANT THE UGLY TOP-10 PRINTOUT
  ###
  
  cat("\n")
  print("Top-10 single game shifts:")
  
  final.df <- our.df.pred %>% 
    group_by(gameId, Team) %>% 
    summarise(Shots = sum(Shots),
              Shots.Adj = sum(Shots.Adj),
              Shots.Diff = Shots.Adj - Shots) %>% 
    arrange(desc(abs(Shots.Diff)))
  
  print(head(final.df, 10))
  
  
  # #######
  # ## IF WE WANT THE TABLE ENTRIES FOR TOP MOVEMENTS
  # #######
  # 
  # biggest.jump <- final.df %>% filter(Shots.Diff > 0) %>% dplyr::select(gameId, Team) %>% head(.,1)
  # biggest.fall <- final.df %>% filter(Shots.Diff < 0) %>% dplyr::select(gameId, Team) %>% head(.,1)
  # 
  # cat("\n")
  # print("Biggest jump and fall, breakdown:")
  # biggest.jump.fall <- our.df.pred %>%
  #   filter(gameId %in% c(biggest.jump$gameId, biggest.fall$gameId),
  #          Team %in% c(biggest.jump$Team, biggest.fall$Team)) %>%
  #   group_by(gameId, Team, gamedDate, HomeAway, Score.Diff, RedCard.Diff) %>%
  #   summarise(Shots = sum(Shots),
  #             Shots.Adj = sum(Shots.Adj),
  #             minutes.spent = sum(minutes.spent)) %>%
  #   mutate(Score.Diff = ifelse(Score.Diff < 0,
  #                              "Trail",
  #                              ifelse(Score.Diff > 0,
  #                                     "Lead",
  #                                     "0")),
  #          RedCard.Diff = ifelse(RedCard.Diff < 0,
  #                                "UpMen",
  #                                ifelse(RedCard.Diff > 0,
  #                                       "DownMen",
  #                                       "0")))
  # # print(biggest.jump.fall)
  # 
  # score.diffs <- biggest.jump.fall %>% 
  #   dplyr::select(-RedCard.Diff) %>%
  #   group_by(gameId, Team, gamedDate, Score.Diff) %>%
  #   summarise(Shots = sum(Shots), 
  #             # Shots.Adj = sum(Shots.Adj),
  #             minutes.spent = sum(minutes.spent)) %>%
  #   pivot_wider(names_from = c(Score.Diff),
  #               values_from = c(Shots, minutes.spent)) %>%
  #   replace(is.na(.), 0 ) %>%
  #   mutate(Shots.Total.Actual = Shots_0 + Shots_Trail + Shots_Lead,
  #          #Shots.Total.Adj = sum(Shots.Adj),
  #          Shots.Min.Lead = paste0(Shots_Lead, " (", minutes.spent_Lead, ")"),
  #          Shots.Min.Trail = paste0(Shots_Trail, " (", minutes.spent_Trail, ")")) %>%
  #   dplyr::select(Shots.Total.Actual,
  #                 #Shots.Total.Adj,
  #                 Shots.Min.Lead,# Minutes.Lead,
  #                 Shots.Min.Trail #, Minutes.Trail
  #   )
  # 
  # 
  # 
  # redcard.diffs <- biggest.jump.fall %>% 
  #   dplyr::select(-Score.Diff) %>%
  #   group_by(gameId, Team, gamedDate, RedCard.Diff) %>%
  #   summarise(Shots = sum(Shots), 
  #             # Shots.Adj = sum(Shots.Adj),
  #             minutes.spent = sum(minutes.spent)) %>%
  #   pivot_wider(names_from = c(RedCard.Diff),
  #               values_from = c(Shots, minutes.spent)) %>%
  #   replace(is.na(.), 0 ) %>%
  #   mutate(Shots.Total.Actual = Shots_0 + Shots_UpMen + Shots_DownMen,
  #          #Shots.Total.Adj = sum(Shots.Adj),
  #          Shots.Min.UpMen = paste0(Shots_UpMen, " (", minutes.spent_UpMen, ")"),
  #          Shots.Min.DownMen = paste0(Shots_DownMen, " (", minutes.spent_DownMen, ")")) %>%
  #   dplyr::select(Shots.Total.Actual,
  #                 #Shots.Total.Adj,
  #                 Shots.Min.UpMen,# Minutes.Lead,
  #                 Shots.Min.DownMen #, Minutes.Trail
  #   )
  # 
  # 
  # cat("\n")
  # # print(score.diffs %>% left_join(redcard.diffs))
  # all.stuff <- score.diffs %>% left_join(redcard.diffs)
  # 
  # all.stuff <- all.stuff %>% 
  #   left_join(biggest.jump.fall %>% group_by(Team, gameId) %>% summarise(Shots.Adj = round(sum(Shots.Adj), 1)), 
  #                                                                by=c("Team", "gameId"))
  # all.stuff <- all.stuff[, c("gameId", "Team", "gamedDate",  
  #               "Shots.Total.Actual", "Shots.Adj", 
  #               "Shots.Min.Lead", "Shots.Min.Trail", 
  #               "Shots.Min.UpMen", "Shots.Min.DownMen")]
  #   
  # 
  # for (j in 1:nrow(all.stuff)){
  #   print(paste0(all.stuff[j,-1], collapse=" & "))
  # }
  # 
  
  
  cat("\n")
  cat("\n")
}




####################################################################################
####################################################################################
############## INCLUDING THE EXTRA TIME (45+ in 1st half; 90+ in 2nd half)
####################################################################################
####################################################################################


############
############
###  SHOTS
############
############


### BUNDESLIGA

# gameId Team                Shots Shots.Adj.No.Minute Shots.Diff.No.Minute
# <int> <chr>               <int>               <dbl>                <dbl>
# 1 576779 Bayern Munich          15                28.2                13.2 
# 2 273207 Bayer Leverkusen       21                32.9                11.9 
# 3 369895 VfL Wolfsburg          13                23.6                10.6 
# 4 252572 TSG Hoffenheim         19                29.5                10.5 
# 5 517592 TSG Hoffenheim         19                28.4                 9.44
# 6 297218 VfB Stuttgart          11                20.4                 9.40
# 7 297218 Eintracht Frankfurt    30                20.8                -9.16
# 8 346209 Borussia Dortmund      14                23.0                 9.00
# 9 396945 Bayer Leverkusen       15                23.2                 8.15
# 10 346241 VfL Wolfsburg          26                18.1                -7.94


## SERIE A

# [1] "Top-10 single game shifts, WITH MINUTE:"

# gameId Team           Shots Shots.Adj.No.Minute Shots.Diff.No.Minute
# <int> <chr>          <int>               <dbl>                <dbl>
# 1 614565 AS Roma           43                27.1                -15.9
# 2 554298 AC Milan          39                24.3                -14.7
# 3 251416 Fiorentina        34                20.0                -14.0
# 4 278257 AS Roma           17                29.3                 12.3
# 5 491648 Napoli            27                38.9                 11.9
# 6 491540 Internazionale    40                51.9                 11.9
# 7 583014 Benevento         25                13.2                -11.8
# 8 377271 Internazionale    23                34.6                 11.6
# 9 644854 Napoli            18                29.2                 11.2
# 10 251390 Juventus          18                28.7                 10.7


## LA LIGA

# gameId Team        Shots Shots.Adj.No.Minute Shots.Diff.No.Minute
# <int> <chr>       <int>               <dbl>                <dbl>
# 1 301979 Real Madrid    18                34.0                16.0 
# 2 324018 Real Madrid    40                55.0                15.0 
# 3 373084 Real Madrid    35                47.4                12.4 
# 4 550610 Real Madrid    17                29.2                12.2 
# 5 275886 Real Madrid    23                34.9                11.9 
# 6 253063 Real Betis     21                31.5                10.5 
# 7 275872 Getafe         22                11.6               -10.4 
# 8 323761 Barcelona      18                28.1                10.1 
# 9 348350 Real Madrid    24                34.0                 9.98
# 10 275626 Barcelona      18                27.8                 9.83


### LIGUE 1

# gameId Team                  Shots Shots.Adj.No.Minute Shots.Diff.No.Minute
# <int> <chr>                 <int>               <dbl>                <dbl>
# 1 609246 Bordeaux                 31                13.4               -17.6 
# 2 271426 Marseille                35                21.7               -13.3 
# 3 424698 Caen                     15                27.2                12.2 
# 4 342759 Paris Saint-Germain      26                37.9                11.9 
# 5 482105 Paris Saint-Germain      13                24.4                11.4 
# 6 342665 Evian Thonon Gaillard    29                17.6               -11.4 
# 7 342835 Paris Saint-Germain      22                11.6               -10.4 
# 8 295037 Caen                     15                25.3                10.3 
# 9 609334 Brest                    26                16.0                -9.97
# 10 271589 Lille                    28                18.3                -9.68


### PREMIER LEAGUE

# gameId Team                 Shots Shots.Adj.No.Minute Shots.Diff.No.Minute
# <int> <chr>                <int>               <dbl>                <dbl>
# 1 293077 West Bromwich Albion    14                30.9                 16.9
# 2 450630 Arsenal                 17                32.9                 15.9
# 3 345773 Manchester City         24                39.8                 15.8
# 4 293077 Blackpool               26                12.6                -13.4
# 5 345674 Manchester City         15                27.0                 12.0
# 6 318258 Liverpool               16                27.8                 11.8
# 7 367396 Hull City               25                13.6                -11.4
# 8 480897 Chelsea                 19                30.1                 11.1
# 9 395498 Manchester City         42                31.0                -11.0
# 10 422633 Stoke City              13                23.9                 10.9






############
############
###  CORNERS
############
############


## BUNDESLIGA

# gameId Team                Corners Corners.Adj.No.Minute Corners.Diff.No.Minute
# <int> <chr>                 <int>                 <dbl>                  <dbl>
# 1 346106 VfL Wolfsburg             9                 16.0                    6.98
# 2 548077 Bayern Munich            10                 15.2                    5.17
# 3 608577 Bayer Leverkusen          6                 11.1                    5.08
# 4 576575 Bayern Munich            10                 15.0                    5.03
# 5 297218 Eintracht Frankfurt      13                  8.17                  -4.83
# 6 320892 Bayern Munich            10                 14.8                    4.75
# 7 252536 Bayern Munich            14                 18.4                    4.36
# 8 517758 Bayern Munich            14                 18.3                    4.35
# 9 320996 Nurnberg                 13                  8.76                  -4.24
# 10 548261 TSG Hoffenheim           14                  9.81                  -4.19

## SERIE A

# [1] "Top-10 single game shifts, WITH MINUTE:"

# gameId Team           Corners Corners.Adj.No.Minute Corners.Diff.No.Minute
# <int> <chr>            <int>                 <dbl>                  <dbl>
# 1 491540 Internazionale      18                 26.8                    8.82
# 2 351984 AS Roma             15                 23.3                    8.28
# 3 614565 AS Roma             20                 12.1                   -7.90
# 4 644854 Napoli              13                 20.9                    7.86
# 5 554298 AC Milan            17                  9.36                  -7.64
# 6 582962 Genoa               11                 18.6                    7.58
# 7 461216 Napoli               7                 14.5                    7.50
# 8 461277 Juventus            14                 20.7                    6.74
# 9 614749 Internazionale      11                 17.6                    6.56
# 10 582835 Napoli              11                 17.2                    6.18


## LA LIGA

# gameId Team            Corners Corners.Adj.No.Minute Corners.Diff.No.Minute
# <int> <chr>             <int>                 <dbl>                  <dbl>
# 1 582165 Barcelona             8                 18.1                   10.1 
# 2 301979 Real Madrid           9                 16.4                    7.44
# 3 402554 Mlaga                13                 20.2                    7.15
# 4 275872 Getafe               14                  6.98                  -7.02
# 5 373169 Barcelona            14                 20.8                    6.80
# 6 490655 Real Madrid          13                 19.4                    6.42
# 7 252805 Real Valladolid       9                 15.4                    6.39
# 8 372983 Real Madrid          10                 16.4                    6.36
# 9 372953 Mlaga                10                 16.2                    6.15
# 10 323761 Barcelona             8                 13.8                    5.77


### LIGUE 1

# gameId Team                  Corners Corners.Adj.No.Minute Corners.Diff.No.Minute
# <int> <chr>                   <int>                 <dbl>                  <dbl>
# 1 342835 Paris Saint-Germain        18                  8.38                  -9.62
# 2 609246 Bordeaux                   11                  4.05                  -6.95
# 3 271589 Lille                      17                 10.3                   -6.70
# 4 482047 Marseille                  11                 17.5                    6.49
# 5 342665 Evian Thonon Gaillard      14                  7.84                  -6.16
# 6 609334 Brest                      14                  7.89                  -6.11
# 7 609326 Marseille                  15                  9.08                  -5.92
# 8 639931 Nice                       14                 19.9                    5.89
# 9 482105 Paris Saint-Germain         7                 12.8                    5.84
# 10 542219 Lille                      10                 15.6                    5.56


### PREMIER LEAGUE

# gameId Team              Corners Corners.Adj.No.Minute Corners.Diff.No.Minute
# <int> <chr>               <int>                 <dbl>                  <dbl>
# 1 345674 Manchester City         9                 15.2                    6.20
# 2 318142 Tottenham Hotspur      19                 24.0                    5.03
# 3 513513 Manchester City        17                 22.0                    4.99
# 4 513478 AFC Bournemouth        10                  5.21                  -4.79
# 5 367271 Swansea City           11                  6.40                  -4.60
# 6 293122 Newcastle United       12                  7.52                  -4.48
# 7 345784 Manchester City        15                 19.3                    4.32
# 8 367214 Manchester City         4                  8.29                   4.29
# 9 292991 Chelsea                14                 18.3                    4.26
# 10 480613 Huddersfield Town      12                  7.74                  -4.26


######
## THE LARGEST JUMPS & DROPS ACROSS ALL 5 LEAGUES HAPPENED IN FRENCH LIGUE 1 in:
##
## * SHOTS:
# 1 271469 Lens                     29                42.6                  41.3                13.6                   12.3 
# 2 609246 Bordeaux                 31                22.2                  21.4                -8.81                  -9.62
##
## * CORNERS:
# 1 271469 Lens                     10                 19.2                    18.7                    9.19                     8.65
# 2 342835 Paris Saint-Germain      18                 13.1                    12.8                   -4.90                    -5.18
#####


#######
## LARGEST JUMPS in SHOTS & CORNERS: Lens
#######

# 271469 Lens (vs Stade Rennais, Lens was at home)
## BAD GAME... incorrectly converted the commentary; 
##  should've been 2-2 instead of 4-0, and red cards were 1 each, not 2 for one team...
## SEEMS LIKE "RENNES" GOT CONFUSED WITH "Lens" as being closer to it as opposed to "Stade Rennais"

## 491606 Chievo Verona
## BAD GAME... was actually 3-2, not 5-0... it's "Chievo Verona vs Hellas Verona", so the NAMES GOT CONFUSED DUE TO "VERONA"

## Valenciennes - confused with Rennes, BAD GAMES



####
## LARGEST JUMP in SHOTS:
####






####
## LARGEST DROP in SHOTS:
##
## gameId == 609246, Team == "Bordeaux" (at home vs Montpellier)
####

#  Went 31 shots -> 21.4 shots (lost)
#
#  Down 0-1 on min 12
#  Down 0-2 on min 17
#  Opponent gets red card on min 39
#  Opponent gets 2nd red card on min 45

# They didn't make a single shot until minute 45 lol...
# Minutes:  [1] 45 45 45 45 46 50 52 55 60 62 67 69 70 74 76 77 80 81 82 83 84 86 88 89 90 90




#### IN BUNDESLIGA:

## 1. BIGGEST JUMP - Wolfsburg in game 346106 (9 -> 11.5)
##    Played away, vs Eintracht Frankfurt
##    1-0 on minute 9, 2-0 on minute 20, 
##    GOT RED CARD ON MIN 35
##    2-1 on min 37
##    2-2 on min 90
##    3-2
##  had 9 corners, 7 of which happened when down a man, and all but 1 happened when up in the score.
##  LOTS OF THEM HAPPENED IN THE 2nD HALF

## 2. BIGGEST DROP - Eintracht Frankfurt in game 297218 (13 -> 11.2)
##   Played at home, Opponent - VfB Stuttgart
##    Opponent got red card on min 15
##    Went down 0-1 on min 65, and DOWN 0-2 on min 69
##  All corners happened when up 1 man, 5 took place when down 1/2 goals
##  4 corners in 1st half, starting min 24-25,  9 in 2nd








View(our.df %>% 
       # filter(gameId == 346106, Team == "VfL Wolfsburg")
       #  filter(gameId == 297218, Team == "Eintracht Frankfurt")
       # filter(gameId == 271469, Team == "Lens" )
       # filter(gameId == 609246, Team == "Bordeaux"
       # filter(gameId == 491606, Team == "Chievo Verona")
       # filter(gameId == 342604, Team == "Valenciennes")
       filter(gameId == 278524, Team == "AC Milan")
)

# Checking the times of shots
# our.df$Minute.clean[our.df$gameId == 609246 & our.df$Team == "Bordeaux" & our.df$Shots > 0]














####################################################################################
####################################################################################
############## EXCLUDING THE EXTRA TIME FOR BAD GAMES (45+ in 1st half; 90+ in 2nd half)
####################################################################################
####################################################################################


############
############
###  SHOTS
############
############


# [1] "Bundesliga"
# 
# 
# [1] "Top-10 single game shifts, WITH MINUTE:"
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 10 × 5
# # Groups:   gameId [9]
# gameId Team                Shots Shots.Adj.No.Minute Shots.Diff.No.Minute
# <int> <chr>               <int>               <dbl>                <dbl>
#   1 576779 Bayern Munich          15                28.5                13.5 
# 2 273207 Bayer Leverkusen       21                33.5                12.5 
# 3 369895 VfL Wolfsburg          13                24.5                11.5 
# 4 252572 TSG Hoffenheim         19                30.2                11.2 
# 5 517592 TSG Hoffenheim         19                28.9                 9.89
# 6 297218 VfB Stuttgart          11                20.8                 9.85
# 7 346209 Borussia Dortmund      14                23.4                 9.44
# 8 297218 Eintracht Frankfurt    30                21.1                -8.89
# 9 396945 Bayer Leverkusen       15                23.6                 8.55
# 10 320947 Hamburg SV             16                24.1                 8.08
# 
# 
# [1] "SerieA"
# 
# 
# [1] "Top-10 single game shifts, WITH MINUTE:"
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 10 × 5
# # Groups:   gameId [10]
# gameId Team           Shots Shots.Adj.No.Minute Shots.Diff.No.Minute
# <int> <chr>          <int>               <dbl>                <dbl>
#   1 614565 AS Roma           43                27.6                -15.4
# 2 554298 AC Milan          39                24.8                -14.2
# 3 251416 Fiorentina        34                20.4                -13.6
# 4 278257 AS Roma           17                30.1                 13.1
# 5 644854 Napoli            18                29.8                 11.8
# 6 491540 Internazionale    40                51.6                 11.6
# 7 251390 Juventus          18                29.5                 11.5
# 8 583014 Benevento         25                13.5                -11.5
# 9 491648 Napoli            27                38.5                 11.5
# 10 432053 Juventus          29                40.4                 11.4
# 
# 
# [1] "LaLiga"
# 
# 
# [1] "Top-10 single game shifts, WITH MINUTE:"
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 10 × 5
# # Groups:   gameId [10]
# gameId Team           Shots Shots.Adj.No.Minute Shots.Diff.No.Minute
# <int> <chr>          <int>               <dbl>                <dbl>
#   1 301979 Real Madrid       18                35.0                 17.0
# 2 324018 Real Madrid       40                55.1                 15.1
# 3 550610 Real Madrid       17                29.9                 12.9
# 4 275886 Real Madrid       23                35.9                 12.9
# 5 373084 Real Madrid       35                47.4                 12.4
# 6 253063 Real Betis        21                32.8                 11.8
# 7 433669 Rayo Vallecano    16                27.2                 11.2
# 8 323761 Barcelona         18                28.7                 10.7
# 9 348350 Real Madrid       24                34.6                 10.6
# 10 275626 Barcelona         18                28.5                 10.5
# 
# 
# [1] "Ligue1"
# 
# 
# [1] "Top-10 single game shifts, WITH MINUTE:"
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 10 × 5
# # Groups:   gameId [10]
# gameId Team                  Shots Shots.Adj.No.Minute Shots.Diff.No.Minute
# <int> <chr>                 <int>               <dbl>                <dbl>
#   1 609246 Bordeaux                 31                13.5               -17.5 
# 2 271426 Marseille                35                21.9               -13.1 
# 3 424698 Caen                     15                27.6                12.6 
# 4 342759 Paris Saint-Germain      26                38.5                12.5 
# 5 482105 Paris Saint-Germain      13                24.9                11.9 
# 6 342665 Evian Thonon Gaillard    29                17.8               -11.2 
# 7 295037 Caen                     15                25.7                10.7 
# 8 342835 Paris Saint-Germain      22                11.6               -10.4 
# 9 541990 Toulouse                 18                28.2                10.2 
# 10 609334 Brest                    26                16.2                -9.84
# 
# 
# [1] "PremierLeague"
# 
# 
# [1] "Top-10 single game shifts, WITH MINUTE:"
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 10 × 5
# # Groups:   gameId [9]
# gameId Team                 Shots Shots.Adj.No.Minute Shots.Diff.No.Minute
# <int> <chr>                <int>               <dbl>                <dbl>
#   1 293077 West Bromwich Albion    14                31.3                 17.3
# 2 345773 Manchester City         24                40.4                 16.4
# 3 450630 Arsenal                 17                33.2                 16.2
# 4 293077 Blackpool               26                12.9                -13.1
# 5 345674 Manchester City         15                27.3                 12.3
# 6 318258 Liverpool               16                28.1                 12.1
# 7 480897 Chelsea                 19                30.6                 11.6
# 8 422633 Stoke City              13                24.1                 11.1
# 9 367396 Hull City               25                13.9                -11.1
# 10 367324 Everton                 25                36.0                 11.0






############
############
###  CORNERS
############
############


# [1] "Bundesliga"
# 
# 
# [1] "Top-10 single game shifts, WITH MINUTE:"
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 10 × 5
# # Groups:   gameId [10]
# gameId Team                Corners Corners.Adj.No.Minute Corners.Diff.No.Minute
# <int> <chr>                 <int>                 <dbl>                  <dbl>
#   1 346106 VfL Wolfsburg             9                 16.3                    7.29
# 2 548077 Bayern Munich            10                 15.4                    5.40
# 3 608577 Bayer Leverkusen          6                 11.3                    5.31
# 4 576575 Bayern Munich            10                 15.0                    5.05
# 5 320892 Bayern Munich            10                 15.0                    4.98
# 6 252572 TSG Hoffenheim           10                 14.8                    4.77
# 7 297218 Eintracht Frankfurt      13                  8.24                  -4.76
# 8 252536 Bayern Munich            14                 18.7                    4.67
# 9 320996 Nurnberg                 13                  8.82                  -4.18
# 10 548261 TSG Hoffenheim           14                  9.85                  -4.15
# 
# 
# [1] "SerieA"
# 
# 
# [1] "Top-10 single game shifts, WITH MINUTE:"
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 10 × 5
# # Groups:   gameId [10]
# gameId Team           Corners Corners.Adj.No.Minute Corners.Diff.No.Minute
# <int> <chr>            <int>                 <dbl>                  <dbl>
#   1 351984 AS Roma             15                 23.5                    8.54
# 2 491540 Internazionale      18                 26.5                    8.49
# 3 644854 Napoli              13                 21.2                    8.22
# 4 461216 Napoli               7                 15.1                    8.11
# 5 614565 AS Roma             20                 12.1                   -7.92
# 6 554298 AC Milan            17                  9.37                  -7.63
# 7 582962 Genoa               11                 18.5                    7.53
# 8 432131 Genoa                9                 16.4                    7.37
# 9 461277 Juventus            14                 20.8                    6.84
# 10 614749 Internazionale      11                 17.5                    6.48
# 
# 
# [1] "LaLiga"
# 
# 
# [1] "Top-10 single game shifts, WITH MINUTE:"
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 10 × 5
# # Groups:   gameId [10]
# gameId Team            Corners Corners.Adj.No.Minute Corners.Diff.No.Minute
# <int> <chr>             <int>                 <dbl>                  <dbl>
#   1 582165 Barcelona             8                 19.0                   11.0 
# 2 301979 Real Madrid           9                 17.3                    8.31
# 3 433669 Rayo Vallecano        9                 17.0                    7.99
# 4 402554 Mlaga                13                 20.3                    7.25
# 5 252805 Real Valladolid       9                 15.9                    6.95
# 6 373169 Barcelona            14                 20.8                    6.80
# 7 275872 Getafe               14                  7.31                  -6.69
# 8 372953 Mlaga                10                 16.5                    6.54
# 9 490655 Real Madrid          13                 19.5                    6.47
# 10 372983 Real Madrid          10                 16.4                    6.41
# 
# 
# [1] "Ligue1"
# 
# 
# [1] "Top-10 single game shifts, WITH MINUTE:"
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 10 × 5
# # Groups:   gameId [10]
# gameId Team                  Corners Corners.Adj.No.Minute Corners.Diff.No.Minute
# <int> <chr>                   <int>                 <dbl>                  <dbl>
# 1 342835 Paris Saint-Germain        18                  8.50                  -9.50
# 2 609246 Bordeaux                   11                  4.14                  -6.86
# 3 482047 Marseille                  11                 17.8                    6.77
# 4 271589 Lille                      17                 10.4                   -6.62
# 5 482105 Paris Saint-Germain         7                 13.1                    6.10
# 6 342665 Evian Thonon Gaillard      14                  7.90                  -6.10
# 7 609334 Brest                      14                  7.95                  -6.05
# 8 639931 Nice                       14                 19.9                    5.92
# 9 609326 Marseille                  15                  9.15                  -5.85
# 10 542219 Lille                      10                 15.8                    5.84
# 
# 
# [1] "PremierLeague"
# 
# 
# [1] "Top-10 single game shifts, WITH MINUTE:"
# `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
# # A tibble: 10 × 5
# # Groups:   gameId [10]
# gameId Team              Corners Corners.Adj.No.Minute Corners.Diff.No.Minute
# <int> <chr>               <int>                 <dbl>                  <dbl>
#   1 345674 Manchester City         9                 15.5                    6.51
# 2 318142 Tottenham Hotspur      19                 24.6                    5.56
# 3 513513 Manchester City        17                 22.0                    5.00
# 4 513478 AFC Bournemouth        10                  5.36                  -4.64
# 5 367214 Manchester City         4                  8.58                   4.58
# 6 367271 Swansea City           11                  6.51                  -4.49
# 7 422633 Stoke City              6                 10.5                    4.46
# 8 293122 Newcastle United       12                  7.61                  -4.39
# 9 345784 Manchester City        15                 19.3                    4.33
# 10 292991 Chelsea                14                 18.3                    4.31
