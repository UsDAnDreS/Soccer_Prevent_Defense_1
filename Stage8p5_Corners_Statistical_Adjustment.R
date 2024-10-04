######
######

library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)



league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

# load(file="gam_ziP_obj_Corners.Robj")

glm.extreme.obj <- list()

extr.score <- c(1, 2, 3, 4, 5)[2]


for (league in league.name){
  
  print(league)
  
  cat("\n")
  
  #######
  ### LOADING, SETTING UP DATA
  #######
  
  check.df <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  our.df <- read.csv(paste0(league, "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
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
    mutate(Period.ID = group_indices(our.df, .dots=c("ID", "minutes.spent", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
    arrange(Period.ID) %>%
    mutate(Period.ID = factor(Period.ID))
  
  our.df <- our.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
  
  ### Fitting the model
  
  # Creating the extreme stuff
  our.df <- our.df %>% mutate(ID = factor(ID),
                              Score.Diff = relevel(factor(
                                ifelse(Score.Diff >= extr.score,
                                       paste0(extr.score, ".or.better"),
                                       ifelse(Score.Diff <= -extr.score,
                                              paste0(-extr.score, ".or.worse"),
                                              Score.Diff))), ref="0"))
  
  
  glm.extreme.obj[[league]] <- glm.nb(Corners ~ Score.Diff + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
                                      data=our.df)
  
  
  
  #######
  ### GETTING ADJUSTMENTS
  #######
  
  
  
  ### 1. SCORE DIFFERENTIAL
  
  glm.extreme.obj[[league]]$coefficients
  
  log.score.diff.effects <- c(Score.Diff0 = 0,
                              glm.extreme.obj[[league]]$coefficients[str_detect(names(glm.extreme.obj[[league]]$coefficients), "Score.Diff")])
  
  # Multiplicative effects
  exp(log.score.diff.effects)
  
  # Doing the multiplication
  our.df$Adj.Coef.ScoreDiff <- 
    exp(-log.score.diff.effects)[sapply(our.df$Score.Diff, function(x) which(names(log.score.diff.effects) == paste0("Score.Diff", x)))]
  
  
  ### 2. RED CARD DIFF
  
  log.redcard.diff.effects <- c(-3:3)*glm.extreme.obj[[league]]$coefficients["RedCard.Diff"]
  names(log.redcard.diff.effects) <- c(-3:3)
  
  # Multiplicative effects
  exp(log.redcard.diff.effects)
  
  
  # Doing the multiplication
  our.df$Adj.Coef.RedCardDiff <- 
    exp(-log.redcard.diff.effects)[sapply(our.df$RedCard.Diff, function(x) which(names(log.redcard.diff.effects) == x))]
  
  
  
  ### 3. HOME/AWAY FACTOR
  
  all.homeaway <- unique(our.df$HomeAway)
  
  log.homeaway.effects <- c(0, -glm.extreme.obj[[league]]$coefficients["HomeAwayHome"])
  names(log.homeaway.effects) <- c("Home", "Away")
  
  # Multiplicative effects
  exp(log.homeaway.effects)
  
  # Doing the multiplication
  our.df$Adj.Coef.HomeAway <- 
    exp(-log.homeaway.effects)[sapply(our.df$HomeAway, function(x) which(names(log.homeaway.effects) == x))]
  
  
  
  our.df <- our.df %>%
    mutate(Corners.Adj = Corners*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway)
  
  
  ### 
  
  cat("\n")
  print("Top-10 single game shifts:")
  final.df <- our.df %>%
    group_by(gameId, gamedDate, Team) %>%
    summarise(Corners = sum(Corners),
              Corners.Adj = sum(Corners.Adj),
              Corners.Diff = Corners.Adj - Corners,) %>%
    arrange(desc(abs(Corners.Diff)))
  
  #  print(head(final.df, 10))
  
  biggest.jump <- final.df %>% filter(Corners.Diff > 0) %>% dplyr::select(gameId, Team) %>% head(.,1)
  biggest.fall <- final.df %>% filter(Corners.Diff < 0) %>% dplyr::select(gameId, Team) %>% head(.,1)
  
  cat("\n")
  print("Biggest jump and fall, breakdown:")
  biggest.jump.fall <- our.df %>%
    filter(gameId %in% c(biggest.jump$gameId, biggest.fall$gameId),
           Team %in% c(biggest.jump$Team, biggest.fall$Team)) %>%
    group_by(gameId, Team, gamedDate, HomeAway, Score.Diff, RedCard.Diff) %>%
    summarise(Corners = sum(Corners),
              Corners.Adj = sum(Corners.Adj),
              minutes.spent = sum(minutes.spent)) %>%
    mutate(Score.Diff = ifelse(Score.Diff %in% c("-1", "-2.or.worse"),
                               "Trail",
                               ifelse(Score.Diff %in% c("1", "2.or.better"),
                                      "Lead",
                                      "0")),
           RedCard.Diff = ifelse(RedCard.Diff < 0,
                                 "UpMen",
                                 ifelse(RedCard.Diff > 0,
                                        "DownMen",
                                        "0")))
  # print(biggest.jump.fall)
  
  score.diffs <- biggest.jump.fall %>% 
    dplyr::select(-RedCard.Diff) %>%
    group_by(gameId, Team, gamedDate, Score.Diff) %>%
    summarise(Corners = sum(Corners), 
              # Corners.Adj = sum(Corners.Adj),
              minutes.spent = sum(minutes.spent)) %>%
    pivot_wider(names_from = c(Score.Diff),
                values_from = c(Corners, minutes.spent)) %>%
    replace(is.na(.), 0 ) %>%
    ungroup() %>%
    mutate(  Corners.Total.Actual = rowSums(dplyr::select(., starts_with("Corners_"))),
             #Corners.Total.Adj = sum(Corners.Adj),
             Corners.Min.Lead = if ("Corners_Lead" %in% names(.)) {paste0(Corners_Lead, " (", minutes.spent_Lead, ")")} else {"0 (0)"},
             Corners.Min.Trail = if ("Corners_Trail" %in% names(.)) {paste0(Corners_Trail, " (", minutes.spent_Trail, ")")} else {"0 (0)"},) %>%
    dplyr::select(gameId,Team, gamedDate, 
                  Corners.Total.Actual,
                  #Corners.Total.Adj,
                  Corners.Min.Lead,# Minutes.Lead,
                  Corners.Min.Trail #, Minutes.Trail
    )

  
  
  redcard.diffs <- biggest.jump.fall %>% 
    dplyr::select(-Score.Diff) %>%
    group_by(gameId, Team, gamedDate, RedCard.Diff) %>%
    summarise(Corners = sum(Corners), 
              # Corners.Adj = sum(Corners.Adj),
              minutes.spent = sum(minutes.spent)) %>%
    pivot_wider(names_from = c(RedCard.Diff),
                values_from = c(Corners, minutes.spent)) %>%
    replace(is.na(.), 0 ) %>%
    ungroup() %>%
    mutate(# Corners.Total.Actual = Corners_0 + Corners_UpMen + Corners_DownMen,
           Corners.Total.Actual = rowSums(dplyr::select(., starts_with("Corners_"))),
           #Corners.Total.Adj = sum(Corners.Adj),
           Corners.Min.UpMen = if ("Corners_UpMen" %in% names(.)) {paste0(Corners_UpMen, " (", minutes.spent_UpMen, ")")} else {"0 (0)"},
           Corners.Min.DownMen = if ("Corners_DownMen" %in% names(.)) {paste0(Corners_DownMen, " (", minutes.spent_DownMen, ")")} else {"0 (0)"}
           ) %>%
    dplyr::select(gameId,Team, gamedDate, 
                  Corners.Total.Actual,
                  #Corners.Total.Adj,
                  Corners.Min.UpMen,# Minutes.Lead,
                  Corners.Min.DownMen #, Minutes.Trail
    )
  
  
  cat("\n")
  print(score.diffs %>% left_join(redcard.diffs))
  all.stuff <- score.diffs %>% left_join(redcard.diffs)
  
  for (j in 1:nrow(all.stuff)){
    print(paste0(all.stuff[j,-1], collapse=" & "))
  }
  
  
  # View(print(biggest.jump.fall %>% 
  #         dplyr::select(-Score.Diff) %>%
  #         group_by(gameId, Team, RedCard.Diff) %>%
  #         summarise(Corners = sum(Corners), 
  #                   minutes.spent = sum(minutes.spent)) %>%
  #         pivot_wider(names_from =c(RedCard.Diff),
  #                     values_from = c(Corners, minutes.spent))))
  
  
  cat("\n")
  cat("\n")
}




# [1] "Bundesliga"
# 
# 
# gameId gamedDate  Team                Corners Corners.Adj Corners.Diff
# <int> <chr>      <chr>                 <int>       <dbl>        <dbl>
# 1 346106 2013-05-18 VfL Wolfsburg             9       15.2          6.15
# 2 517758 2018-12-15 Bayern Munich            14       19.5          5.49
# 3 576575 2020-10-17 Bayern Munich            10       15.3          5.26
# 4 297218 2011-02-27 Eintracht Frankfurt      13        8.31        -4.69
# 5 608577 2021-09-19 Bayer Leverkusen          6       10.5          4.54
# 6 608704 2022-01-15 Bayern Munich            12       16.5          4.53
# 7 548077 2020-06-16 Bayern Munich            10       14.5          4.52
# 8 320996 2011-09-17 Nurnberg                 13        8.67        -4.33
# 9 320892 2011-12-16 Bayern Munich            10       14.2          4.24
# 10 548261 2019-11-24 TSG Hoffenheim          14       10.0         -3.96
# 
# 
# [1] "SerieA"
# 
# 
# gameId gamedDate  Team           Corners Corners.Adj Corners.Diff
# <int> <chr>      <chr>            <int>       <dbl>        <dbl>
# 1 351984 2012-11-04 AS Roma             15        24.7         9.65
# 2 644854 2023-02-25 Napoli              13        22.0         9.02
# 3 461277 2016-09-10 Juventus            14        22.3         8.34
# 4 461216 2016-10-23 Napoli               7        15.2         8.22
# 5 582962 2021-01-31 Genoa               11        19.1         8.08
# 6 491540 2017-12-03 Internazionale      18        26.1         8.06
# 7 582835 2021-04-26 Napoli              11        18.6         7.58
# 8 614565 2022-05-14 AS Roma             20        12.5        -7.46
# 9 554298 2020-07-01 AC Milan            17        10.0        -6.97
# 10 614749 2021-12-17 Internazionale      11        17.8         6.82
# 
# 
# [1] "LaLiga"
# 
# 
# gameId gamedDate  Team            Corners Corners.Adj Corners.Diff
# <int> <chr>      <chr>             <int>       <dbl>        <dbl>
# 1 582165 2020-10-01 Barcelona             8       18.5         10.5 
# 2 373169 2013-09-21 Barcelona            14       22.2          8.18
# 3 402554 2014-10-18 Mlaga                13       21.0          8.04
# 4 372983 2014-02-16 Real Madrid          10       17.4          7.38
# 5 301979 2011-02-13 Real Madrid           9       16.1          7.14
# 6 490655 2017-09-23 Real Madrid          13       20.1          7.08
# 7 302157 2011-05-21 Real Madrid          15       21.7          6.69
# 8 275872 2009-11-07 Getafe               14        7.38        -6.62
# 9 252805 2008-09-13 Real Valladolid       9       15.6          6.62
# 10 582118 2020-11-08 Real Sociedad        15       21.5          6.48
# 
# 
# [1] "Ligue1"
# 
# gameId gamedDate  Team                  Corners Corners.Adj Corners.Diff
# <int> <chr>      <chr>                   <int>       <dbl>        <dbl>
# 1 342835 2012-11-17 Paris Saint-Germain        18        8.57        -9.43
# 2 639931 2023-04-30 Nice                       14       20.8          6.84
# 3 609246 2022-03-20 Bordeaux                   11        4.38        -6.62
# 4 271589 2010-05-08 Lille                      17       10.6         -6.42
# 5 542140 2019-10-18 Paris Saint-Germain        13       19.1          6.06
# 6 482047 2018-05-11 Marseille                  11       17.0          6.00
# 7 342665 2013-03-30 Evian Thonon Gaillard      14        8.08        -5.92
# 8 449175 2016-11-30 Lyon                       10       15.9          5.89
# 9 482105 2018-03-14 Paris Saint-Germain         7       12.8          5.79
# 10 609334 2022-01-09 Brest                      14        8.22        -5.78
# 
# 
# [1] "PremierLeague"
# 
# gameId gamedDate  Team              Corners Corners.Adj Corners.Diff
# <int> <chr>      <chr>               <int>       <dbl>        <dbl>
# 1 345674 2012-12-29 Manchester City         9       15.7          6.69
# 2 513513 2019-04-03 Manchester City        17       22.8          5.84
# 3 292991 2011-03-07 Chelsea                14       19.2          5.22
# 4 269930 2010-01-16 Chelsea                15       20.2          5.16
# 5 345784 2012-10-06 Manchester City        15       20.1          5.10
# 6 638144 2023-04-27 Newcastle United       14       19.0          4.99
# 7 318142 2012-05-06 Tottenham Hotspur      19       23.9          4.93
# 8 605855 2021-12-26 Manchester City        14       18.7          4.72
# 9 513478 2019-05-04 AFC Bournemouth        10        5.33        -4.67
# 10 480591 2018-03-31 Manchester City        11       15.6          4.64




####
# LARGEST DROP IN CORNERS:
####

## 342835  Paris Saint-Germain at home vs Stade Rennais (18 -> 8.57, 9.5 corners) 
## 0-1 on 13th min, 
## 1-1 on 21st
## opp red card on 26th
## 1-2 on 34th (opp goal)
## opp 2nd red card on 52nd
#
## More than half the corners came when up 2 men and down 1 in score
## 16 out of 18 happened when up 1 man
## ALL took place after the 1st goal by opponent (so when they started trailing)


### LARGEST DROPS IN EACH LEAGUE:

# 4 297218 2011-02-27 Eintracht Frankfurt     13        8.31        -4.69
# 8 614565 2022-05-14 AS Roma                 20        12.5        -7.46
# 8 275872 2009-11-07 Getafe                  14        7.38        -6.62
# 1 342835 2012-11-17 Paris Saint-Germain     18        8.57        -9.43
# 9 513478 2019-05-04 AFC Bournemouth         10        5.33        -4.67




## Eintracht Frankfurt (0-2 at home vs VfB Stuttgart): red card for opponent on 15th min, went down 0-1 on 64th, 0-2 on 68th min
## AS Roma (1-1 home vs Venezia on 2022-05-14): went down 0-1 on 1st min, then opponent also got red card on 32nd min, tied game on 76th min
## Getafe (0-2 home vs Deportivo La Coruna on 2009-11-07): went down 0-1 on 13th min, opp got red card on 26th min, down 0-2 on 59th, opp got 2nd red card on 72nd
## Paris Saint-Germain (see above) ...
## AFC Bournemouth (1-0 home vs Tottenham on 2019-05-04): opp got red cards on min 43 and 48, 0-0 entire game but the goal at the end to go up 1-0;
##                                                        only 1 of their 10 corners happened before a red card, and 7 out of 10 happened when up 2 men



####
## LARGEST JUMP IN SHOTS:
####

## 582165 Barcelona on 2020-10-01 away vs Celta Vigo (8 -> 18.5, a 9.5 corner addition)
##  
## Up 1-0 on 11th min
## Red card on 42nd min
## Up 2-0 on 51st min
## Up 3-0 on 95th min

## 7 of 8 corners happened when down a man, 6 of 8 when up at least 2 goals


### GAMES WITH LARGEST JUMPS IN EACH LEAGUE, 2008-2023:

# 1 346106 2013-05-18 VfL Wolfsburg        9       15.2         6.15
# 1 351984 2012-11-04 AS Roma             15       24.7         9.65
# 1 582165 2020-10-01 Barcelona            8       18.5         10.5 
# 2 639931 2023-04-30 Nice                14       20.8         6.84
# 1 345674 2012-12-29 Manchester City      9       15.7         6.69


## VfL Wolfsburg (2-2 away vs Eintracht on 2013-05-18) went up 1-0 on 8th min, 2-0 on 19th min, got red card on 35th; opp scored instantly after on 36th min, but then it held at 1-2 till 90th min
##    1 corner when tied early; 1 when up 2-0; remaining 7 corners when down 1 man and 1 goal

## AS Roma (4-1 at home vs Palermo on 2012-11-04; LARGE BASE of 15 corners) - up 1-0 on 11th min, 2-0 on 31st, 3-0 on 69th, 4-0 on 79th, opp scored on 84th
##    13 of 15 corners when Roma up at least 2 goals; 2 of those corners when down a man and up 2+ goals;

## Barcelona  (3-0 on the road vs Celta Vigo) - see above

## Nice (1-0 on the road vs Troyes ... on 2023-04-30; LARGE BASE of 14 corners) - scored on 2nd min (1-0), held on; nothing else; all 14 corners when up 1-0

## Manchester City (4-3 on the road vs Norwich.. on 2012-12-29) - Up 2-0 by 4th min; 2 corners after; opp scores 2-1; 3 more corners; red card for City; 1 corner; City up 3-1; 2 corners; opp scores 3-2; cit scores 4-2; opp scores 3-3;
##  All corners when ahead in score; a chunk when up 2+ goals; and half of them when down a man


View(our.df %>% 
       # filter(gameId == 609246, Team == "Bordeaux")
       # filter(gameId == 576779, Team == "Bayern Munich")
       #  filter(gameId == 278257 , Team == "AS Roma")
       # filter(gameId == 301979 , Team == "Real Madrid")
       # filter(gameId == 424698 , Team == "Caen")
)


View(check.df %>% 
       #filter(gameId == 609246, Team == "Bordeaux")
       #filter(gameId == 293077, Team == "West Bromwich Albion")
       filter(gameId == 424698 , Team == "Caen")
)
