######
######

library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)



league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

load(file="gam_ziP_obj_Shots.Robj")

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
  
  
  glm.extreme.obj[[league]] <- glm.nb(Shots ~ Score.Diff + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
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
    mutate(Shots.Adj = Shots*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway)
  
  
  ### 
  
  cat("\n")
  print("Top-10 single game shifts:")
  final.df <- our.df %>%
    group_by(gameId, gamedDate, Team) %>%
    summarise(Shots = sum(Shots),
              Shots.Adj = sum(Shots.Adj),
              Shots.Diff = Shots.Adj - Shots,) %>%
    arrange(desc(abs(Shots.Diff)))

 #  print(head(final.df, 10))
  
  biggest.jump <- final.df %>% filter(Shots.Diff > 0) %>% dplyr::select(gameId, Team) %>% head(.,1)
  biggest.fall <- final.df %>% filter(Shots.Diff < 0) %>% dplyr::select(gameId, Team) %>% head(.,1)
  
  cat("\n")
  print("Biggest jump and fall, breakdown:")
  biggest.jump.fall <- our.df %>%
    filter(gameId %in% c(biggest.jump$gameId, biggest.fall$gameId),
           Team %in% c(biggest.jump$Team, biggest.fall$Team)) %>%
    group_by(gameId, Team, gamedDate, HomeAway, Score.Diff, RedCard.Diff) %>%
    summarise(Shots = sum(Shots),
              Shots.Adj = sum(Shots.Adj),
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
    summarise(Shots = sum(Shots), 
              # Shots.Adj = sum(Shots.Adj),
              minutes.spent = sum(minutes.spent)) %>%
    pivot_wider(names_from = c(Score.Diff),
                values_from = c(Shots, minutes.spent)) %>%
    replace(is.na(.), 0 ) %>%
    mutate(Shots.Total.Actual = Shots_0 + Shots_Trail + Shots_Lead,
           #Shots.Total.Adj = sum(Shots.Adj),
           Shots.Min.Lead = paste0(Shots_Lead, " (", minutes.spent_Lead, ")"),
           Shots.Min.Trail = paste0(Shots_Trail, " (", minutes.spent_Trail, ")")) %>%
    dplyr::select(Shots.Total.Actual,
                  #Shots.Total.Adj,
                  Shots.Min.Lead,# Minutes.Lead,
                  Shots.Min.Trail #, Minutes.Trail
    )
  
  
  
  redcard.diffs <- biggest.jump.fall %>% 
         dplyr::select(-Score.Diff) %>%
         group_by(gameId, Team, gamedDate, RedCard.Diff) %>%
         summarise(Shots = sum(Shots), 
                  # Shots.Adj = sum(Shots.Adj),
                   minutes.spent = sum(minutes.spent)) %>%
         pivot_wider(names_from = c(RedCard.Diff),
                     values_from = c(Shots, minutes.spent)) %>%
         replace(is.na(.), 0 ) %>%
         mutate(Shots.Total.Actual = Shots_0 + Shots_UpMen + Shots_DownMen,
                #Shots.Total.Adj = sum(Shots.Adj),
                Shots.Min.UpMen = paste0(Shots_UpMen, " (", minutes.spent_UpMen, ")"),
                Shots.Min.DownMen = paste0(Shots_DownMen, " (", minutes.spent_DownMen, ")")) %>%
         dplyr::select(Shots.Total.Actual,
                       #Shots.Total.Adj,
                       Shots.Min.UpMen,# Minutes.Lead,
                       Shots.Min.DownMen #, Minutes.Trail
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
  #         summarise(Shots = sum(Shots), 
  #                   minutes.spent = sum(minutes.spent)) %>%
  #         pivot_wider(names_from =c(RedCard.Diff),
  #                     values_from = c(Shots, minutes.spent))))
  
  
  cat("\n")
  cat("\n")
}


### Shots when:
## Team Opponent Season Shots Shots.Adj  Down 2+ goals; Down 1 goal; Up 1 goal; Up 2+ goals;  Down 2 men; Down 1 man; Up 1 man; Up 2 men





# [1] "Bundesliga"
# 
# 
# gameId gamedDate  Team                Shots Shots.Adj Shots.Diff
# <int> <chr>      <chr>               <int>     <dbl>      <dbl>
# 1 297218 2011-02-27 Eintracht Frankfurt    30      20.0      -9.99
# 2 576779 2021-03-20 Bayern Munich          15      24.5       9.46
# 3 273207 2010-04-17 Bayer Leverkusen       21      30.3       9.26
# 4 346241 2013-01-26 VfL Wolfsburg          26      16.8      -9.20
# 5 548098 2020-06-06 Fortuna Dsseldorf      25      16.4      -8.65
# 6 487112 2017-12-17 RB Leipzig             21      12.6      -8.44
# 7 426918 2016-02-28 Borussia Dortmund      26      17.7      -8.32
# 8 576794 2021-04-10 Hertha Berlin          23      15.3      -7.72
# 9 369895 2013-08-10 VfL Wolfsburg          13      20.6       7.62
# 10 320996 2011-09-17 Nurnberg               22      14.4      -7.58
# 
# 
# [1] "SerieA"
# 
# 
# gameId gamedDate  Team       Shots Shots.Adj Shots.Diff
# <int> <chr>      <chr>      <int>     <dbl>      <dbl>
#   1 614565 2022-05-14 AS Roma       43      26.6     -16.4 
# 2 554298 2020-07-01 AC Milan      39      23.6     -15.4 
# 3 251416 2009-02-15 Fiorentina    34      19.3     -14.7 
# 4 583014 2021-01-03 Benevento     25      12.6     -12.4 
# 5 278257 2010-01-09 AS Roma       17      28.3      11.3 
# 6 582930 2021-02-20 Sassuolo      30      19.2     -10.8 
# 7 376941 2014-05-11 Bologna       29      18.3     -10.7 
# 8 644854 2023-02-25 Napoli        18      28.2      10.2 
# 9 377165 2013-12-08 Cagliari      30      19.9     -10.1 
# 10 554487 2019-10-30 Juventus      27      17.0      -9.97
# 
# 
# [1] "LaLiga"
# 
# 
# gameId gamedDate  Team           Shots Shots.Adj Shots.Diff
# <int> <chr>      <chr>          <int>     <dbl>      <dbl>
# 1 301979 2011-02-13 Real Madrid       18      33.4      15.4 
# 2 324018 2011-08-28 Real Madrid       40      52.3      12.3 
# 3 550610 2019-08-17 Real Madrid       17      28.2      11.2 
# 4 275872 2009-11-07 Getafe            22      10.8     -11.2 
# 5 275886 2009-10-31 Real Madrid       23      34.1      11.1 
# 6 373084 2013-11-23 Real Madrid       35      45.8      10.8 
# 7 253063 2009-01-11 Real Betis        21      31.3      10.3 
# 8 348350 2013-01-06 Real Madrid       24      33.7       9.67
# 9 348407 2012-11-10 Rayo Vallecano    28      18.4      -9.64
# 10 323761 2012-03-24 Barcelona         18      27.6       9.56
# 
# 
# [1] "Ligue1"
# 
# gameId gamedDate  Team                  Shots Shots.Adj Shots.Diff
# <int> <chr>      <chr>                 <int>     <dbl>      <dbl>
#   1 609246 2022-03-20 Bordeaux                 31      11.2     -19.8 
# 2 271426 2009-10-31 Marseille                35      21.0     -14.0 
# 3 342665 2013-03-30 Evian Thonon Gaillard    29      17.1     -11.9 
# 4 342835 2012-11-17 Paris Saint-Germain      22      10.3     -11.7 
# 5 609334 2022-01-09 Brest                    26      15.6     -10.4 
# 6 271589 2010-05-08 Lille                    28      17.8     -10.2 
# 7 542015 2020-02-01 Nimes                    23      12.9     -10.1 
# 8 424698 2015-09-12 Caen                     15      25.1      10.1 
# 9 342759 2013-01-11 Paris Saint-Germain      26      35.8       9.81
# 10 512314 2018-09-26 Dijon FCO                30      20.2      -9.80
# 
# 
# [1] "PremierLeague"
# 
# gameId gamedDate  Team                 Shots Shots.Adj Shots.Diff
# <int> <chr>      <chr>                <int>     <dbl>      <dbl>
# 1 293077 2010-11-01 West Bromwich Albion    14      30.1       16.1
# 2 450630 2017-05-21 Arsenal                 17      31.5       14.5
# 3 345773 2012-10-20 Manchester City         24      38.4       14.4
# 4 293077 2010-11-01 Blackpool               26      11.9      -14.1
# 5 395498 2015-03-21 Manchester City         42      29.7      -12.3
# 6 367396 2013-11-02 Hull City               25      13.2      -11.8
# 7 367323 2013-12-26 Newcastle United        25      13.6      -11.4
# 8 345674 2012-12-29 Manchester City         15      26.1       11.1
# 9 318258 2012-04-10 Liverpool               16      27.0       11.0
# 10 480613 2018-03-10 Huddersfield Town       30      19.2      -10.8




####
# LARGEST DROP IN SHOTS:
####

##    609246 Bordeaux home (31 -> 11.2, nearly a 20 shot drop), vs Montpellier
##    0-2 loss, with opponent getting 2 red cards

#
#  Down 0-1 on min 12
#  Down 0-2 on min 17
#  Opponent gets red card on min 39
#  Opponent gets 2nd red card on min 45

# They didn't make a single shot until minute 45 lol...
# Minutes:  [1] 45 45 45 45 46 50 52 55 60 62 67 69 70 74 76 77 80 81 82 83 84 86 88 89 90 90


### LARGEST DROPS IN EACH LEAGUE:

# 1 297218 2011-02-27 Eintracht Frankfurt     30      20.0      -9.99
# 1 614565 2022-05-14 AS Roma                 43      26.6      -16.4
# 4 275872 2009-11-07 Getafe                  22      10.8      -11.2
# 1 609246 2022-03-20 Bordeaux                31      11.2      -19.8
# 4 293077 2010-11-01 Blackpool               26      11.9      -14.1


## Eintracht Frankfurt (0-2 at home vs VfB Stuttgart): red card for opponent on 15th min, went down 0-1 on 64th, 0-2 on 68th min
## AS Roma (1-1 home vs Venezia on 2022-05-14): went down 0-1 on 1st min, then opponent also got red card on 32nd min, tied game on 76th min
## Getafe (0-2 home vs Deportivo La Coruna on 2009-11-07): went down 0-1 on 13th min, opp got red card on 26th min, down 0-2 on 59th, opp got 2nd red card on 72nd
## Bordeaux (see above) ...
## Blackpool (2-1 home vs West Brom on 2010-11-01): opp got red card on 11th min, went up 1-0 on 12th, opp got 2nd red on 29th min, went up 2-0 on 62nd, opp scored on 85th to make it 2-1


####
## LARGEST JUMP IN SHOTS:
####

## 293077 2010-11-01 West Bromwich Albion (14 -> 30.1, a 16 shot addition), vs Blackpool)
##  
## Red card on 11th min
## Down 0-1 on 13th
## 2nd red card on 29th min
## Down 0-2 on 63rd
## Scored to make it 1-2 on 85th


### GAMES WITH LARGEST JUMPS IN EACH LEAGUE, 2008-2023:

# 2 576779 2021-03-20 Bayern Munich           15      24.5       9.46
# 5 278257 2010-01-09 AS Roma                 17      28.3      11.3
# 1 301979 2011-02-13 Real Madrid             18      33.4      15.4 
# 8 424698 2015-09-12 Caen                    15      25.1      10.1
# 1 293077 2010-11-01 West Bromwich Albion    14      30.1       16.1


## Bayern (4-0 at home vs VfB Stuttgart on 2021-03-20) got a red card on 11th min, still went on to quickly score 2 goals, and win 4-0 overall
## AS Roma (1-0 at home vs Chievo Verona on 2010-01-09) scored on 2nd minute, got a red card on 11th, and held on to 1-0 throughout the rest (80 minutes)
## Real Madrid (1-0 on the road vs Espanyol on 2011-02-13) got red card on 3rd minute, scored on 25th, and held on to 1-0 through the rest (66 minutes)
## Caen (3-1 on the road vs Troyes ... on 2015-09-12) scored on 22nd (1-0), scored on 38th (2-0), red card on 57th, scored 3rd on 64th, allowed a goal on 93rd (3-1)
## West Brom (1-2 on the road vs Blackpool.. on 2010-11-01)

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
