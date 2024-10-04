######
######

library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

## If we use models where "Minute.clean" was included
include.minute <- TRUE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

# load(file="gam_ziP_obj_Shots.Robj")

  load(file=paste0(ifelse(include.minute,
                          "",
                          "NO_MINUTES_TEST_"),
    "gam_ziP_obj",  
                   ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
                   "_Shots.Robj"))


for (league in league.name){
  
  print(league)
  
  cat("\n")
  
  #######
  ### LOADING, SETTING UP DATA
  #######
  
#  our.df <- read.csv(paste0(league, "_NO_EXTRA_TIME_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  ## To be used for prediction (including everything, so that totals correspond to full data)
  our.df.pred <- read.csv(paste0(league, "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  # ## REMOVING NA betting data
  # our.df <- our.df %>% filter(!is.na(Weighted.Win.Prob))
  # dim(our.df)
  # 
  # ## For EXTRA TIME: JUST FORCE IT TO BE EQUAL TO THE LAST MINUTE (e.g. for extra time in 1st half - make it =45, in 2nd half - =90)
  # 
  # # our.df$Minute.clean[our.df$half_id == 1 & our.df$Minute.clean > 45] <- 45
  # # our.df$Minute.clean[our.df$Minute.clean > 90] <- 90
  # 
  # 
  # our.df  <- our.df  %>%
  #   mutate(abs.Score.Diff = abs(Score.Diff),
  #          abs.RedCard.Diff = abs(RedCard.Diff))
  # 
  # our.df  <- our.df  %>%
  #   mutate(Period.ID = group_indices(our.df, .dots=c("ID", "Minute.clean", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
  #   arrange(Period.ID) %>%
  #   mutate(Period.ID = factor(Period.ID))
  # 
  # our.df <- our.df %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
  
  
  #######
  ### GETTING ADJUSTMENTS
  #######

  
  
if (include.minute == TRUE){
### 1. SCORE DIFFERENTIAL
  
# Getting the DIFFERENTIALS (log-scale "linear" effects)
all.score.diffs <- c(min(our.df.pred$Score.Diff):max(our.df.pred$Score.Diff))
log.score.diff.effects <- as.numeric(predict(gam.ziP.obj[[league]],
                                  newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                                       Score.Diff = all.score.diffs))) -
  as.numeric(predict(gam.ziP.obj[[league]],
                     newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
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
log.redcard.diff.effects <- as.numeric(predict(gam.ziP.obj[[league]],
                                             newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                                                  RedCard.Diff = all.redcard.diffs))) -
  as.numeric(predict(gam.ziP.obj[[league]],
                     newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
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
log.homeaway.effects <- as.numeric(predict(gam.ziP.obj[[league]],
                                             newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                                                  HomeAway = all.homeaway))) -
  as.numeric(predict(gam.ziP.obj[[league]],
                     newdata = data.frame(Minute.clean=45, Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                          HomeAway = rep("Home", length(all.homeaway)))))

names(log.homeaway.effects) <- all.homeaway
log.homeaway.effects

# Multiplicative effects
exp(log.homeaway.effects)

# Doing the multiplication
our.df.pred$Adj.Coef.HomeAway <- 
  exp(-log.homeaway.effects)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]


### 4. Minute

# Getting the DIFFERENTIALS (log-scale "linear" effects)
all.minutes <- c(min(our.df.pred$Minute.clean):max(our.df.pred$Minute.clean))
log.minute.effects <- as.numeric(predict(gam.ziP.obj[[league]],
                                             newdata = data.frame(Score.Diff = 0, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                                                  Minute.clean = all.minutes))) -
  as.numeric(predict(gam.ziP.obj[[league]],
                     newdata = data.frame(Score.Diff = 0, Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                          Minute.clean = rep(45, length(all.minutes)))))

names(log.minute.effects) <- all.minutes
log.minute.effects


# Multiplicative effects
exp(log.minute.effects)

# Doing the multiplication
our.df.pred$Adj.Coef.Minute <- 
  exp(-log.minute.effects)[sapply(our.df.pred$Minute.clean, function(x) which(names(log.minute.effects) == x))]


our.df.pred <- our.df.pred %>%
  mutate(Shots.Adj = Shots*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway*Adj.Coef.Minute,
         Shots.Adj.No.Min = Shots*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway)

} else {
  
  ### 1. SCORE DIFFERENTIAL
  
  # Getting the DIFFERENTIALS (log-scale "linear" effects)
  all.score.diffs <- c(min(our.df.pred$Score.Diff):max(our.df.pred$Score.Diff))
  log.score.diff.effects <- as.numeric(predict(TEST_gam.ziP.obj[[league]],
                                               newdata = data.frame(Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
                                                                    Score.Diff = all.score.diffs))) -
    as.numeric(predict(TEST_gam.ziP.obj[[league]],
                       newdata = data.frame(Weighted.Win.Prob = 0, HomeAway = "Home", RedCard.Diff =0,
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
  log.redcard.diff.effects <- as.numeric(predict(TEST_gam.ziP.obj[[league]],
                                                 newdata = data.frame(Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
                                                                      RedCard.Diff = all.redcard.diffs))) -
    as.numeric(predict(TEST_gam.ziP.obj[[league]],
                       newdata = data.frame(Weighted.Win.Prob = 0, HomeAway = "Home", Score.Diff =0,
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
  log.homeaway.effects <- as.numeric(predict(TEST_gam.ziP.obj[[league]],
                                             newdata = data.frame(Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                                                  HomeAway = all.homeaway))) -
    as.numeric(predict(TEST_gam.ziP.obj[[league]],
                       newdata = data.frame( Weighted.Win.Prob = 0, Score.Diff = 0, RedCard.Diff =0,
                                            HomeAway = rep("Home", length(all.homeaway)))))
  
  names(log.homeaway.effects) <- all.homeaway
  log.homeaway.effects
  
  # Multiplicative effects
  exp(log.homeaway.effects)
  
  # Doing the multiplication
  our.df.pred$Adj.Coef.HomeAway <- 
    exp(-log.homeaway.effects)[sapply(our.df.pred$HomeAway, function(x) which(names(log.homeaway.effects) == x))]

  
  our.df.pred <- our.df.pred %>%
    mutate(Shots.Adj = Shots*Adj.Coef.ScoreDiff*Adj.Coef.RedCardDiff*Adj.Coef.HomeAway,
           )
  
  
}


### 
cat("\n")
print(paste0("Top-10 single game shifts, ", ifelse(include.minute, "WITH MINUTE:", "NO MINUTE:")))

final.df <- our.df.pred %>% 
  group_by(gameId, Team) %>% 
  summarise(Shots = sum(Shots),
            Shots.Adj= sum(Shots.Adj),
            Shots.Diff = Shots.Adj - Shots,
            Shots.Adj.No.Min = sum(Shots.Adj.No.Min),
            Shots.Diff.No.Min = Shots.Adj.No.Min - Shots) %>% 
  arrange(desc(abs(Shots.Diff.No.Min)))

print(head(final.df, 20))

# our.df %>%
#   filter(gameId == head(final.df$gameId, 1), Team == head(final.df$Team, 1))




cat("\n")
cat("\n")
}



############
############
###  SHOTS
############
############


  ########
  #### MINUTES INCLUDED
  ########
  
  # [1] "Bundesliga"
  # 
  # 
  # [1] "Top-10 single game shifts, WITH MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team                Shots Shots.Adj Shots.Diff
  # <int> <chr>               <int>     <dbl>      <dbl>
  #   1 297218 Eintracht Frankfurt    30      25.3      -4.66
  # 2 576779 Bayern Munich          15      19.5       4.49
  # 3 273207 Bayer Leverkusen       21      25.3       4.30
  # 4 426918 Borussia Dortmund      26      21.8      -4.23
  # 5 346241 VfL Wolfsburg          26      21.9      -4.15
  # 6 548098 Fortuna Dsseldorf      25      21.0      -4.05
  # 7 369895 VfL Wolfsburg          13      17.0       4.01
  # 8 517592 TSG Hoffenheim         19      22.9       3.93
  # 9 320996 Nurnberg               22      18.2      -3.83
  # 10 487235 Borussia Dortmund      27      23.3      -3.69
  # 
  # 
  # [1] "SerieA"
  # 
  # 
  # [1] "Top-10 single game shifts, WITH MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team       Shots Shots.Adj Shots.Diff
  # <int> <chr>      <int>     <dbl>      <dbl>
  #   1 614565 AS Roma       43      34.7      -8.33
  # 2 554298 AC Milan      39      31.8      -7.19
  # 3 251416 Fiorentina    34      27.0      -6.96
  # 4 583014 Benevento     25      18.9      -6.09
  # 5 278257 AS Roma       17      22.4       5.37
  # 6 582930 Sassuolo      30      24.7      -5.33
  # 7 376941 Bologna       29      23.8      -5.24
  # 8 377165 Cagliari      30      25.1      -4.93
  # 9 583050 Benevento     29      24.1      -4.90
  # 10 403488 Palermo       24      19.1      -4.88
  # 
  # 
  # [1] "LaLiga"
  # 
  # 
  # [1] "Top-10 single game shifts, WITH MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team           Shots Shots.Adj Shots.Diff
  # <int> <chr>          <int>     <dbl>      <dbl>
  #   1 301979 Real Madrid       18      24.2       6.21
  # 2 275872 Getafe            22      16.4      -5.65
  # 3 373084 Real Madrid       35      40.6       5.55
  # 4 324018 Real Madrid       40      45.2       5.24
  # 5 348407 Rayo Vallecano    28      23.2      -4.82
  # 6 610441 Cdiz              27      22.4      -4.61
  # 7 275886 Real Madrid       23      27.6       4.58
  # 8 348163 Rayo Vallecano    27      22.5      -4.49
  # 9 433839 Real Betis        28      23.5      -4.48
  # 10 610675 Valencia          20      15.6      -4.44
  # 
  # 
  # [1] "Ligue1"
  # 
  # 
  # [1] "Top-10 single game shifts, WITH MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team                  Shots Shots.Adj Shots.Diff
  # <int> <chr>                 <int>     <dbl>      <dbl>
  #   1 609246 Bordeaux                 31      20.1     -10.9 
  # 2 271426 Marseille                35      26.8      -8.19
  # 3 342835 Paris Saint-Germain      22      15.5      -6.47
  # 4 342665 Evian Thonon Gaillard    29      22.5      -6.46
  # 5 271589 Lille                    28      21.9      -6.08
  # 6 609334 Brest                    26      20.2      -5.81
  # 7 512314 Dijon FCO                30      24.4      -5.62
  # 8 512350 Guingamp                 22      17.0      -5.03
  # 9 320373 Lorient                  23      18.1      -4.93
  # 10 295037 Brest                    24      19.1      -4.90
  # 
  # 
  # [1] "PremierLeague"
  # 
  # 
  # [1] "Top-10 single game shifts, WITH MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team              Shots Shots.Adj Shots.Diff
  # <int> <chr>             <int>     <dbl>      <dbl>
  #   1 293077 Blackpool            26      18.8      -7.18
  # 2 367396 Hull City            25      18.8      -6.18
  # 3 450630 Arsenal              17      22.6       5.64
  # 4 513478 AFC Bournemouth      20      14.6      -5.39
  # 5 367323 Newcastle United     25      19.7      -5.28
  # 6 541476 Burnley              23      17.7      -5.26
  # 7 480613 Huddersfield Town    30      24.9      -5.13
  # 8 293054 Manchester City      33      27.9      -5.06
  # 9 513627 Leicester City       23      18.0      -5.00
  # 10 345773 Manchester City      24      28.8       4.85


  
  ########
  #### MINUTES EXCLUDED
  ########


  # [1] "Bundesliga"
  # 
  # 
  # [1] "Top-10 single game shifts, NO MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team                Shots Shots.Adj Shots.Diff
  # <int> <chr>               <int>     <dbl>      <dbl>
  #   1 297218 Eintracht Frankfurt    30      24.5      -5.55
  # 2 487112 RB Leipzig             21      16.1      -4.91
  # 3 273207 Bayer Leverkusen       21      25.8       4.84
  # 4 346241 VfL Wolfsburg          26      21.2      -4.83
  # 5 576779 Bayern Munich          15      19.6       4.64
  # 6 548098 Fortuna Dsseldorf      25      20.6      -4.42
  # 7 426918 Borussia Dortmund      26      21.7      -4.28
  # 8 369895 VfL Wolfsburg          13      16.9       3.95
  # 9 576794 Hertha Berlin          23      19.1      -3.92
  # 10 320996 Nurnberg               22      18.1      -3.92
  # 
  # 
  # [1] "SerieA"
  # 
  # 
  # [1] "Top-10 single game shifts, NO MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team          Shots Shots.Adj Shots.Diff
  # <int> <chr>         <int>     <dbl>      <dbl>
  #   1 614565 AS Roma          43      34.4      -8.63
  # 2 554298 AC Milan         39      30.7      -8.32
  # 3 251416 Fiorentina       34      26.0      -8.00
  # 4 583014 Benevento        25      18.2      -6.77
  # 5 376941 Bologna          29      23.3      -5.72
  # 6 582930 Sassuolo         30      24.4      -5.59
  # 7 304692 Chievo Verona    20      14.6      -5.40
  # 8 403488 Palermo          24      18.7      -5.31
  # 9 377165 Cagliari         30      24.7      -5.26
  # 10 614820 AS Roma          24      18.8      -5.22
  # 
  # 
  # [1] "LaLiga"
  # 
  # 
  # [1] "Top-10 single game shifts, NO MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team           Shots Shots.Adj Shots.Diff
  # <int> <chr>          <int>     <dbl>      <dbl>
  #   1 275872 Getafe            22      16.2      -5.85
  # 2 301979 Real Madrid       18      23.7       5.68
  # 3 433824 Real Madrid       30      24.7      -5.30
  # 4 610441 Cdiz              27      21.9      -5.06
  # 5 348407 Rayo Vallecano    28      23.0      -5.01
  # 6 324018 Real Madrid       40      45.0       4.96
  # 7 490631 Las Palmas        24      19.2      -4.84
  # 8 610675 Valencia          20      15.2      -4.77
  # 9 302002 Real Zaragoza     33      28.4      -4.63
  # 10 348163 Rayo Vallecano    27      22.5      -4.48
  # 
  # 
  # [1] "Ligue1"
  # 
  # 
  # [1] "Top-10 single game shifts, NO MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team                  Shots Shots.Adj Shots.Diff
  # <int> <chr>                 <int>     <dbl>      <dbl>
  #   1 609246 Bordeaux                 31      18.0     -13.0 
  # 2 271426 Marseille                35      26.3      -8.71
  # 3 342665 Evian Thonon Gaillard    29      21.4      -7.59
  # 4 342835 Paris Saint-Germain      22      14.6      -7.35
  # 5 512314 Dijon FCO                30      23.3      -6.72
  # 6 609334 Brest                    26      19.3      -6.71
  # 7 271589 Lille                    28      21.6      -6.36
  # 8 512350 Guingamp                 22      16.0      -6.05
  # 9 542015 Nimes                    23      17.1      -5.89
  # 10 342759 Paris Saint-Germain      26      31.9       5.86
  # 
  # 
  # [1] "PremierLeague"
  # 
  # 
  # [1] "Top-10 single game shifts, NO MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team              Shots Shots.Adj Shots.Diff
  # <int> <chr>             <int>     <dbl>      <dbl>
  #   1 293077 Blackpool            26      18.6      -7.45
  # 2 367396 Hull City            25      18.7      -6.33
  # 3 367323 Newcastle United     25      19.0      -6.04
  # 4 395498 Manchester City      42      36.0      -5.96
  # 5 541476 Burnley              23      17.7      -5.33
  # 6 293054 Manchester City      33      27.7      -5.25
  # 7 513478 AFC Bournemouth      20      14.8      -5.23
  # 8 293126 Aston Villa          28      22.8      -5.17
  # 9 480613 Huddersfield Town    30      24.8      -5.16
  # 10 345773 Manchester City      24      29.1       5.13
  
  


############
############
###  CORNERS
############
############


  #######
  ### WITH MINUTE INCLUDED
  #######
  
  
  # [1] "Bundesliga"
  # 
  # 
  # [1] "Top-10 single game shifts, WITH MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team                Corners Corners.Adj Corners.Diff
  # <int> <chr>                 <int>       <dbl>        <dbl>
  # 1 346106 VfL Wolfsburg             9       12.7          3.75
  # 2 548077 Bayern Munich            10       13.3          3.26
  # 3 252572 TSG Hoffenheim           10       12.9          2.91
  # 4 576575 Bayern Munich            10       12.9          2.90
  # 5 297218 Eintracht Frankfurt      13       10.2         -2.81
  # 6 369814 Hannover 96               8       10.8          2.80
  # 7 320996 Nurnberg                 13       10.3         -2.71
  # 8 608577 Bayer Leverkusen          6        8.61         2.61
  # 9 548261 TSG Hoffenheim           14       11.5         -2.48
  # 10 252536 Bayern Munich            14       16.5          2.46
  # 
  # 
  # [1] "SerieA"
  # 
  # 
  # [1] "Top-10 single game shifts, WITH MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team           Corners Corners.Adj Corners.Diff
  # <int> <chr>            <int>       <dbl>        <dbl>
  #   1 614565 AS Roma             20       16.1         -3.94
  # 2 351984 AS Roma             15       18.6          3.61
  # 3 491540 Internazionale      18       21.6          3.56
  # 4 554298 AC Milan            17       13.4         -3.55
  # 5 644854 Napoli              13       16.5          3.53
  # 6 461216 Napoli               7       10.0          3.05
  # 7 432131 Genoa                9       12.0          2.96
  # 8 583014 Benevento           12        9.06        -2.94
  # 9 582962 Genoa               11       13.9          2.87
  # 10 461277 Juventus            14       16.8          2.84
  # 
  # 
  # [1] "LaLiga"
  # 
  # 
  # [1] "Top-10 single game shifts, WITH MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team            Corners Corners.Adj Corners.Diff
  # <int> <chr>             <int>       <dbl>        <dbl>
  #   1 582165 Barcelona             8        15.2         7.19
  # 2 301979 Real Madrid           9        14.8         5.75
  # 3 433669 Rayo Vallecano        9        14.6         5.58
  # 4 402554 Mlaga                13        18.4         5.45
  # 5 301957 Athletic Club         9        14.0         4.96
  # 6 252805 Real Valladolid       9        13.9         4.94
  # 7 373169 Barcelona            14        18.9         4.87
  # 8 372983 Real Madrid          10        14.7         4.72
  # 9 372953 Mlaga                10        14.7         4.66
  # 10 582118 Real Sociedad        15        19.5         4.50
  # 
  # 
  # [1] "Ligue1"
  # 
  # 
  # [1] "Top-10 single game shifts, WITH MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team                  Corners Corners.Adj Corners.Diff
  # <int> <chr>                   <int>       <dbl>        <dbl>
  #   1 342835 Paris Saint-Germain        18       11.8         -6.23
  # 2 609246 Bordeaux                   11        6.63        -4.37
  # 3 271589 Lille                      17       12.7         -4.27
  # 4 342665 Evian Thonon Gaillard      14       10.2         -3.78
  # 5 609334 Brest                      14       10.2         -3.77
  # 6 609326 Marseille                  15       11.3         -3.71
  # 7 573218 Lyon                       19       15.6         -3.41
  # 8 482047 Marseille                  11       14.4          3.38
  # 9 369498 Valenciennes               22       18.7         -3.33
  # 10 542140 Paris Saint-Germain        13       16.3          3.29
  # 
  # 
  # [1] "PremierLeague"
  # 
  # 
  # [1] "Top-10 single game shifts, WITH MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team              Corners Corners.Adj Corners.Diff
  # <int> <chr>               <int>       <dbl>        <dbl>
  #   1 345674 Manchester City         9       11.9          2.91
  # 2 513478 AFC Bournemouth        10        7.43        -2.57
  # 3 578369 Chelsea                10       12.4          2.39
  # 4 367271 Swansea City           11        8.71        -2.29
  # 5 480591 Manchester City        11       13.2          2.22
  # 6 513513 Manchester City        17       19.2          2.21
  # 7 605897 Manchester City        11       13.2          2.17
  # 8 422633 Stoke City              6        8.16         2.16
  # 9 480613 Huddersfield Town      12        9.84        -2.16
  # 10 292944 Aston Villa            16       18.1          2.15

  
  
  
  
  #######
  ### WITH MINUTE EXCLUDED
  #######
  
  # [1] "Bundesliga"
  # 
  # 
  # [1] "Top-10 single game shifts, NO MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team                Corners Corners.Adj Corners.Diff
  # <int> <chr>                 <int>       <dbl>        <dbl>
  #   1 346106 VfL Wolfsburg             9       13.6          4.56
  # 2 548077 Bayern Munich            10       13.4          3.38
  # 3 252572 TSG Hoffenheim           10       13.3          3.34
  # 4 608577 Bayer Leverkusen          6        9.31         3.31
  # 5 576575 Bayern Munich            10       13.3          3.29
  # 6 252536 Bayern Munich            14       17.3          3.26
  # 7 320892 Bayern Munich            10       13.2          3.18
  # 8 297218 Eintracht Frankfurt      13        9.87        -3.13
  # 9 369814 Hannover 96               8       10.9          2.94
  # 10 517758 Bayern Munich            14       16.9          2.91
  # 
  # 
  # [1] "SerieA"
  # 
  # 
  # [1] "Top-10 single game shifts, NO MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team           Corners Corners.Adj Corners.Diff
  # <int> <chr>            <int>       <dbl>        <dbl>
  #   1 614565 AS Roma             20       15.9         -4.15
  # 2 554298 AC Milan            17       13.0         -4.02
  # 3 491540 Internazionale      18       21.9          3.94
  # 4 351984 AS Roma             15       18.9          3.90
  # 5 644854 Napoli              13       16.7          3.67
  # 6 461216 Napoli               7       10.5          3.51
  # 7 582962 Genoa               11       14.5          3.47
  # 8 432131 Genoa                9       12.4          3.42
  # 9 461277 Juventus            14       17.2          3.19
  # 10 583014 Benevento           12        8.84        -3.16
  # 
  # 
  # [1] "LaLiga"
  # 
  # 
  # [1] "Top-10 single game shifts, NO MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team            Corners Corners.Adj Corners.Diff
  # <int> <chr>             <int>       <dbl>        <dbl>
  #   1 582165 Barcelona             8       15.1          7.07
  # 2 301979 Real Madrid           9       14.5          5.54
  # 3 433669 Rayo Vallecano        9       14.4          5.36
  # 4 402554 Mlaga                13       17.8          4.77
  # 5 252805 Real Valladolid       9       13.7          4.66
  # 6 373169 Barcelona            14       18.6          4.56
  # 7 275872 Getafe               14        9.53        -4.47
  # 8 301957 Athletic Club         9       13.3          4.28
  # 9 372983 Real Madrid          10       14.3          4.26
  # 10 372953 Mlaga                10       14.2          4.23
  # 
  # 
  # [1] "Ligue1"
  # 
  # 
  # [1] "Top-10 single game shifts, NO MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team                  Corners Corners.Adj Corners.Diff
  # <int> <chr>                   <int>       <dbl>        <dbl>
  #   1 342835 Paris Saint-Germain        18       11.5         -6.49
  # 2 609246 Bordeaux                   11        6.22        -4.78
  # 3 271589 Lille                      17       12.7         -4.35
  # 4 482047 Marseille                  11       15.3          4.31
  # 5 342665 Evian Thonon Gaillard      14        9.93        -4.07
  # 6 609334 Brest                      14        9.96        -4.04
  # 7 609326 Marseille                  15       11.2         -3.81
  # 8 482105 Paris Saint-Germain         7       10.7          3.74
  # 9 542219 Lille                      10       13.7          3.73
  # 10 639931 Nice                       14       17.7          3.66
  # 
  # 
  # [1] "PremierLeague"
  # 
  # 
  # [1] "Top-10 single game shifts, NO MINUTE:"
  # `summarise()` has grouped output by 'gameId'. You can override using the `.groups` argument.
  # # A tibble: 10 × 5
  # # Groups:   gameId [10]
  # gameId Team              Corners Corners.Adj Corners.Diff
  # <int> <chr>               <int>       <dbl>        <dbl>
  #   1 345674 Manchester City         9       11.9          2.91
  # 2 318142 Tottenham Hotspur      19       21.6          2.62
  # 3 513478 AFC Bournemouth        10        7.52        -2.48
  # 4 513513 Manchester City        17       19.5          2.46
  # 5 578369 Chelsea                10       12.4          2.41
  # 6 367271 Swansea City           11        8.63        -2.37
  # 7 345624 Sunderland             11        8.76        -2.24
  # 8 293122 Newcastle United       12        9.79        -2.21
  # 9 269930 Chelsea                15       17.2          2.17
  # 10 292991 Chelsea                14       16.2          2.16
  
  
  
  
  
  


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
