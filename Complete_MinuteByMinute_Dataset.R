#####
## MIGHT STILL BE MISSING THE EVENTS SINCE THE START OF THE HALF/GAME !!!
#####

library(mgcv)
library(tidyverse)
library(splines)

# load("final_df_w_red_cards.csv")

our.df <- read.csv(file="CLEANED_Bundesliga_All_Minutes_With_Score_Updated_WITH_BOOKING_ODDS.csv")
View(our.df)


our.df$ShotAttempts

our.df %>%
  filter(gameId == 252449, half_id == 2)


# !!!!
our.df <- our.df %>% full_join(our.df %>%
                       group_by(ID, gameId, half_id, HomeTeam, AwayTeam, gamedDate,
                                HomeBet, DrawBet, AwayBet, Weighted.Win.Prob.Home) %>%
                       summarise(Minute.clean = c(ifelse(half_id == 1, 1, 46):max(Minute.clean)))) %>%
                         group_by(ID, gameId) %>%
                         arrange(desc(half_id), desc(Minute.clean), .by_group=TRUE)
# !!!!

stat.columns <- c("Goal", "Foul", "Corner", "YellowCard", "RedCard", "ShotAttempts", "Saves")

our.df[, stat.columns] <- sapply(our.df[, stat.columns], function(x) ifelse(is.na(x), "Null", x))

dim(our.df)



########
## MAKING HOMESCORE and AWAYSCORE in PROPER ORDER, based on GOAL COLUMN ("HOME"/"AWAY")
########


# our.df$unique.id <- paste0(our.df$gameId, our.df$gamedDate, our.df$HomeTeam)

our.df$homeScore.old <- our.df$homeScore
our.df$homeScore <- unlist(tapply(our.df$Goal, our.df$ID, function(x){log.vec <- x == "Home";
if (sum(log.vec) > 0){
  apply(sapply(which(log.vec), function(z) c(rep(1, z), rep(0,length(log.vec) - z))),
        1,
        sum)} else {
          rep(0, length(log.vec))
        }} ))

our.df$awayScore.old <- our.df$awayScore
our.df$awayScore <- unlist(tapply(our.df$Goal, our.df$ID, function(x){log.vec <- x == "Away";
if (sum(log.vec) > 0){
  apply(sapply(which(log.vec), function(z) c(rep(1, z), rep(0, length(log.vec) - z))),
        1,
        sum)} else {
          rep(0, length(log.vec))
        }} ))


########
## MAKING A VARIABLE KEEPING TRACK OF RED-CARDED MEN, based on REDCARD COLUMN ("HOME"/"AWAY")
## => NEED THAT HERE to TAKE CARE OF "NA" FOR NON-EVENT ENTRIES
########


our.df$homeRedCardTot <- unlist(tapply(our.df$RedCard, our.df$ID, function(x){log.vec <- x == "Home";
if (sum(log.vec) > 0){
  apply(sapply(which(log.vec), function(z) c(rep(1, z), rep(0,length(log.vec) - z))),
        1,
        sum)} else {
          rep(0, length(log.vec))
        }} ))

our.df$awayRedCardTot <- unlist(tapply(our.df$RedCard, our.df$ID, function(x){log.vec <- x == "Away";
if (sum(log.vec) > 0){
  apply(sapply(which(log.vec), function(z) c(rep(1, z), rep(0, length(log.vec) - z))),
        1,
        sum)} else {
          rep(0, length(log.vec))
        }} ))



our.df <- our.df %>%
  group_by(ID) %>%
  mutate(homeScore = c(lead(homeScore)[-length(homeScore)], 0),
         awayScore = c(lead(awayScore)[-length(awayScore)], 0))




#############


# View(our.df)

## Creating a STACKED HOME | AWAY DATASET:
##
## IF THERE'S AT LEAST 1 SHOT: INCLUDE AS MANY "TRUE" OBSERVATIONS AS THERE ARE SHOTS
## IF THERE ARE NO SHOTS: INCLUDE 1 "FALSE" OBSERVATION

my.func <- function(x){
  if (sum(x) > 0){
    return(rep(TRUE, sum(x)))
  } else {
    return(FALSE)
  }
}


# interm.df <- our.df %>%
#   select(gameId, ID, half_id, Minute.clean,  homeScore, awayScore, homeRedCardTot, awayRedCardTot, Weighted.Win.Prob.Home, ShotAttempts) %>%
#   mutate(ShotAttempt.Home = ifelse(ShotAttempts == "Home", TRUE, FALSE),
#          ShotAttempt.Away = ifelse(ShotAttempts == "Away", TRUE, FALSE)) %>%
#   group_by(gameId, ID, half_id, Minute.clean,  homeScore, awayScore, homeRedCardTot, awayRedCardTot) %>%
#   summarise(ShotAttempt = my.func(ShotAttempt.Home)) %>%
#   # summarise(ShotAttempt = ifelse(sum(ShotAttempt.Home) > 0, rep(TRUE, sum(ShotAttempt.Home)), FALSE)) %>%
#   # ShotAttempt.Away = ifelse(sum(ShotAttempt.Away > 0), rep(TRUE, sum(ShotAttempt.Away)), FALSE)) %>%
#   ungroup() %>%
#   group_by(gameId, ID) %>%
#   arrange(desc(half_id), desc(Minute.clean), desc(homeScore), desc(awayScore), .by_group=TRUE) %>%
#   ungroup() %>%
#   mutate(ScoreDiff = homeScore - awayScore,
#          RedCardDiff = homeRedCardTot - awayRedCardTot) %>%
#   select(-homeScore, -awayScore, -homeRedCardTot, -awayRedCardTot) %>%
#   mutate(Home.Away = "Home")


diff(interm.df$Minute.clean)

interm.df[c(768:772),]

final.df <- 
rbind(
  our.df %>%
    select(gameId, ID, half_id, Minute.clean,  homeScore, awayScore, homeRedCardTot, awayRedCardTot, Weighted.Win.Prob.Home, ShotAttempts) %>%
    mutate(ShotAttempt.Home = ifelse(ShotAttempts == "Home", TRUE, FALSE),
           ShotAttempt.Away = ifelse(ShotAttempts == "Away", TRUE, FALSE)) %>%
    group_by(gameId, ID, half_id, Minute.clean,  homeScore, awayScore, homeRedCardTot, awayRedCardTot, Weighted.Win.Prob.Home) %>%
    summarise(ShotAttempt = my.func(ShotAttempt.Home)) %>%
    # summarise(ShotAttempt = ifelse(sum(ShotAttempt.Home) > 0, rep(TRUE, sum(ShotAttempt.Home)), FALSE)) %>%
    # ShotAttempt.Away = ifelse(sum(ShotAttempt.Away > 0), rep(TRUE, sum(ShotAttempt.Away)), FALSE)) %>%
    ungroup() %>%
    group_by(gameId, ID) %>%
    arrange(desc(half_id), desc(Minute.clean), desc(homeScore), desc(awayScore), .by_group=TRUE) %>%
    ungroup() %>%
    mutate(ScoreDiff = homeScore - awayScore,
           RedCardDiff = homeRedCardTot - awayRedCardTot) %>%
    select(-homeScore, -awayScore, -homeRedCardTot, -awayRedCardTot) %>%
    mutate(Home.Away = "Home") %>%
    rename(Weighted.Win.Prob=Weighted.Win.Prob.Home),
  
  our.df %>%
    select(gameId, ID, half_id, Minute.clean,  homeScore, awayScore, homeRedCardTot, awayRedCardTot, Weighted.Win.Prob.Home, ShotAttempts) %>%
    mutate(ShotAttempt.Home = ifelse(ShotAttempts == "Home", TRUE, FALSE),
           ShotAttempt.Away = ifelse(ShotAttempts == "Away", TRUE, FALSE)) %>%
    group_by(gameId, ID, half_id, Minute.clean,  homeScore, awayScore, homeRedCardTot, awayRedCardTot, Weighted.Win.Prob.Home) %>%
    summarise(ShotAttempt = my.func(ShotAttempt.Away)) %>%
    # summarise(ShotAttempt = ifelse(sum(ShotAttempt.Home) > 0, rep(TRUE, sum(ShotAttempt.Home)), FALSE)) %>%
    # ShotAttempt.Away = ifelse(sum(ShotAttempt.Away > 0), rep(TRUE, sum(ShotAttempt.Away)), FALSE)) %>%
    ungroup() %>%
    group_by(gameId, ID) %>%
    arrange(desc(half_id), desc(Minute.clean), desc(homeScore), desc(awayScore), .by_group=TRUE) %>%
    ungroup() %>%
    mutate(ScoreDiff = -(homeScore - awayScore),
           RedCardDiff = -(homeRedCardTot - awayRedCardTot)) %>%
    select(-homeScore, -awayScore, -homeRedCardTot, -awayRedCardTot) %>%
    mutate(Home.Away = "Away", Weighted.Win.Prob.Home = -Weighted.Win.Prob.Home) %>%
    rename(Weighted.Win.Prob=Weighted.Win.Prob.Home)
)

dim(final.df)

View(head(final.df))


write.csv(file="Bundesliga_Complete_MinuteByMinute_Dataset.csv",
          final.df, row.names=F)

# dim(final.df %>%
#       filter(Home.Away == "Home"))
# dim(final.df %>%
#       filter(Home.Away == "Away"))
#
# View(our.df)
