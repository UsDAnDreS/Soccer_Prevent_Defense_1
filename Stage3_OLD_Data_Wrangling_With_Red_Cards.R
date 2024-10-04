#######
### 2522451
#######


####
## SEE LINES 80+, working on PREMATURE "END OF MATCH/SECOND HALF" ENTRIES
#####

####
##  SEE LINE 195+
##
## CHECK what's up with 0 MINUTES SPENT, YET 5+ SHOTS
##  => THOSE ARE BAD RECORDINGS AGAIN... e.g. mentions a whole different game during commentary
##    BUUUUT... SOME ARE POTENTIALLY THOSE "90"/"45" WHICH DON'T CHANGE WITH THE EXTRA TIME...
##
## ALSO.. TRY LOOKING FOR the "# OF DIFFERENT TEAM NAMES WITHIN PARENTHESES"
## (as some games mention TOTALLY DIFFERENT GAMES DURING COMMENTARY)
####


######
## NOTE: there are DIFFERENT GAMES under the SAME GAME_ID!!!
## E.g. 252451 shows up as both: "Cologne FC vs someone" and "Hertha vs VfL Wolfsburg"
## => THAT TURNS OUT TO BE THE ONLY CASE, but it SHOULD BE PRUDENT TO CREATE OUR OWN ID VARIABLE based on:
##    gameId, gamedDate, HomeTeam, AwayTeam


## SOME STUFF SEEMS OUT OF ORDER:
##      - Either the homeScore, awayScore  isn't always sequentially increasing
##      - Or something is going on with the minutes
##
##  CHECK: 252731 2008-09-20 Bayern Munich  vs Werder
##
## ISSUE: if there's a score on min 45+ on the first half, then it doesn't change for minutes that are equal to or lower in the 2nd half
##        FIX: Need to find all the moments where there's lower scores for later observations, and set them to the previous, higher, score


library(tidyverse)

our.df <- read.csv("Bundesliga_All_Minutes_With_Score_Updated_WITH_BOOKING_ODDS.csv",
                   colClasses= c(Minute="character"))
head(our.df,100)

# our.df %>% group_by(gameId) %>%
#   summarise(n.dates = length(unique(gamedDate)),
#             n.HomeTeams = length(unique(HomeTeam)),
#             n.AwayTeams = length(unique(AwayTeam))) %>%
#   filter(n.dates > 1 | n.HomeTeams > 1 | n.AwayTeams > 1)

our.df <- our.df %>%
  mutate(ID = group_indices(our.df, .dots=c("gameId", "gamedDate", "HomeTeam", "AwayTeam", "HomeBet", "DrawBet", "AwayBet", "Weighted.Win.Prob.Home"))) %>%
  arrange(ID)

print(length(unique(our.df$ID)))

###
## CHECK FOR >2 NAMES IN PARENTHESES
###

library(stringr)
# Get the parenthesis and what is inside
parenth.list <- str_extract_all(our.df$Event, "\\([^()]+\\)")
# VAR.entries <- 

our.df$Mentioned.Team <- NA
our.df$Mentioned.Team[sapply(parenth.list, length) == 1] <- unlist(parenth.list)
our.df$Mentioned.Team[str_detect(our.df$Event, "VAR Decision")] <- NA

bad.games.w.too.many.team.mentions <- our.df %>%
  group_by(ID) %>%
  summarise(n.team.mentions = length(unique(Mentioned.Team))) %>%
  filter(n.team.mentions > 3) %>%
  .[["ID"]]


print("Number of games with too many team mentions (>2 per game), to be dropped:")
print(length(bad.games.w.too.many.team.mentions))

dim(our.df)
our.df <- our.df %>% 
  filter(!ID %in% bad.games.w.too.many.team.mentions)



###
## CHECK FOR "MATCH ENDS" PRE-MATURE ENTRIES
## (check "PREMATURITY" by seeing if:
##      1. It's the last entry
##      2. If not, whether the following entry has a higher minute entry
###

bad.game.ending.entries <- our.df %>%
  group_by(ID) %>%
  summarise(ind.match.end = tail(which(str_detect(tolower(Event), "match ends|second half ends")), 1),
            minute.after.fail = ifelse(ind.match.end == 1,
                                       F,
                                       Minute[ind.match.end] > Minute[ind.match.end-1]))  %>%
  filter(minute.after.fail == T) %>%
  select(ID,  
         ind.match.end)

## Simply REMOVE the problematic observations (don't move them anywhere.. there are more problems)
## (Some are still coming with '45.1' minute)
for (j in 1:length(bad.game.ending.entries$ID)){
  our.ind <- which(our.df$ID == bad.game.ending.entries$ID[j])
  bad.ind <- our.ind[bad.game.ending.entries$ind.match.end[j]]
  our.df <- our.df[-bad.ind, ]
}



View(our.df %>%
       filter(ID==1509))


###
## Check for UNORDERED MINUTES
## (a clear issue on several occasions)
## 
## ... TOUGH TO RESOLVE.. there are issues that are too inconsistent to be able to pin down
## (starting minutes of the 2nd half - can be 45, 46, 50, etc)
###

# our.ind <- which(str_detect(tolower(our.df$Event), "second half begins"))
# View(our.df[sort(c(our.ind, our.ind-1, our.ind+1)), ] %>% select(ID, Minute, Event))
# 
# our.ind <- which(str_detect(tolower(our.df$Event), "first half ends"))
# View(our.df[sort(c(our.ind, our.ind-1, our.ind+1)), ] %>% select(Minute, Event))
# table(our.df[sort(c(our.ind, our.ind-1, our.ind+1)), ]$Minute)




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

View(our.df %>% filter(ID==3))

View(our.df %>%
       # filter(gameId == 252451 )
       filter(gameId == 252731)
)




#######
## DATES
#######

# our.df$gamedDate <- str_replace(our.df$gamedDate, "^\\s", "")
# our.df$gamedDate <- as.Date(our.df$gamedDate, "%B %d %Y")
# 
# our.df$gamedDate

tail(our.df$gamedDate)

dim(our.df)




########
## MAKING A VARIABLE KEEPING TRACK OF RED-CARDED MEN, based on REDCARD COLUMN ("HOME"/"AWAY")
########

## REDEFINE REDCARD to also include SECOND YELLOWS !!!
table(our.df$RedCard)
our.df$RedCard <- ifelse((our.df$YellowCard != "Null") & str_detect(tolower(our.df$Event), "second yellow"),
                         our.df$YellowCard,
                         our.df$RedCard)

  
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






### Old code checking bad games
#  bad.gameIds <- our.df %>%
#   group_by(gameId, gamedDate, HomeTeam, AwayTeam) %>%
#   summarise(homeScore.diff=diff(homeScore.old),
#             awayScore.diff=diff(awayScore.old)) %>%
#   filter(homeScore.diff > 0 | awayScore.diff > 0) %>%
#   .[["gameId"]] %>% unique()
# 
# View(our.df %>%
#   filter(gameId %in% bad.gameIds) %>%
#   select(ID, Minute, Event, homeScore, awayScore, homeScore, awayScore, homeScore.old, awayScore.old))

# View(our.df %>% filter(gameId %in% bad.gameIds))


# tail(sort(unique(as.Date(our.df$gamedDate))))

our.df$Minute.num <- as.numeric(our.df$Minute) 
sort(unique(our.df$Minute.num))


## REMOVE GAMES WITH BAD MINUTES (above... 120?)

bad.game.ids <- unique(our.df$gameId[which(our.df$Minute.num > 121)])
bad.game.ids

our.df <- our.df %>%
  filter(!gameId %in% bad.game.ids)
dim(our.df)


# Summing up the "45.10" into "45+10 = 55"
our.df$Minute.clean <- sapply(strsplit(our.df$Minute, "\\."),
                              function(x) sum(as.numeric(x)))

# Half id: anything that has <46 is in 1st half 
# (even the extra time is converted to a number under 46, e.g. "45.1" is 45+1)

our.df$half_id <- ifelse(our.df$Minute.num < 46, 
                         1, 2)


our.df <- our.df %>%
  group_by(ID) %>%
  mutate(homeScore = c(lead(homeScore)[-length(homeScore)], 0),
         awayScore = c(lead(awayScore)[-length(awayScore)], 0))


write.csv(our.df, file="A5_CUMSUM_RedCard_AS.csv",
          row.names=F)


upd.df <- our.df %>%
  group_by(ID, gameId, half_id, homeScore, awayScore, homeRedCardTot, awayRedCardTot, HomeBet, DrawBet, AwayBet, Weighted.Win.Prob.Home) %>%
  summarise(min.minute = ifelse(homeScore[1] == 0 & awayScore[1] == 0, 
                                ifelse(half_id[1] == 1, 0, 45), min(Minute.clean)),
            max.minute = max(Minute.clean),
            homeGoals = sum(Goal == "Home"),
            awayGoals = sum(Goal == "Away"),
            homeFouls = sum(Foul == "Home"),
            awayFouls = sum(Foul == "Away"),
            homeCorners = sum(Corner == "Home"),
            awayCorners = sum(Corner == "Away"),
            homeRedCards = sum(RedCard == "Home"),
            awayRedCards = sum(RedCard == "Away"),
            homeShots = sum(ShotAttempts == "Home"),
            awayShots = sum(ShotAttempts == "Away"),
            homeSaves = sum(Saves == "Home"),
            awaySaves = sum(Saves == "Away")
  ) %>% 
  ungroup() %>%
  group_by(ID, gameId, half_id) %>%
  arrange(min.minute, .by_group = TRUE)

head(upd.df)

## Making sure that the "min.minute" for 1st half is 0, and for 2nd half is 45
upd.df$min.minute <- upd.df %>% group_by(ID, gameId, half_id) %>%
  summarise(min.minute = ifelse(half_id == 1, 
                                c(0, min.minute[-1]),
                                c(45, min.minute[-1]))) %>% .[["min.minute"]]


upd.df <- upd.df %>%
  group_by(ID, gameId, half_id) %>%
  mutate(min.spent = c(ifelse(half_id[1] == 1,
                              max.minute[1],
                              max.minute[1]-45),
                       diff(max.minute)))  #%>%
# select(homeScore, awayScore, min.minute, max.minute, min.spent)

## Game ids where "min spent" are negative, due to bad initial recording of the data (stuff was out of order, etc etc)

upd.df %>% filter(min.spent < 0) %>%
  select(homeScore, awayScore, min.minute, max.minute, min.spent)

bad.IDs <- unique(upd.df %>% filter(min.spent <0) %>% .[["ID"]])
bad.IDs

print("# of games with negative minutes spent (to be dropped):")
print(length(bad.IDs))

upd.df <- upd.df %>%
  filter(!ID %in% bad.IDs)
dim(upd.df)


######
## CHECKING MIN.SPENT == 0 !!
######

# bad.IDs <- unique(upd.df %>% filter(min.spent <0 | (min.spent == 0 & ())) %>% .[["ID"]])
# View(upd.df %>% filter(min.spent == 0))


# View(upd.df[sort(c(our.ind, our.ind+1, our.ind-1)), ] %>%
#        select(homeScore, awayScore, min.minute, max.minute, min.spent))

main.df <- upd.df %>%
  mutate(Score.Diff = homeScore - awayScore,
         RedCard.Diff = homeRedCardTot - awayRedCardTot) %>%
  group_by(ID, gameId, Score.Diff, RedCard.Diff, HomeBet, DrawBet, AwayBet, Weighted.Win.Prob.Home) %>%
  summarise(minutes.spent = sum(min.spent),
            homeGoals = sum(homeGoals), awayGoals = sum(awayGoals),
            homeFouls = sum(homeFouls), awayFouls = sum(awayFouls),
            homeCorners = sum(homeCorners), awayCorners = sum(awayCorners), 
            homeRedCards = sum(homeRedCards), awayRedCards = sum(awayRedCards), 
            homeShots = sum(homeShots), awayShots = sum(awayShots),
            homeSaves = sum(homeSaves), awaySaves = sum(awaySaves))

dim(main.df)
# View(main.df)


## Saving it for MULTI-RESPONSE VARIABLE
write.csv(file="main_df_w_red_cards_WITH_BOOKING_ODDS.csv", main.df, row.names=F)


home.df <- data.frame(main.df[, c("ID", "gameId", "Score.Diff", "RedCard.Diff", "HomeBet", "DrawBet", "Weighted.Win.Prob.Home")], 
                      HomeAway="Home",
                      main.df[, c("minutes.spent", colnames(main.df)[str_detect(colnames(main.df), "home")])])
colnames(home.df) <- str_remove(colnames(home.df), "home")
colnames(home.df)[colnames(home.df) == "HomeBet"] <- "WinBet"
colnames(home.df)[colnames(home.df) == "Weighted.Win.Prob.Home"] <- "Weighted.Win.Prob"



away.df <- data.frame(main.df[, c("ID", "gameId", "Score.Diff", "RedCard.Diff", "AwayBet", "DrawBet", "Weighted.Win.Prob.Home")], 
                      HomeAway="Away",
                      main.df[, c("minutes.spent", colnames(main.df)[str_detect(colnames(main.df), "away")])])
away.df$Score.Diff <- -away.df$Score.Diff
away.df$RedCard.Diff <- -away.df$RedCard.Diff
away.df$Weighted.Win.Prob.Home <- -away.df$Weighted.Win.Prob.Home

colnames(away.df) <- str_remove(colnames(away.df), "away")
colnames(away.df)[colnames(away.df) == "AwayBet"] <- "WinBet"
colnames(away.df)[colnames(away.df) == "Weighted.Win.Prob.Home"] <- "Weighted.Win.Prob"
colnames(away.df)

final.df <- rbind(home.df, away.df) %>% 
  arrange(ID, abs(Score.Diff))



#### Checking the NEGATIVE MINUTES SPENT

final.df %>% filter(minutes.spent < 0)

## CHECKING THE 0 MINUTES SPENT, YET A BUNCH OF STATS ACCUMULATED
##  => THOSE ARE BAD RECORDINGS AGAIN... e.g. mentions a whole different game during commentary
final.df %>% 
  filter(minutes.spent == 0, Shots > 0)

View(our.df %>%
       filter(ID == 1589))

# upd.df %>%
#   filter(ID == 1589)

## The "match ends" entry at the halftime is ruining some MAX.MINUTES

final.df <- final.df %>% mutate(ID = factor(ID), gameId = factor(gameId))
write.csv(file="final_df_w_red_cards_WITH_BOOKING_ODDS.csv", final.df, row.names=F)

