library(mgcv)
library(tidyverse)
library(splines)

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

# View(our.df %>% filter(ID==3))
# 
# View(our.df %>%
#        # filter(gameId == 252451 )
#        filter(gameId == 252731)
# )

our.df$gamedDate <- str_replace(our.df$gamedDate, "^\\s", "")
our.df$gamedDate <- as.Date(our.df$gamedDate) #, "%B %d %Y")

tail(our.df$gamedDate)

dim(our.df)




########
## MAKING A VARIABLE KEEPING TRACK OF RED-CARDED MEN, based on REDCARD COLUMN ("HOME"/"AWAY")
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



write.csv(file="CLEANED_Bundesliga_All_Minutes_With_Score_Updated_WITH_BOOKING_ODDS.csv",
          our.df,
          row.names = F)

