###
# Main data file
###

our.df <- read.csv("Bundesliga_All_Minutes_With_Score_Updated.csv",
                   colClasses= c(Minute="character"))

our.df$gamedDate <- str_replace(our.df$gamedDate, "^\\s", "")
our.df$gamedDate <- as.Date(our.df$gamedDate, "%B %d %Y")

our.df <- our.df %>%
  mutate(ID = group_indices(our.df, .dots=c("gameId", "gamedDate", "HomeTeam", "AwayTeam"))) %>%
  arrange(ID)



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




######
######
## ODDS
######
######

library(tidyverse)

all.book.files <- list.files("Odds_Data")

our.book.df <- NULL
for (j in 1:length(all.book.files)){
  our.book.df <- rbind(our.book.df,
                       read.csv(paste0("Odds_Data/", all.book.files[j]), header=T))
}

dim(our.book.df)


##########
## Creating "Weighted.Win.Prob.Home"
##  p_win*1 + p_draw*0 + p_loss*(-1)
## (making sure that none of the p's are Inf or Negative)
##########

our.book.df$Weighted.Win.Prob.Home <- ifelse(our.book.df$HomeBet > 1 & our.book.df$DrawBet > 1 & our.book.df$AwayBet > 1,
                                             (1)*1/our.book.df$HomeBet  + (0)*1/our.book.df$DrawBet + (-1)* 1/our.book.df$AwayBet,
                                             NA)
summary(our.book.df$Weighted.Win.Prob.Home)
View(our.book.df)



#######
## FIGURING OUT THE TEAM NAME MATCHING
#######

## All distinct team names from MAIN df
our.df.names <- sort(unique(c(our.df$HomeTeam, our.df$AwayTeam)))
our.df.names 

wrong.league.names <- c("Osasuna", "Atletico Madrid", "Mlaga","Espanyol", "Almera", 
                        "Villarreal", "Racing Santander", "Athletic Club", "Numancia",
                        "Real Madrid", "Deportivo La Corua",
                        "Valencia", "Real Valladolid", "Sporting Gijn", "Getafe", "Barcelona",                 
                        "Mallorca", "Sevilla")

our.df.names <- our.df.names[!our.df.names %in% wrong.league.names]
our.df.names

## All distinct team names from BOOKING df
our.book.team.names <- sort(unique(c(our.book.df$HomeTeam, our.book.df$AwayTeam)))
our.book.team.names


#######
## Name matching process:
##    1. First find the best matching names from the odds to each distinct ESPN name.
##    2. Then for the unmatched odds names, find the remaining ESPN name matches.
## We'll end up with some ESPN names matched to several booking odds names simply because the
## latter had errors (e.g. "BayerN Leverkusen" typo)
#######

library(stringdist)
match.book <- our.book.team.names[amatch(our.df.names, our.book.team.names, maxDist = Inf)]
match.book[our.df.names == "TSV Eintracht Braunschweig"] <- "Braunschweig"
name.match.list <- data.frame(booking=match.book, espn=our.df.names)

# our.df.match <- our.df.names[amatch(our.book.team.names, our.df.names, maxDist = Inf)]
# our.df.match
# our.df.match[our.book.team.names == "Braunschweig"] <- "TSV Eintracht Braunschweig"
# 
# cbind(our.book.team.names,
#       our.df.match)

unmatched.book.names <- our.book.team.names[!our.book.team.names %in% match.book]
unmatched.book.names

our.match <- our.df.names[amatch(unmatched.book.names, our.df.names, maxDist = Inf)]
our.match

name.match.list <- rbind(name.match.list,
                         data.frame(booking=unmatched.book.names, espn=our.match))

name.match.list 
name.match.list[name.match.list$booking == "Eintracht",]$espn <- "Eintracht Frankfurt"
name.match.list[name.match.list$booking == "Heidenheim", ]$espn <- "1. FC Heidenheim 1846"
name.match.list[name.match.list$booking == "Holstein Kiel", ]$espn <- "Holstein Kiel"

name.match.list

write.csv(file="Bundesliga_ESPN_Booking_Name_Matching_List.csv", 
          name.match.list,
          row.names = F)



#######
## Joining the ESPN names onto the Booking odds 
## (because each Booking odds name will have a UNIQUE ESPN name match,
##  NOT VICE VERSA)
#######

full.book.df <- our.book.df %>%
  left_join(name.match.list,
            by = c("HomeTeam" = "booking")) %>%
  left_join(name.match.list,
            by = c("AwayTeam" = "booking")) %>%
  rename(HomeTeam.espn = espn.x,
         AwayTeam.espn = espn.y)

write.csv(file="full_booking_odds_file_Bundesliga.csv",
          full.book.df,
          row.names = F)
