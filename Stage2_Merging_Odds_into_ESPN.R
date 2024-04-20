###
# Main data file
###

our.df <- read.csv("Bundesliga_All_Minutes_With_Score_Updated.csv",
                   colClasses= c(Minute="character"))

our.df$gamedDate <- str_replace(our.df$gamedDate, "^\\s", "")
our.df$gamedDate <- as.Date(our.df$gamedDate, "%B %d %Y")


####
## Odds file, with ESPN names added
####

book.df <- read.csv("full_booking_odds_file_Bundesliga.csv")
# View(data.frame(book.df$Date,
#                 as.Date(book.df$Date, "%d %B %Y")))
book.df$Date <- as.Date(book.df$Date, "%d %B %Y")


####
## Merging by:
##   "gamedDate" = "Date"
##    HomeTeam = HomeTeam.espn
##    AwayTeam = AwayTeam.espn
####
dim(our.df)

merged.df <- our.df %>%
  left_join(book.df %>%
              select(-HomeTeam, -AwayTeam),
            by=c("gamedDate" = "Date",
                 "HomeTeam" = "HomeTeam.espn",
                 "AwayTeam" = "AwayTeam.espn"))
dim(merged.df)

View(merged.df)

View(merged.df %>% 
  filter(HomeTeam == "Bayern Munich"))

View(book.df)

## Checking observations for which bet data was NA
## (hence the betting data was missing in the Bundesliga file)

# How many games?
length(merged.df %>% 
  filter(is.na(HomeBet)) %>%
  select(gameId) %>%
  unique() %>% .[[1]])

# Mostly from Spanish league
merged.df %>% 
  filter(is.na(HomeBet)) %>%
  select(HomeTeam) %>%
  unique()

merged.df %>% 
  filter(is.na(HomeBet)) %>%
  select(AwayTeam) %>%
  unique()



#####
## Saving into a CSV file
#####

write.csv(file="Bundesliga_All_Minutes_With_Score_Updated_WITH_BOOKING_ODDS.csv",
          merged.df,
          row.names=F)
