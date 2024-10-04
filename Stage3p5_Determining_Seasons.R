league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

## 2020:
##    Bundesliga => last games end of June, a couple of relegation games in early July (not in dataset though)
##    SerieA => went into July AND August... (last games Aug 2nd), "2020-08-02"
##    LaLiga => went into July, last games July 19th, "2020-07-19"
##    Ligue 1 => cancelled games rest of the way
##    Premier League => went into July, last games July 26th, "2020-07-26"

years <- c(2009:2023)
july <- as.Date(paste0(c(2008,years), "-07-01"))
end.dates.2020 <- c("2020-07-01", "2020-08-02", "2020-07-19", "2020-07-01", "2020-07-26")
remove.extra <- FALSE

for (league in league.name){
  print(league)
  our.df <- read.csv(file=paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
                        "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  # plot(sort(unique(as.Date(our.df$gamedDate))))
  
 # sort(diff(sort(unique(as.Date(our.df$gamedDate)))), decreasing=T)
  
  #as.Date(our.df$gamedDate)
  
  ## JULY is never there in NON-2020 years, so that's the separator
  table(format(as.Date(unique(our.df$gamedDate)), "%m"))
  
  our.df$gamedDate[format(as.Date(our.df$gamedDate), "%m") == "07"]
  
  plot(as.Date(paste0("2008-", sort(format(as.Date(unique(our.df$gamedDate)), "%m-%d")))))
  
  our.df$season <- numeric(nrow(our.df))
  
  ## If it's neither of 2019/2020 or 2020/2021 seasons, we just check if the dates are between july's
  ## If it's 2019/2020, we go from prior july to the 2020 end date
  ## If it's 2020/2021, we go from 2020 end date (EXCLUDING it) to next july
  for (i in 1:length(years)){
    print(i)
    if (!years[i] %in% c(2020,2021)){
      our.df$season[as.Date(our.df$gamedDate) > july[i] & as.Date(our.df$gamedDate) < july[i+1]] <- years[i]
    } else if (years[i] == 2020) {
      our.df$season[as.Date(our.df$gamedDate) > july[i] & as.Date(our.df$gamedDate) <= end.dates.2020[which(league.name == league)]] <- years[i]
    } else {
      our.df$season[as.Date(our.df$gamedDate) > end.dates.2020[which(league.name == league)] & as.Date(our.df$gamedDate) < july[i+1]] <- years[i]
    }
  }
 
  write.csv(our.df,
            file=paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
                               "_Sami_minute_by_minute_df_w_red_cards_WITH_BOOKING_ODDS.csv"),
            row.names=F) 
}

