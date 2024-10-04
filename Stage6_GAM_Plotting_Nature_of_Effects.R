library(readr)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(MASS)
library(xtable)
library(mgcv)
library(gratia)
library(ggplot2)  
library(patchwork)


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE


#########
### SHOTS
#########

# load("gam_ziP_obj_Shots.Robj")

# load(file=paste0("gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Shots.Robj"))



#######
### LOADING, SETTING UP DATA
#######

gam.nb.obj <- list()

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

for (league in league.name){

## To be used for modeling (excluding bad minute entries)
our.df <- read.csv(paste0(league, 
                          ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
                          "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))


## REMOVING NA betting data
our.df <- our.df %>% filter(!is.na(Weighted.Win.Prob))
dim(our.df)


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

}




# Compare smooths across the models
Bundesliga <- gam.nb.obj[["Bundesliga"]]
comp <- gratia::compare_smooths(model=Bundesliga,
                                'Serie A' = gam.nb.obj[["SerieA"]], 
                                'Ligue 1' = gam.nb.obj[["Ligue1"]], 
                                "Premier League" = gam.nb.obj[["PremierLeague"]], 
                                "La Liga" = gam.nb.obj[["LaLiga"]])

# Draw the comparison and store the plot objects
plots <- gratia::draw(comp)

# Set the desired y-axis limits
y_limits <- c(-1, 1)  # Example y-axis limits

# Extract individual plots from the patchwork object
plot_list <- list()
for (i in 1:length(plots)) {
  plot_list[[i]] <- plots[[i]]
}

# Adjust the y-axis limits for the second plot and remove legends for all plots
adjusted_plots_shots <- lapply(seq_along(plots), function(i) {
  plot <- plots[[i]]
  if (i == 1) {
    plot + 
      labs(y = "Shots.Per.Min", x= "Red Card Difference") +
      geom_vline(xintercept = c(-2:2), linetype = "dashed", color = "black") +
      coord_cartesian(ylim = y_limits) + 
      scale_x_continuous(breaks = -2:2) +  # Set custom breaks on x-axis
      theme_minimal() +
      theme(plot.title = element_blank())
  } else if (i == 2) {
    plot + 
      labs(y = "Shots.Per.Min", x= "Score Difference") +
      geom_vline(xintercept = c(-2:2), linetype = "dashed", color = "black") +
      coord_cartesian(ylim = y_limits) + 
      scale_x_continuous(breaks = -8:8) +  # Set custom breaks on x-axis
      theme_minimal() +
      theme(plot.title = element_blank())
  } else {
    plot +
      labs(y = "Shots.Per.Min", x= "Win Prob Difference") +
      coord_cartesian(ylim = y_limits) + 
      theme_minimal() +
      theme(plot.title = element_blank())
  } 
})


## For the slides presentation
adjusted_plots_shots_short1 <- adjusted_plots_shots[c(2, 1)]
# adjusted_plots_shots_short2 <- adjusted_plots_shots[c(1, 4)]


# Reorder the plots based on the x-axis variable
adjusted_plots_shots <- adjusted_plots_shots[c(2, 1, 3)]

wrap_plots(adjusted_plots_shots) +
  plot_layout(ncol = 1, byrow=FALSE) +
  plot_layout(guides = "collect")  &
  guides(color = guide_legend(title = "Leagues"), fill = guide_legend(title = "Leagues"))




########
## CORNERS
########

#load("gam_ziP_obj_Corners.Robj")
# load(file=paste0("gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))

gam.nb.obj <- list()

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

for (league in league.name){
  
  ## To be used for modeling (excluding bad minute entries)
  our.df <- read.csv(paste0(league, 
                            ifelse(remove.extra, "_NO_EXTRA_TIME", ""), 
                            "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  ## REMOVING NA betting data
  our.df <- our.df %>% filter(!is.na(Weighted.Win.Prob))
  dim(our.df)
  
  
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
  
  gam.nb.obj[[league]] <- gam(Corners ~
                                 s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5) + log(minutes.spent + 1),
                               family = "nb", data= our.df)
  
}



# Compare smooths across the models
Bundesliga <- gam.nb.obj[["Bundesliga"]]
comp <- gratia::compare_smooths(model=Bundesliga,
                                'Serie A' = gam.nb.obj[["SerieA"]], 
                                'Ligue 1' = gam.nb.obj[["Ligue1"]], 
                                "Premier League" = gam.nb.obj[["PremierLeague"]], 
                                "La Liga" = gam.nb.obj[["LaLiga"]])

# Draw the comparison and store the plot objects
plots <- gratia::draw(comp)

# Set the desired y-axis limits
y_limits <- c(-1, 1)  # Example y-axis limits

# Extract individual plots from the patchwork object
plot_list <- list()
for (i in 1:length(plots)) {
  plot_list[[i]] <- plots[[i]]
}

# Adjust the y-axis limits for the second plot and remove legends for all plots
adjusted_plots_corners <- lapply(seq_along(plots), function(i) {
  plot <- plots[[i]]
  if (i == 1) {
    plot + 
      labs(y = "Corners.Per.Min", x= "Red Card Difference") +
      geom_vline(xintercept = c(-2:2), linetype = "dashed", color = "black") +
      coord_cartesian(ylim = y_limits) + 
      scale_x_continuous(breaks = -2:2) +  # Set custom breaks on x-axis
      theme_minimal() +
      theme(plot.title = element_blank())
  } else if (i == 2) {
    plot + 
      labs(y = "Corners.Per.Min", x= "Score Difference") +
      geom_vline(xintercept = c(-2:2), linetype = "dashed", color = "black") +
      coord_cartesian(ylim = y_limits) + 
      scale_x_continuous(breaks = -8:8) +  # Set custom breaks on x-axis
      theme_minimal() +
      theme(plot.title = element_blank())
  } else {
    plot +
      labs(y = "Corners.Per.Min", x= "Win Prob Difference") +
      coord_cartesian(ylim = y_limits) + 
      theme_minimal() +
      theme(plot.title = element_blank())
  } 
})

## For the slides presentation
adjusted_plots_corners_short1 <- adjusted_plots_corners[c(2,1)]
# adjusted_plots_corners_short2 <- adjusted_plots_corners[c(1, 4)]

# Reorder the plots based on the x-axis variable
adjusted_plots_corners <- adjusted_plots_corners[c(2, 1, 3)]

wrap_plots(adjusted_plots_corners) +
  plot_layout(ncol = 1, byrow=FALSE) +
  plot_layout(guides = "collect")  &
  guides(color = guide_legend(title = "Leagues"), fill = guide_legend(title = "Leagues"))





#######
## COMBINING SHOTS & CORNERS
#######

# Combine the adjusted plots back into a patchwork object
adjusted_patchwork <- wrap_plots(append(adjusted_plots_shots, adjusted_plots_corners))

#print(adjusted_plots_shots)

# Set the size of each plot (adjust as needed)
adjusted_patchwork <- adjusted_patchwork + 
  plot_layout(ncol = 2, byrow=FALSE)

# Print the adjusted patchwork object with one common legend
#

print(adjusted_patchwork + 
        plot_layout(guides = "collect") & 
        guides(color = guide_legend(title = "Leagues"), fill = guide_legend(title = "Leagues")))



##### For the SLIDES PRESENTATION

## 1. SCORE & REDCARD DIFF

# Combine the adjusted plots back into a patchwork object
adjusted_patchwork_short1 <- wrap_plots(append(adjusted_plots_shots_short1, adjusted_plots_corners_short1))

#print(adjusted_plots_shots)

# Set the size of each plot (adjust as needed)
adjusted_patchwork_short1 <- adjusted_patchwork_short1 + 
  plot_layout(ncol = 2, byrow=FALSE)

# Print the adjusted patchwork object with one common legend
#
# Width: 698; Height: 465
print(adjusted_patchwork_short1 + 
        plot_layout(guides = "collect") & 
        guides(color = guide_legend(title = "Leagues"), fill = guide_legend(title = "Leagues")))
