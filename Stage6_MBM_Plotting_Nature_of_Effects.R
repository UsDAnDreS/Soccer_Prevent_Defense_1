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

# model.type <- c("ziP", "nb")[2]


# load("gam_ziP_obj_Shots.Robj")

load(file=paste0("gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Shots.Robj"))

# Compare smooths across the models
Bundesliga <- gam.ziP.obj[["Bundesliga"]]
comp <- gratia::compare_smooths(model=Bundesliga,
                                'Serie A' = gam.ziP.obj[["SerieA"]], 
                                'Ligue 1' = gam.ziP.obj[["Ligue1"]], 
                                "Premier League" = gam.ziP.obj[["PremierLeague"]], 
                                "La Liga" = gam.ziP.obj[["LaLiga"]])

# Draw the comparison and store the plot objects
plots <- gratia::draw(comp)

# Set the desired y-axis limits
y_limits <- c(-0.6, 0.6)  # Example y-axis limits

# Extract individual plots from the patchwork object
plot_list <- list()
for (i in 1:length(plots)) {
  plot_list[[i]] <- plots[[i]]
}

# Adjust the y-axis limits for the second plot and remove legends for all plots
adjusted_plots_shots <- lapply(seq_along(plots), function(i) {
  plot <- plots[[i]]
  if (i == 2) {
    plot + 
      labs(y = "Shots.Per.Min", x= "Red Card Difference") +
      geom_vline(xintercept = c(-2:2), linetype = "dashed", color = "black") +
      coord_cartesian(ylim = y_limits) + 
      scale_x_continuous(breaks = -2:2) +  # Set custom breaks on x-axis
      theme_minimal() +
      theme(plot.title = element_blank())
  } else if (i == 3) {
    plot + 
      labs(y = "Shots.Per.Min", x= "Score Difference") +
      geom_vline(xintercept = c(-2:2), linetype = "dashed", color = "black") +
      coord_cartesian(ylim = y_limits) + 
      scale_x_continuous(breaks = -8:8) +  # Set custom breaks on x-axis
      theme_minimal() +
      theme(plot.title = element_blank())
  } else {
    plot +
      labs(y = "Shots.Per.Min", x= ifelse(i==1, "Minute", "Win Prob Difference")) +
      coord_cartesian(ylim = y_limits) + 
      theme_minimal() +
      theme(plot.title = element_blank())
  } 
})


## For the slides presentation
adjusted_plots_shots_short1 <- adjusted_plots_shots[c(3, 2)]
adjusted_plots_shots_short2 <- adjusted_plots_shots[c(1, 4)]


# Reorder the plots based on the x-axis variable
adjusted_plots_shots <- adjusted_plots_shots[c(3, 2, 1)]

wrap_plots(adjusted_plots_shots) +
  plot_layout(ncol = 1, byrow=FALSE) +
  plot_layout(guides = "collect")  &
  guides(color = guide_legend(title = "Leagues"), fill = guide_legend(title = "Leagues"))




########
## CORNERS
########

#load("gam_ziP_obj_Corners.Robj")
load(file=paste0("gam_ziP_obj",  ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Corners.Robj"))


# Compare smooths across the models
Bundesliga <- gam.ziP.obj[["Bundesliga"]]
comp <- gratia::compare_smooths(model=Bundesliga,
                                'Serie A' = gam.ziP.obj[["SerieA"]], 
                                'Ligue 1' = gam.ziP.obj[["Ligue1"]], 
                                "Premier League" = gam.ziP.obj[["PremierLeague"]], 
                                "La Liga" = gam.ziP.obj[["LaLiga"]])

# Draw the comparison and store the plot objects
plots <- gratia::draw(comp)

# Set the desired y-axis limits
y_limits <- c(-0.6, 0.6)  # Example y-axis limits

# Extract individual plots from the patchwork object
plot_list <- list()
for (i in 1:length(plots)) {
  plot_list[[i]] <- plots[[i]]
}

# Adjust the y-axis limits for the second plot and remove legends for all plots
adjusted_plots_corners <- lapply(seq_along(plots), function(i) {
  plot <- plots[[i]]
  if (i == 2) {
    plot + 
      labs(y = "Corners.Per.Min", x= "Red Card Difference") +
      geom_vline(xintercept = c(-2:2), linetype = "dashed", color = "black") +
      coord_cartesian(ylim = y_limits) + 
      scale_x_continuous(breaks = -2:2) +  # Set custom breaks on x-axis
      theme_minimal() +
      theme(plot.title = element_blank())
  } else if (i == 3) {
    plot + 
      labs(y = "Corners.Per.Min", x= "Score Difference") +
      geom_vline(xintercept = c(-2:2), linetype = "dashed", color = "black") +
      coord_cartesian(ylim = y_limits) + 
      scale_x_continuous(breaks = -8:8) +  # Set custom breaks on x-axis
      theme_minimal() +
      theme(plot.title = element_blank())
  } else {
    plot +
      labs(y = "Corners.Per.Min", x= ifelse(i==1, "Minute", "Win Prob Difference")) +
      coord_cartesian(ylim = y_limits) + 
      theme_minimal() +
      theme(plot.title = element_blank())
  } 
})

## For the slides presentation
adjusted_plots_corners_short1 <- adjusted_plots_corners[c(3, 2)]
adjusted_plots_corners_short2 <- adjusted_plots_corners[c(1, 4)]

# Reorder the plots based on the x-axis variable
adjusted_plots_corners <- adjusted_plots_corners[c(3, 2, 1)]

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


## 2. Minute

# Combine the adjusted plots back into a patchwork object
adjusted_patchwork_short2 <- wrap_plots(append(adjusted_plots_shots_short2, adjusted_plots_corners_short2))

#print(adjusted_plots_shots)

# Set the size of each plot (adjust as needed)
adjusted_patchwork_short2 <- adjusted_patchwork_short2 + 
  plot_layout(ncol = 2, byrow=FALSE)

# Print the adjusted patchwork object with one common legend
#
# Width: 698; Height: 465
print(adjusted_patchwork_short2 + 
        plot_layout(guides = "collect") & 
        guides(color = guide_legend(title = "Leagues"), fill = guide_legend(title = "Leagues")))

