library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(plotrix)
library(gridExtra)
library(ggpubr)







##############
##############
####  SHOTS
##############
##############


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

extr.score <- c(1, 2, 3, 4, 5)[2]

glm.extreme.obj <- list()


for (league in league.name){
  
  print(league)
  
  # load("final_df_w_red_cards.csv")
  
  final.df <- read.csv(paste0(league, "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  #########
  ## Fitting the Neg Bin regression with:
  ##    * Score.Diff as DUMMY variables with "0" as reference level 
  ##      (achieved via mutate(Score.Diff = relevel(factor(Score.Diff), ref="0")))
  ##    * Merge the RIGHT-HAND extremes into MOST RIGHTHAND CATEGORY, while LEFT-HAND - into MOST LEFTHAND
  ##    * Making effects of Weighted.Win.Prob & RedCard.Diff linear
  #########
  
  
  glm.extreme.obj[[league]] <- glm.nb(Shots ~ Score.Diff + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
                            data=final.df %>% mutate(ID = factor(ID),
                                                     Score.Diff = relevel(factor(
                                                       ifelse(Score.Diff >= extr.score,
                                                              paste0(extr.score, ".or.better"),
                                                              ifelse(Score.Diff <= -extr.score,
                                                                     paste0(-extr.score, ".or.worse"),
                                                                     Score.Diff))), ref="0")))
  glm.extreme.obj[[league]]
  
  
}

glm.extreme.obj[[1]]$coefficients



##########
#######
## FIRST, the DUMMY VARIABLE EFFECTS
#######
##########

#####
### SCORE DIFF DF CALCULATION
#####

coef.df <- NULL

for (league in league.name){
  coef.df <- rbind(coef.df,
                   data.frame(League = league, t(coefficients(glm.extreme.obj[[league]]))))
}

coef.df.scorediff <- coef.df[, c("League", colnames(coef.df)[str_detect(colnames(coef.df), "Score.Diff")])]
coef.df.scorediff$Score.Diff.0 <- 0

  #read.csv("dummy_variable_coefficients Corners.csv")

# Rename columns for clarity
colnames(coef.df.scorediff) <- c('League', '-1', '-2 or worse', '+1',
                                 '+2 or better', '0')

# Transform the dataset to long format
df_long.scorediff <- coef.df.scorediff %>%
  pivot_longer(cols = -League, names_to = "scoreDiffCategory", values_to = "value")

## Making it an exp(value)
df_long.scorediff$value <- exp(df_long.scorediff$value)
#df_long$value <- df_long$value

# Define the desired order of score difference categories

df_long.scorediff$scoreDiffCategory <- factor(df_long.scorediff$scoreDiffCategory,
                                    ordered=T,
                                    levels=c('-2 or worse', '-1', 
                                             '0', 
                                             '+1', '+2 or better'))


# Create a new dataframe for segments
segments_data.scorediff <- df_long.scorediff %>%
  mutate(xmin = as.numeric(factor(scoreDiffCategory))-0.4 ,
         xmax = as.numeric(factor(scoreDiffCategory)) + 0.4,  # Adjust width of segments
         xmid = as.numeric(factor(scoreDiffCategory)),
         ymin = value,  # Start from 0 for y-axis
         ymid = value,
         ymax = value) %>%
  dplyr::select(xmin, xmid, xmax, ymin, ymid, ymax, League)



#####
### RED CARD DIFF DF CALCULATION
#####

coef.df.redcarddiff <- coef.df %>% dplyr::select(League, RedCard.Diff) 

for (j in c(-2:2)){
  coef.df.redcarddiff[, paste(j)] <- exp(j*coef.df.redcarddiff[, "RedCard.Diff"])
}
coef.df.redcarddiff[, "RedCard.Diff"] <- NULL


colnames(coef.df.redcarddiff) <- c('League', '-2', '-1', '0', '+1', '+2')


# Transform the dataset to long format
df_long.redcarddiff <- coef.df.redcarddiff %>%
  pivot_longer(cols = -League, names_to = "redcardDiffCategory", values_to = "value")

df_long.redcarddiff$redcardDiffCategory <- factor(df_long.redcarddiff$redcardDiffCategory,
                                      ordered=T,
                                      levels=c('-2', '-1', 
                                               '0', 
                                               '+1', '+2'))



# Create a new dataframe for segments
segments_data.redcarddiff <- df_long.redcarddiff %>%
  mutate(xmin = as.numeric(factor(redcardDiffCategory))-0.4 ,
         xmax = as.numeric(factor(redcardDiffCategory)) + 0.4,  # Adjust width of segments
         xmid = as.numeric(factor(redcardDiffCategory)),
         ymin = value,  # Start from 0 for y-axis
         ymid = value,
         ymax = value) %>%
  dplyr::select(xmin, xmid, xmax, ymin, ymid, ymax, League)


###
## Finding the common Y-axis limits across score & red card differentials
###

y_min <- min(df_long.scorediff$value, 
             df_long.redcarddiff$value)

y_max <- max(df_long.scorediff$value, 
             df_long.redcarddiff$value)





######
## PLOTTING for SCORE DIFF & RED CARD DIFF EFFECTS
######


score.diff.names <- c('-2 \n or worse', '-1', 
                      '0', 
                      '+1', '+2 \n or better')

# Create the geom_segment plot with horizontal lines
plot1 <- ggplot(segments_data.scorediff, aes(x = xmin, xend = xmax, y = ymin, yend = ymax, color = League)) +
  geom_segment(size = 2, alpha=0.7) +  # Add segments
  geom_line(aes(x=xmid, y=ymid), lwd=1) +
  scale_x_continuous(breaks = 1:length(levels(df_long.scorediff$scoreDiffCategory)), 
                     labels = score.diff.names
                     # labels = levels(df_long.scorediff$scoreDiffCategory)
  ) +  # Use custom order and labels
  theme_minimal() +  # Use a minimal theme
  labs(#title = "Shot Coefficients by Score Differential and League",
    x = "Score Difference Category",
    y = "Multiplicative Effect on Shots") +
  ylim(y_min, y_max)

redcard.diff.names <- c('-2\n', '-1', 
                      '0', 
                      '+1', '+2\n')

# Create the geom_segment plot with horizontal lines
plot2 <- ggplot(segments_data.redcarddiff, aes(x = xmin, xend = xmax, y = ymin, yend = ymax, color = League)) +
  geom_segment(size = 2, alpha=0.7) +  # Add segments
  geom_line(aes(x=xmid, y=ymid), lwd=1) +
  scale_x_continuous(breaks = 1:length(levels(df_long.redcarddiff$redcardDiffCategory)), 
                     # labels = levels(df_long.redcarddiff$redcardDiffCategory)
                     labels = redcard.diff.names
                     ) +  # Use custom order and labels
  theme_minimal() +  # Use a minimal theme
  labs(#title = "Shot Coefficients by Red Card Differential and League",
       x = "Red Card Difference Category",
       y = "Multiplicative Effect on Shots")  +
  ylim(y_min, y_max)
# theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.5))



# SHOTS EFFECT PLOTS
# width: 672;  height: 442

library(ggpubr)
ggarrange(plot1, plot2, common.legend = T, legend="right")







#######
## VARIABLE SELECTION RESULTS
#######


load("Shots_stepwise_obj.Robj")

score.diff.names <- c(paste0("Score.Diff.",c(9:1)), paste0("Score.Diff",c(1:9)))
all.coef.names <- c(score.diff.names, "Weighted.Win.Prob", "HomeAwayHome", "RedCard.Diff")
stepwise.obj[["Bundesliga"]]

coef.matrix <- matrix(0, nrow=5, ncol=length(all.coef.names))
colnames(coef.matrix) <- all.coef.names
proper.league.names <- c("Bundesliga", "Serie A", "La Liga", "Ligue 1", "Premier League")
rownames(coef.matrix) <- proper.league.names

for (j in 1:length(league.name)){
  league.coefs <- stepwise.obj[[league.name[j]]]$coefficients[-1]
  coef.names.league <- names(league.coefs)
  coef.matrix[j, coef.names.league] <- league.coefs
}

print(coef.matrix)
coef.matrix <- cbind(coef.matrix[,1:9], Score.Diff0 = 0, coef.matrix[,-c(1:9)])
exp.coef.mat <- exp(coef.matrix)


exp.coef.mat.only.scores <- exp(coef.matrix[,1:19])
colnames(exp.coef.mat.only.scores) <- str_replace(str_remove(colnames(exp.coef.mat.only.scores), "Score.Diff"),
                                                  "\\.", "-")
colnames(exp.coef.mat.only.scores)[10] <- " 0"
colnames(exp.coef.mat.only.scores)[11:19] <- paste0("+", colnames(exp.coef.mat.only.scores)[11:19])
  

#######
## The actual plot
#######

colnames(exp.coef.mat)[1:19] <- c(paste0("Score Diff = ", colnames(exp.coef.mat.only.scores)))
colnames(exp.coef.mat)[20] <- "Weighted Win Prob"
colnames(exp.coef.mat)[21] <- "Home (vs Away)"
colnames(exp.coef.mat)[22] <- "Red Card Diff"


## width: 530; height: 424
par(mar = c(2.1, 10, 4.0, 2.1))

color2D.matplot(exp.coef.mat,
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)
par(las=2)
staxlab(3,at=c(1:ncol(exp.coef.mat))-0.25,labels=colnames(exp.coef.mat),srt=-45)
par(las=1)
axis(2,at=c(nrow(exp.coef.mat):1)-0.5,labels=rownames(exp.coef.mat))

par(mar = c(5.1, 4.1, 4.1, 2.1))


## FLIPPING IT
## !! LIKE THAT ONE WAY MORE !!
## SCREENSHOTTED into "Stepwise_Selection_Results_Everything.png"

par(mar = c(2.1, 10, 4.0, 2.1))

color2D.matplot(t(exp.coef.mat),
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)
par(las=1)
axis(2,at=c(ncol(exp.coef.mat):1)-0.5,labels=colnames(exp.coef.mat))
# staxlab(2,at=rev(c(1:ncol(exp.coef.mat)))-0.25,labels=colnames(exp.coef.mat))
par(las=1)
axis(3,at=c(nrow(exp.coef.mat):1)-0.5,labels=rownames(exp.coef.mat))

par(mar = c(5.1, 4.1, 4.1, 2.1))





#####
## JUST THE SCORE DIFFERENTIALS
#####

# Imported into: "Stepwise_Selection_ScoreDiff_Only_Wide_Format.png", with
# width: 947;  height: 463

par(mar = c(2.1, 10, 4.0, 2.1))

color2D.matplot(exp.coef.mat.only.scores,
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)
par(las=2)
staxlab(3,at=c(1:ncol(exp.coef.mat.only.scores))-0.25,labels=paste0(colnames(exp.coef.mat.only.scores), "\n")
        ,srt=0, adj=1.2
        )
axis(3,at=c(ncol(exp.coef.mat.only.scores):1)-0.5, labels=rep("", ncol(exp.coef.mat.only.scores)))
# par(las=1)
axis(2,at=c(nrow(exp.coef.mat.only.scores):1)-0.5,labels=rownames(exp.coef.mat.only.scores),
     )

par(mar = c(5.1, 4.1, 4.1, 2.1))



######
## TRYING A SCORE DIFF ON Y-AXIS, LEAGUE NAME ON X-AXIS
## (so a FLIPPED version)
######

par(mar = c(2.1, 10, 4.0, 2.1))

color2D.matplot(t(exp.coef.mat.only.scores),
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)

par(las=1)
axis(2,at=c(ncol(exp.coef.mat.only.scores):1)-0.5,labels=colnames(exp.coef.mat.only.scores))

# par(las=2)
#staxlab(2,at=c(1:ncol(exp.coef.mat.only.scores))-0.5,labels=colnames(exp.coef.mat.only.scores))

#axis(2,at=c(ncol(exp.coef.mat.only.scores):1)-0.5, labels=rep("", ncol(exp.coef.mat.only.scores)))
# par(las=1)
axis(3,at=c(nrow(exp.coef.mat.only.scores):1)-0.5,labels=rownames(exp.coef.mat.only.scores),
)

par(mar = c(5.1, 4.1, 4.1, 2.1))










# ######
# ## THIRD, for WIN PROB DIFF
# #######
# 
# 
# library(mgcv)
# 
# 
# fit <- glm.extreme.obj[["Bundesliga"]] 
# 
# library(sjPlot)
# plot_model(fit, type = "pred", terms = "Weighted.Win.Prob")
# 
# # Set up smaller margins
# par(mar = c(2, 11, 2, 11))  # Adjust margins: bottom, left, top, right
# 
# # Set up the plotting area with multiple panels
# par(mfrow = c(5, 1))  # 5 rows, 1 column
# 
# 
# our.colors <- c("purple", "maroon", "black", "blue", "red")
# 
# plot(gam.extreme.obj[[league.name[2]]], all.terms=T)
# 
# for (j in 1:length(league.name)){
#   # plot(gam.extreme.obj[[league.name[j]]], select = 6, 
#   #      shade = TRUE, ylab = "ShotAttempt", col = our.colors[j] , main = league.name[j],
#   #      all.terms = TRUE
#   #     # , ylim = c(-0.2, 0.3)
#   #      )
#   # abline(v = c(-3:3), lty = "dotted")
#   # axis(1, at = -3:3, labels = -3:3)
#   
#   fit <- glm.extreme.obj[[league.name[j]]] 
#   
#   plot_model(fit, type = "pred", terms = "Weighted.Win.Prob", add=TRUE)
#   
# }
# 
# 
# 
# install.packages("margins")
# 
# library(margins)
# 
# # Calculate predicted values and marginal effects
# margins_model <- margins(glm.extreme.obj[["Bundesliga"]])
# 
# # Extract marginal effects for plotting
# margins_df <- summary(margins_model)
# 
# # Plot marginal effects using ggplot2
# ggplot(margins_df, aes(x = factor, y = AME)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
#   labs(x = "Predictors", y = "Average Marginal Effects",
#        title = "Marginal Effects of Predictors on Response Variable") +
#   theme_minimal()
# 
# 
# 
# 
# 
# 
# # Plot the first GAM model
# plot(gam.obj2_PremierLeague, select = 1, shade = TRUE, ylab = "ShotAttempt", col ="purple", main = "Premier League", ylim = c(-0.2, 0.3))
# abline(v = c(-3:3), lty = "dotted")
# axis(1, at = -3:3, labels = -3:3)
# 
# # Plot the second GAM model
# plot(gam.obj2_Bundesliga, select = 1, shade = TRUE, ylab = "ShotAttempt", col = "maroon", main = "Bundesliga", ylim = c(-0.2, 0.3))
# abline(v = c(-3:3), lty = "dotted")
# axis(1, at = -3:3, labels = -3:3)
# 
# # Plot the third GAM model
# plot(gam.obj2_LaLiga, select = 1, shade = TRUE, ylab = "ShotAttempt",col = "black", main = "La Liga", ylim = c(-0.2, 0.3))
# abline(v = c(-3:3), lty = "dotted")
# axis(1, at = -3:3, labels = -3:3)
# 
# # Plot the fourth GAM model
# plot(gam.obj2_Ligue1, select = 1, shade = TRUE, ylab = "ShotAttempt", col ="blue", main = "Ligue 1", ylim = c(-0.2, 0.3))
# abline(v = c(-3:3), lty = "dotted")
# axis(1, at = -3:3, labels = -3:3)
# 
# # Plot the fifth GAM model
# plot(gam.obj2, select = 1, shade = TRUE, ylab = "ShotAttempt", col = "red", main = "Serie A", ylim = c(-0.2, 0.3))
# abline(v = c(-3:3), lty = "dotted")
# axis(1, at = -3:3, labels = -3:3)
# 
# # Reset the plotting settings
# par(mfrow = c(1, 1))  # Reset to a single panel
# 
# 
# 
# # Create an empty plot
# plot(NULL, xlim = c(-3, 3), ylim = c(-1, 1), xlab = "X-Axis Label", ylab = "ShotAttempt")
# 
# # Plot smooths from each GAM model
# plot.gam(gam.obj2_PremierLeague, select = 1, col = "red", add = TRUE)
# plot.gam(gam.obj2_Bundesliga, select = 1, col = "blue", add = TRUE)
# plot.gam(gam.obj2_LaLiga, select = 1, col = "green", add = TRUE)
# plot.gam(gam.obj_Ligue1, select = 1, col = "orange", add = TRUE)
# 
# # Add vertical dotted lines
# abline(v = c(-3:3), lty = "dotted")
# 
# # Customize x-axis labels
# axis(1, at = -3:3, labels = -3:3)
























##############
##############
####  CORNERS
##############
##############



league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

extr.score <- c(1, 2, 3, 4, 5)[2]

glm.extreme.obj <- list()


for (league in league.name){
  
  print(league)
  
  # load("final_df_w_red_cards.csv")
  
  final.df <- read.csv(paste0(league, "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  #########
  ## Fitting the Neg Bin regression with:
  ##    * Score.Diff as DUMMY variables with "0" as reference level 
  ##      (achieved via mutate(Score.Diff = relevel(factor(Score.Diff), ref="0")))
  ##    * Merge the RIGHT-HAND extremes into MOST RIGHTHAND CATEGORY, while LEFT-HAND - into MOST LEFTHAND
  ##    * Making effects of Weighted.Win.Prob & RedCard.Diff linear
  #########
  
  
  glm.extreme.obj[[league]] <- glm.nb(Corners ~ Score.Diff + Weighted.Win.Prob  + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
                                      data=final.df %>% mutate(ID = factor(ID),
                                                               Score.Diff = relevel(factor(
                                                                 ifelse(Score.Diff >= extr.score,
                                                                        paste0(extr.score, ".or.better"),
                                                                        ifelse(Score.Diff <= -extr.score,
                                                                               paste0(-extr.score, ".or.worse"),
                                                                               Score.Diff))), ref="0")))
  glm.extreme.obj[[league]]
  
  
}

glm.extreme.obj[[1]]$coefficients



##########
#######
## FIRST, the DUMMY VARIABLE EFFECTS
#######
##########

#####
### SCORE DIFF DF CALCULATION
#####

coef.df <- NULL

for (league in league.name){
  coef.df <- rbind(coef.df,
                   data.frame(League = league, t(coefficients(glm.extreme.obj[[league]]))))
}

coef.df.scorediff <- coef.df[, c("League", colnames(coef.df)[str_detect(colnames(coef.df), "Score.Diff")])]
coef.df.scorediff$Score.Diff.0 <- 0

#read.csv("dummy_variable_coefficients Corners.csv")

# Rename columns for clarity
colnames(coef.df.scorediff) <- c('League', '-1', '-2 or worse', '+1',
                                 '+2 or better', '0')

# Transform the dataset to long format
df_long.scorediff <- coef.df.scorediff %>%
  pivot_longer(cols = -League, names_to = "scoreDiffCategory", values_to = "value")

## Making it an exp(value)
df_long.scorediff$value <- exp(df_long.scorediff$value)
#df_long$value <- df_long$value

# Define the desired order of score difference categories

df_long.scorediff$scoreDiffCategory <- factor(df_long.scorediff$scoreDiffCategory,
                                              ordered=T,
                                              levels=c('-2 or worse', '-1', 
                                                       '0', 
                                                       '+1', '+2 or better'))


# Create a new dataframe for segments
segments_data.scorediff <- df_long.scorediff %>%
  mutate(xmin = as.numeric(factor(scoreDiffCategory))-0.4 ,
         xmax = as.numeric(factor(scoreDiffCategory)) + 0.4,  # Adjust width of segments
         xmid = as.numeric(factor(scoreDiffCategory)),
         ymin = value,  # Start from 0 for y-axis
         ymid = value,
         ymax = value) %>%
  dplyr::select(xmin, xmid, xmax, ymin, ymid, ymax, League)



#####
### RED CARD DIFF DF CALCULATION
#####

coef.df.redcarddiff <- coef.df %>% dplyr::select(League, RedCard.Diff) 

for (j in c(-2:2)){
  coef.df.redcarddiff[, paste(j)] <- exp(j*coef.df.redcarddiff[, "RedCard.Diff"])
}
coef.df.redcarddiff[, "RedCard.Diff"] <- NULL


colnames(coef.df.redcarddiff) <- c('League', '-2', '-1', '0', '+1', '+2')


# Transform the dataset to long format
df_long.redcarddiff <- coef.df.redcarddiff %>%
  pivot_longer(cols = -League, names_to = "redcardDiffCategory", values_to = "value")

df_long.redcarddiff$redcardDiffCategory <- factor(df_long.redcarddiff$redcardDiffCategory,
                                                  ordered=T,
                                                  levels=c('-2', '-1', 
                                                           '0', 
                                                           '+1', '+2'))



# Create a new dataframe for segments
segments_data.redcarddiff <- df_long.redcarddiff %>%
  mutate(xmin = as.numeric(factor(redcardDiffCategory))-0.4 ,
         xmax = as.numeric(factor(redcardDiffCategory)) + 0.4,  # Adjust width of segments
         xmid = as.numeric(factor(redcardDiffCategory)),
         ymin = value,  # Start from 0 for y-axis
         ymid = value,
         ymax = value) %>%
  dplyr::select(xmin, xmid, xmax, ymin, ymid, ymax, League)


###
## Finding the common Y-axis limits across score & red card differentials
###

y_min <- min(df_long.scorediff$value, 
             df_long.redcarddiff$value)

y_max <- max(df_long.scorediff$value, 
             df_long.redcarddiff$value)





######
## PLOTTING for SCORE DIFF & RED CARD DIFF EFFECTS
######

score.diff.names <- c('-2 \n or worse', '-1', 
                      '0', 
                      '+1', '+2 \n or better')

# Create the geom_segment plot with horizontal lines
plot3 <- ggplot(segments_data.scorediff, aes(x = xmin, xend = xmax, y = ymin, yend = ymax, color = League)) +
  geom_segment(size = 2, alpha=0.7) +  # Add segments
  geom_line(aes(x=xmid, y=ymid), lwd=1) +
  scale_x_continuous(breaks = 1:length(levels(df_long.scorediff$scoreDiffCategory)), 
                     labels = score.diff.names
                     # labels = levels(df_long.scorediff$scoreDiffCategory)
                     ) +  # Use custom order and labels
  theme_minimal() +  # Use a minimal theme
  labs(#title = "Shot Coefficients by Score Differential and League",
    x = "Score Difference Category",
    y = "Multiplicative Effect on Corners") +
  ylim(y_min, y_max)
# theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.5))


redcard.diff.names <- c('-2\n', '-1', 
                        '0', 
                        '+1', '+2\n')

# Create the geom_segment plot with horizontal lines
plot4 <- ggplot(segments_data.redcarddiff, aes(x = xmin, xend = xmax, y = ymin, yend = ymax, color = League)) +
  geom_segment(size = 2, alpha=0.7) +  # Add segments
  geom_line(aes(x=xmid, y=ymid), lwd=1) +
  scale_x_continuous(breaks = 1:length(levels(df_long.redcarddiff$redcardDiffCategory)), 
                     # labels = levels(df_long.redcarddiff$redcardDiffCategory)
                     labels = redcard.diff.names
  ) +  # Use custom order and labels
  theme_minimal() +  # Use a minimal theme
  labs(#title = "Shot Coefficients by Red Card Differential and League",
    x = "Red Card Difference Category",
    y = "Multiplicative Effect on Corners")  +
  ylim(y_min, y_max)
# theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.5))


# CORNERS EFFECT PLOTS
# width: 672;  height: 442

library(ggpubr)
ggarrange(plot3, plot4, common.legend = T, legend="right")







#######
## VARIABLE SELECTION RESULTS
#######


load("Corners_stepwise_obj.Robj")

score.diff.names <- c(paste0("Score.Diff.",c(9:1)), paste0("Score.Diff",c(1:9)))
all.coef.names <- c(score.diff.names, "Weighted.Win.Prob", "HomeAwayHome", "RedCard.Diff")
stepwise.obj[["Bundesliga"]]

coef.matrix <- matrix(0, nrow=5, ncol=length(all.coef.names))
colnames(coef.matrix) <- all.coef.names
proper.league.names <- c("Bundesliga", "Serie A", "La Liga", "Ligue 1", "Premier League")
rownames(coef.matrix) <- proper.league.names

for (j in 1:length(league.name)){
  league.coefs <- stepwise.obj[[league.name[j]]]$coefficients[-1]
  coef.names.league <- names(league.coefs)
  coef.matrix[j, coef.names.league] <- league.coefs
}

print(coef.matrix)
coef.matrix <- cbind(coef.matrix[,1:9], Score.Diff0 = 0, coef.matrix[,-c(1:9)])
exp.coef.mat <- exp(coef.matrix)


exp.coef.mat.only.scores <- exp(coef.matrix[,1:19])
colnames(exp.coef.mat.only.scores) <- str_replace(str_remove(colnames(exp.coef.mat.only.scores), "Score.Diff"),
                                                  "\\.", "-")
colnames(exp.coef.mat.only.scores)[10] <- " 0"
colnames(exp.coef.mat.only.scores)[11:19] <- paste0("+", colnames(exp.coef.mat.only.scores)[11:19])


#######
## The actual plot
#######

colnames(exp.coef.mat)[1:19] <- c(paste0("Score Diff = ", colnames(exp.coef.mat.only.scores)))
colnames(exp.coef.mat)[20] <- "Weighted Win Prob"
colnames(exp.coef.mat)[21] <- "Home (vs Away)"
colnames(exp.coef.mat)[22] <- "Red Card Diff"


## width: 530; height: 424
par(mar = c(2.1, 10, 4.0, 2.1))

color2D.matplot(exp.coef.mat,
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)
par(las=2)
staxlab(3,at=c(1:ncol(exp.coef.mat))-0.25,labels=colnames(exp.coef.mat),srt=-45)
par(las=1)
axis(2,at=c(nrow(exp.coef.mat):1)-0.5,labels=rownames(exp.coef.mat))

par(mar = c(5.1, 4.1, 4.1, 2.1))


## FLIPPING IT
## !! LIKE THAT ONE WAY MORE !!
## SCREENSHOTTED into "Stepwise_Selection_Results_Everything.png"

par(mar = c(2.1, 10, 4.0, 2.1))

color2D.matplot(t(exp.coef.mat),
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)
par(las=1)
axis(2,at=c(ncol(exp.coef.mat):1)-0.5,labels=colnames(exp.coef.mat))
# staxlab(2,at=rev(c(1:ncol(exp.coef.mat)))-0.25,labels=colnames(exp.coef.mat))
par(las=1)
axis(3,at=c(nrow(exp.coef.mat):1)-0.5,labels=rownames(exp.coef.mat))

par(mar = c(5.1, 4.1, 4.1, 2.1))





#######
## VARIABLE SELECTION RESULTS
#######


load("Corners_stepwise_obj.Robj")

score.diff.names <- c(paste0("Score.Diff.",c(9:1)), paste0("Score.Diff",c(1:9)))
all.coef.names <- c(score.diff.names, "Weighted.Win.Prob", "HomeAwayHome", "RedCard.Diff")
stepwise.obj[["Bundesliga"]]

coef.matrix <- matrix(0, nrow=5, ncol=length(all.coef.names))
colnames(coef.matrix) <- all.coef.names
proper.league.names <- c("Bundesliga", "Serie A", "La Liga", "Ligue 1", "Premier League")
rownames(coef.matrix) <- proper.league.names

for (j in 1:length(league.name)){
  league.coefs <- stepwise.obj[[league.name[j]]]$coefficients[-1]
  coef.names.league <- names(league.coefs)
  coef.matrix[j, coef.names.league] <- league.coefs
}

print(coef.matrix)
coef.matrix <- cbind(coef.matrix[,1:9], Score.Diff0 = 0, coef.matrix[,-c(1:9)])
exp.coef.mat <- exp(coef.matrix)


exp.coef.mat.only.scores <- exp(coef.matrix[,1:19])
colnames(exp.coef.mat.only.scores) <- str_replace(str_remove(colnames(exp.coef.mat.only.scores), "Score.Diff"),
                                                  "\\.", "-")
colnames(exp.coef.mat.only.scores)[10] <- " 0"
colnames(exp.coef.mat.only.scores)[11:19] <- paste0("+", colnames(exp.coef.mat.only.scores)[11:19])


#######
## The actual plot
#######

colnames(exp.coef.mat)[1:19] <- c(paste0("Score Diff = ", colnames(exp.coef.mat.only.scores)))
colnames(exp.coef.mat)[20] <- "Weighted Win Prob"
colnames(exp.coef.mat)[21] <- "Home (vs Away)"
colnames(exp.coef.mat)[22] <- "Red Card Diff"


## width: 530; height: 424
par(mar = c(2.1, 10, 4.0, 2.1))

color2D.matplot(exp.coef.mat,
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)
par(las=2)
staxlab(3,at=c(1:ncol(exp.coef.mat))-0.25,labels=colnames(exp.coef.mat),srt=-45)
par(las=1)
axis(2,at=c(nrow(exp.coef.mat):1)-0.5,labels=rownames(exp.coef.mat))

par(mar = c(5.1, 4.1, 4.1, 2.1))


## FLIPPING IT
## !! LIKE THAT ONE WAY MORE !!
## SCREENSHOTTED into "Stepwise_Selection_Results_Everything.png"

par(mar = c(2.1, 10, 4.0, 2.1))

color2D.matplot(t(exp.coef.mat),
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)
par(las=1)
axis(2,at=c(ncol(exp.coef.mat):1)-0.5,labels=colnames(exp.coef.mat))
# staxlab(2,at=rev(c(1:ncol(exp.coef.mat)))-0.25,labels=colnames(exp.coef.mat))
par(las=1)
axis(3,at=c(nrow(exp.coef.mat):1)-0.5,labels=rownames(exp.coef.mat))

par(mar = c(5.1, 4.1, 4.1, 2.1))





#####
## JUST THE SCORE DIFFERENTIALS
#####

# Imported into: "Stepwise_Selection_ScoreDiff_Only_Wide_Format.png", with
# width: 947;  height: 463

par(mar = c(2.1, 10, 4.0, 2.1))

color2D.matplot(exp.coef.mat.only.scores,
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)
par(las=2)
staxlab(3,at=c(1:ncol(exp.coef.mat.only.scores))-0.25,labels=paste0(colnames(exp.coef.mat.only.scores), "\n")
        ,srt=0, adj=1.2
)
axis(3,at=c(ncol(exp.coef.mat.only.scores):1)-0.5, labels=rep("", ncol(exp.coef.mat.only.scores)))
# par(las=1)
axis(2,at=c(nrow(exp.coef.mat.only.scores):1)-0.5,labels=rownames(exp.coef.mat.only.scores),
)

par(mar = c(5.1, 4.1, 4.1, 2.1))



######
## TRYING A SCORE DIFF ON Y-AXIS, LEAGUE NAME ON X-AXIS
## (so a FLIPPED version)
######

par(mar = c(2.1, 10, 4.0, 2.1))

color2D.matplot(t(exp.coef.mat.only.scores),
                cs1=c(1,0),cs2=c(0,1),cs3=c(0,1),
                show.legend=F,
                show.values=2,
                xlab='',
                ylab='',
                axes=F)

par(las=1)
axis(2,at=c(ncol(exp.coef.mat.only.scores):1)-0.5,labels=colnames(exp.coef.mat.only.scores))

# par(las=2)
#staxlab(2,at=c(1:ncol(exp.coef.mat.only.scores))-0.5,labels=colnames(exp.coef.mat.only.scores))

#axis(2,at=c(ncol(exp.coef.mat.only.scores):1)-0.5, labels=rep("", ncol(exp.coef.mat.only.scores)))
# par(las=1)
axis(3,at=c(nrow(exp.coef.mat.only.scores):1)-0.5,labels=rownames(exp.coef.mat.only.scores),
)

par(mar = c(5.1, 4.1, 4.1, 2.1))







##############
##############
#######
####### COMBINING THE SHOTS & CORNERS EFFECT PLOTS
#######
##############
##############


ggarrange(plot1, plot2, plot3, plot4, 
          ncol=2, nrow=2,
          common.legend = T, legend="right")
