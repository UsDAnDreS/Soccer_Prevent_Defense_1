library(mgcv)
library(tidyverse)
library(splines)
library(MASS)

#-------------------------------------------------------------

setwd("C:/Users/ascem/Desktop/Prof. Andrey Code/Dummy_Var_Selection_All_Leagues")

df <- read.csv("LaLiga_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv")

# Define a function to categorize scoreDiff
categorize_score_diff <- function(scoreDiff) {
  if (scoreDiff >= 4) {
    return("4 or more")
  } else if (scoreDiff <= -4) {
    return("-4 or less")
  } else {
    return(as.character(scoreDiff))  # Ensure it's character for consistent type
  }
}

# Apply the function using mutate to create the scoreDiffCategory column
final.df <- df %>%
  mutate(scoreDiffCategory = sapply(Score.Diff, categorize_score_diff))


#-------------------------------------------------------------

# Set the working directory
#setwd("C:/Users/ascem/Desktop/Prof. Andrey Code/Dummy_Var_Selection_All_Leagues")

# List of league names
#league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

# Loop through each league and read the corresponding CSV file
#for (league in league.name) {
#  file_path <- paste0(league, "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv")
  
#  if (file.exists(file_path)) {
#    print(paste("Processing file for league:", league))
#    final.df <- read.csv(file_path)
#  } else {
#    warning(paste("File not found for league:", league))
#  }
#}

#-------------------------------------------------------------

#########
## Fitting the Neg Bin regression with:
##    * Score.Diff as DUMMY variables with "0" as reference level 
##      (achieved via mutate(Score.Diff = relevel(factor(Score.Diff), ref="0")))
##    * Making effects of Weighted.Win.Prob & RedCard.Diff linear
#########

# Ensure scoreDiffCategory is treated as a factor and set reference level
final.df <- final.df %>%
  mutate(ID = factor(ID),
         scoreDiffCategory = factor(scoreDiffCategory, levels = c("0", "1", "-1", "2", "-2", "3", "-3", "4 or more", "-4 or less")))

# SHOTS
glm.no.int.offset.obj <- glm.nb(Shots ~ scoreDiffCategory + Weighted.Win.Prob + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
                                data=final.df)
glm.no.int.offset.obj

#CORNERS
glm.no.int.offset.obj2 <- glm.nb(Corners ~ scoreDiffCategory + Weighted.Win.Prob + HomeAway + RedCard.Diff + offset(log(minutes.spent+1)),
                                data=final.df)
glm.no.int.offset.obj2


## Extracting the design matrix with dummy variables being treated as separate predictors
# X <- model.matrix(glm.no.int.offset.obj)[,-1]
# y <- glm.no.int.offset.obj$y

## Fitting the negative binomial
# glm.model.mat <- glm.nb(y ~ . + offset(glm.no.int.offset.obj$offset), data=data.frame(X))
# glm.model.mat

## Using stepwise selection with BIC as the criteria (leads to a simpler model than AIC)
# step.obj <- step(glm.model.mat, k=log(length(y)), trace=TRUE)
# print(step.obj)


#__________________________________________________
# Prof. Andrey Version


## BUNDESLIGA
## Score Diff: -3 to 4

# Coefficients:
#   (Intercept)       Score.Diff.3       Score.Diff.2       Score.Diff.1        Score.Diff1        Score.Diff2  
# -2.09382            0.16082            0.20890            0.16994           -0.07412           -0.11785  
# Score.Diff3        Score.Diff4  Weighted.Win.Prob       HomeAwayHome       RedCard.Diff  
# -0.14266           -0.15025            0.56731            0.04749           -0.40008  


# Df Deviance    AIC
# <none>                    30330 110680
# - Score.Diff4        1    30345 110685
# - Score.Diff3        1    30371 110711
# - HomeAwayHome       1    30373 110713
# - Score.Diff.3       1    30376 110716
# - Score.Diff1        1    30391 110731
# - Score.Diff2        1    30402 110742
# - Score.Diff.2       1    30555 110895
# - Score.Diff.1       1    30664 111004
# - RedCard.Diff       1    31082 111422
# - Weighted.Win.Prob  1    33635 113975


## SERIE A
## Score Diff: -5 to 5

# Coefficients:
#   (Intercept)       Score.Diff.5       Score.Diff.4       Score.Diff.3       Score.Diff.2       Score.Diff.1  
# -2.06004            0.30190            0.27173            0.31066            0.33333            0.19058  
# Score.Diff1        Score.Diff2        Score.Diff3        Score.Diff4        Score.Diff5  Weighted.Win.Prob  
# -0.19443           -0.24882           -0.29074           -0.28140           -0.34793            0.62411  
# HomeAwayHome       RedCard.Diff  
# 0.03582           -0.38494  


# Df Deviance    AIC
# <none>                    34749 128146
# - Score.Diff.5       1    34761 128146
# - Score.Diff5        1    34767 128153
# - HomeAwayHome       1    34776 128162
# - Score.Diff.4       1    34783 128169
# - Score.Diff4        1    34792 128178
# - Score.Diff3        1    34918 128304
# - Score.Diff.3       1    34933 128318
# - Score.Diff2        1    35073 128458
# - Score.Diff1        1    35215 128601
# - Score.Diff.1       1    35231 128617
# - Score.Diff.2       1    35371 128757
# - RedCard.Diff       1    35886 129271
# - Weighted.Win.Prob  1    39220 132605




## LA LIGA
##  Score Diff: -5 to 5

# Coefficients:
#   (Intercept)       Score.Diff.5       Score.Diff.4       Score.Diff.3       Score.Diff.2       Score.Diff.1  
# -2.16644            0.33807            0.35795            0.32540            0.29659            0.18608  
# Score.Diff1        Score.Diff2        Score.Diff3        Score.Diff4        Score.Diff5  Weighted.Win.Prob  
# -0.17650           -0.21583           -0.19596           -0.22043           -0.26021            0.54333  
# HomeAwayHome       RedCard.Diff  
# 0.09178           -0.39427


# Df Deviance    AIC
# <none>                    31437 114228
# - Score.Diff5        1    31450 114231
# - Score.Diff.5       1    31454 114234
# - Score.Diff4        1    31468 114248
# - Score.Diff.4       1    31501 114281
# - Score.Diff3        1    31506 114286
# - HomeAwayHome       1    31591 114371
# - Score.Diff.3       1    31603 114384
# - Score.Diff2        1    31658 114439
# - Score.Diff1        1    31777 114557
# - Score.Diff.1       1    31841 114622
# - Score.Diff.2       1    31863 114643
# - RedCard.Diff       1    32668 115448
# - Weighted.Win.Prob  1    34642 117422




## League 1
##    Score Diff: -4 through 4

# Coefficients:
#   (Intercept)       Score.Diff.4       Score.Diff.3       Score.Diff.2       Score.Diff.1        Score.Diff1  
# -2.1601             0.2600             0.2774             0.2207             0.1635            -0.1701  
# Score.Diff2        Score.Diff3        Score.Diff4  Weighted.Win.Prob       HomeAwayHome       RedCard.Diff  
# -0.1648            -0.1994            -0.2438             0.5150             0.0658            -0.3980 

# Df Deviance    AIC
# <none>                    35045 126333
# - Score.Diff.4       1    35075 126352
# - Score.Diff4        1    35076 126353
# - Score.Diff3        1    35111 126388
# - HomeAwayHome       1    35129 126406
# - Score.Diff.3       1    35169 126446
# - Score.Diff2        1    35183 126460
# - Score.Diff.2       1    35296 126573
# - Score.Diff.1       1    35404 126681
# - Score.Diff1        1    35404 126681
# - RedCard.Diff       1    36463 127740
# - Weighted.Win.Prob  1    37444 128721



## PREMIER LEAGUE
##    Score Diff: -4 through 4

# Coefficients:
#   (Intercept)       Score.Diff.4       Score.Diff.3       Score.Diff.2       Score.Diff.1        Score.Diff1  
# -2.12909            0.22518            0.23802            0.22147            0.17431           -0.14951  
# Score.Diff2        Score.Diff3        Score.Diff4  Weighted.Win.Prob       HomeAwayHome       RedCard.Diff  
# -0.16237           -0.22161           -0.20932            0.67302            0.05202           -0.49241  



#####
## -3 THROUGH 4 is the LOWEST COMMON DENOMINATOR;
##  -4 THROUGH 4 IS THE MOST COMMON SYMMETRIC PRESENCE (4 out of 5 leagues)
#####


#__________________________________________________
# SAMI Version - Shots

# Bundesliga

#Call:  glm.nb(formula = y ~ scoreDiffCategory1 + scoreDiffCategory.1 + 
#                scoreDiffCategory2 + scoreDiffCategory.2 + scoreDiffCategory3 + 
#                scoreDiffCategory.3 + scoreDiffCategory4.or.more + Weighted.Win.Prob + 
#               HomeAwayHome + RedCard.Diff + offset(glm.no.int.offset.obj$offset), 
#              data = data.frame(X), init.theta = 21.25746292, link = log)

#Coefficients:
#  (Intercept)          scoreDiffCategory1         scoreDiffCategory.1  
#-2.09223                    -0.07623                     0.16849  
#scoreDiffCategory2         scoreDiffCategory.2          scoreDiffCategory3  
#-0.12027                     0.20774                    -0.14531  
#scoreDiffCategory.3  scoreDiffCategory4.or.more           Weighted.Win.Prob  
#0.15991                    -0.16714                     0.57018  
#HomeAwayHome                RedCard.Diff  
#0.04746                    -0.40144  

#Degrees of Freedom: 28377 Total (i.e. Null);  28367 Residual
#Null Deviance:	    35420 
#Residual Deviance: 30320 	AIC: 110600

# Serie A

#Call:  glm.nb(formula = Shots ~ scoreDiffCategory + Weighted.Win.Prob + 
#                HomeAway + RedCard.Diff + offset(log(minutes.spent + 1)), 
#              data = final.df, init.theta = 17.73190343, link = log)

#Coefficients:
#  (Intercept)           scoreDiffCategory1          scoreDiffCategory-1  
#-2.05951                     -0.19518                      0.19014  
#scoreDiffCategory2          scoreDiffCategory-2           scoreDiffCategory3  
#-0.24970                      0.33302                     -0.29176  
#scoreDiffCategory-3   scoreDiffCategory4 or more  scoreDiffCategory-4 or less  
#0.31047                     -0.31665                      0.26670  
#Weighted.Win.Prob                 HomeAwayHome                 RedCard.Diff  
#0.62500                      0.03577                     -0.38618  

#Degrees of Freedom: 32199 Total (i.e. Null);  32188 Residual
#(2358 observations deleted due to missingness)
#Null Deviance:	    41970 
#Residual Deviance: 34740 	AIC: 128000

# Ligue 1

#Call:  glm.nb(formula = Shots ~ scoreDiffCategory + Weighted.Win.Prob + 
#                HomeAway + RedCard.Diff + offset(log(minutes.spent + 1)), 
#              data = final.df, init.theta = 20.38905168, link = log)

#Coefficients:
#  (Intercept)           scoreDiffCategory1          scoreDiffCategory-1  
#-2.15911                     -0.17124                      0.16270  
#scoreDiffCategory2          scoreDiffCategory-2           scoreDiffCategory3  
#-0.16614                      0.22009                     -0.20090  
#scoreDiffCategory-3   scoreDiffCategory4 or more  scoreDiffCategory-4 or less  
#0.27690                     -0.22560                      0.18597  
#Weighted.Win.Prob                 HomeAwayHome                 RedCard.Diff  
#0.51654                      0.06563                     -0.39882  

#Degrees of Freedom: 32563 Total (i.e. Null);  32552 Residual
#(118 observations deleted due to missingness)
#Null Deviance:	    40490 
#Residual Deviance: 35050 	AIC: 126200

# Premier League

#Call:  glm.nb(formula = Shots ~ scoreDiffCategory + Weighted.Win.Prob + 
#                HomeAway + RedCard.Diff + offset(log(minutes.spent + 1)), 
#              data = final.df, init.theta = 11.71926963, link = log)

#Coefficients:
#  (Intercept)           scoreDiffCategory1          scoreDiffCategory-1  
#-2.12826                     -0.15105                      0.17355  
#scoreDiffCategory2          scoreDiffCategory-2           scoreDiffCategory3  
#-0.16425                      0.22103                     -0.22376  
#scoreDiffCategory-3   scoreDiffCategory4 or more  scoreDiffCategory-4 or less  
#0.23789                     -0.19027                      0.17450  
#Weighted.Win.Prob                 HomeAwayHome                 RedCard.Diff  
#0.67537                      0.05223                     -0.49497  

#Degrees of Freedom: 27961 Total (i.e. Null);  27950 Residual
#(206 observations deleted due to missingness)
#Null Deviance:	    36980 
#Residual Deviance: 30350 	AIC: 113600

# La Liga

#Call:  glm.nb(formula = Shots ~ scoreDiffCategory + Weighted.Win.Prob + 
#                HomeAway + RedCard.Diff + offset(log(minutes.spent + 1)), 
#              data = final.df, init.theta = 20.6864762, link = log)

#Coefficients:
#  (Intercept)           scoreDiffCategory1          scoreDiffCategory-1  
#-2.16629                     -0.17696                      0.18613  
#scoreDiffCategory2          scoreDiffCategory-2           scoreDiffCategory3  
#-0.21651                      0.29688                     -0.19686  
#scoreDiffCategory-3   scoreDiffCategory4 or more  scoreDiffCategory-4 or less  
#0.32592                     -0.22942                      0.34579  
#Weighted.Win.Prob                 HomeAwayHome                 RedCard.Diff  
#0.54511                      0.09163                     -0.39469  

#Degrees of Freedom: 29933 Total (i.e. Null);  29922 Residual
#(4638 observations deleted due to missingness)
#Null Deviance:	    37660 
#Residual Deviance: 31440 	AIC: 114100


#__________________________________________________

# SAMI Version - Corners

# Bundesliga

#Call:  glm.nb(formula = Corners ~ scoreDiffCategory + Weighted.Win.Prob + 
#                HomeAway + RedCard.Diff + offset(log(minutes.spent + 1)), 
#              data = final.df, init.theta = 11.2050692, link = log)

#Coefficients:
#  (Intercept)           scoreDiffCategory1          scoreDiffCategory-1  
#-3.04921                     -0.17475                      0.19476  
#scoreDiffCategory2          scoreDiffCategory-2           scoreDiffCategory3  
#-0.28578                      0.25830                     -0.28512  
#scoreDiffCategory-3   scoreDiffCategory4 or more  scoreDiffCategory-4 or less  
#0.15681                     -0.28218                     -0.10310  
#Weighted.Win.Prob                 HomeAwayHome                 RedCard.Diff  
#0.64086                      0.04616                     -0.36489  

#Degrees of Freedom: 28377 Total (i.e. Null);  28366 Residual
#(224 observations deleted due to missingness)
#Null Deviance:	    31350 
#Residual Deviance: 28760 	AIC: 77530

# Serie A

#Call:  glm.nb(formula = Corners ~ scoreDiffCategory + Weighted.Win.Prob + 
#                HomeAway + RedCard.Diff + offset(log(minutes.spent + 1)), 
#              data = final.df, init.theta = 10.32392587, link = log)

#Coefficients:
#  (Intercept)           scoreDiffCategory1          scoreDiffCategory-1  
#-2.99582                     -0.30457                      0.24178  
#scoreDiffCategory2          scoreDiffCategory-2           scoreDiffCategory3  
#-0.45265                      0.32648                     -0.49719  
#scoreDiffCategory-3   scoreDiffCategory4 or more  scoreDiffCategory-4 or less  
#0.35486                     -0.58851                      0.12545  
#Weighted.Win.Prob                 HomeAwayHome                 RedCard.Diff  
#0.65318                      0.05782                     -0.32380  

#Degrees of Freedom: 32199 Total (i.e. Null);  32188 Residual
#(2358 observations deleted due to missingness)
#Null Deviance:	    36840 
#Residual Deviance: 33080 	AIC: 90840

# Ligue1

#Call:  glm.nb(formula = Corners ~ scoreDiffCategory + Weighted.Win.Prob + 
#                HomeAway + RedCard.Diff + offset(log(minutes.spent + 1)), 
#              data = final.df, init.theta = 10.64189947, link = log)

#Coefficients:
#  (Intercept)           scoreDiffCategory1          scoreDiffCategory-1  
#-3.08054                     -0.29589                      0.22304  
#scoreDiffCategory2          scoreDiffCategory-2           scoreDiffCategory3  
#-0.37715                      0.28651                     -0.46319  
#scoreDiffCategory-3   scoreDiffCategory4 or more  scoreDiffCategory-4 or less  
#0.31194                     -0.31597                      0.13496  
#Weighted.Win.Prob                 HomeAwayHome                 RedCard.Diff  
#0.55846                      0.09274                     -0.35506  

#Degrees of Freedom: 32563 Total (i.e. Null);  32552 Residual
#(118 observations deleted due to missingness)
#Null Deviance:	    36220 
#Residual Deviance: 33160 	AIC: 90110

# Premier League

#Call:  glm.nb(formula = Corners ~ scoreDiffCategory + Weighted.Win.Prob + 
#                HomeAway + RedCard.Diff + offset(log(minutes.spent + 1)), 
#              data = final.df, init.theta = 8.419302996, link = log)

#Coefficients:
#  (Intercept)           scoreDiffCategory1          scoreDiffCategory-1  
#-3.01625                     -0.23770                      0.21864  
#scoreDiffCategory2          scoreDiffCategory-2           scoreDiffCategory3  
#-0.26810                      0.25541                     -0.36552  
#scoreDiffCategory-3   scoreDiffCategory4 or more  scoreDiffCategory-4 or less  
#0.25124                     -0.29730                      0.17594  
#Weighted.Win.Prob                 HomeAwayHome                 RedCard.Diff  
#0.68993                      0.06004                     -0.42388  

#Degrees of Freedom: 27961 Total (i.e. Null);  27950 Residual
#(206 observations deleted due to missingness)
#Null Deviance:	    32300 
#Residual Deviance: 28960 	AIC: 81650


# La Liga

#Call:  glm.nb(formula = Corners ~ scoreDiffCategory + Weighted.Win.Prob + 
#                HomeAway + RedCard.Diff + offset(log(minutes.spent + 1)), 
#              data = final.df, init.theta = 11.25870889, link = log)

#Coefficients:
#  (Intercept)           scoreDiffCategory1          scoreDiffCategory-1  
#-3.0524                      -0.2967                       0.2332  
#scoreDiffCategory2          scoreDiffCategory-2           scoreDiffCategory3  
#-0.4257                       0.3276                      -0.4599  
#scoreDiffCategory-3   scoreDiffCategory4 or more  scoreDiffCategory-4 or less  
#0.3058                      -0.3672                       0.3235  
#Weighted.Win.Prob                 HomeAwayHome                 RedCard.Diff  
#0.5616                       0.1379                      -0.3374  

#Degrees of Freedom: 29933 Total (i.e. Null);  29922 Residual
#(4638 observations deleted due to missingness)
#Null Deviance:	    33750 
#Residual Deviance: 30410 	AIC: 82350


#__________________________________________________

## Dummy variable coefficient PLOT (adjust the file name for "Corners" or "Shots")

library(ggplot2)
library(tidyr)
library(dplyr)

df <- read.csv("dummy_variable_coefficients Corners.csv")

# Rename columns for clarity
colnames(df) <- c('League', 'scoreDiff0', 'scoreDiff1', 'scoreDiff-1',
                  'scoreDiff2', 'scoreDiff-2', 'scoreDiff3', 'scoreDiff-3',
                  'scoreDiff4 or more', 'scoreDiff4 or less')

# Transform the dataset to long format
df_long <- df %>%
  pivot_longer(cols = -league_name, names_to = "scoreDiffCategory", values_to = "value")

# Define the desired order of score difference categories
desired_order <- c('scoreDiff4 or less', 'scoreDiff-3', 'scoreDiff-2', 'scoreDiff-1',
                   'scoreDiff0', 'scoreDiff1', 'scoreDiff2', 'scoreDiff3', 'scoreDiff4 or more')

# Convert scoreDiffCategory to factor with desired order
df_long$scoreDiffCategory <- factor(df_long$scoreDiffCategory, levels = desired_order)

# Create a new dataframe for segments
segments_data <- df_long %>%
  mutate(xmin = as.numeric(factor(scoreDiffCategory)),
         xmax = as.numeric(factor(scoreDiffCategory)) + 0.9,  # Adjust width of segments
         ymin = 0,  # Start from 0 for y-axis
         ymax = value) %>%
  select(xmin, xmax, ymin, ymax, league_name)

# Create the geom_segment plot with horizontal lines
ggplot(segments_data, aes(x = xmin, xend = xmax, y = ymin, yend = ymax, color = league_name)) +
  geom_segment(size = 2) +  # Add segments
  scale_x_continuous(breaks = 1:length(desired_order), labels = desired_order) +  # Use custom order and labels
  theme_minimal() +  # Use a minimal theme
  labs(title = "Corner Coefficients by League",
       x = "Score Difference Category",
       y = "Corner Coefficients") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#__________________________________________________
