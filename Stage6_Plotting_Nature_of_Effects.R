######
######

library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


load(file="gam_ziP_obj_Shots.Robj")

for (league in league.name){
  
  print(league)
  
  plot.gam(gam.ziP.obj[[league]], pages=1)
  
}

#plot.gam(gam.ziP.obj[[league]])
# abline(v=c(min(our.df.cut$Score.Diff): max(our.df.cut$Score.Diff)))
