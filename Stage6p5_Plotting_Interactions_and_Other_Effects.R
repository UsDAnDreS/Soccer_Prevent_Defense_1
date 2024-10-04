######
## !!! GOTTA CHECK THE BIC vs NO INTERACTION MODEL !!!
######

library(mgcv)
library(tidyverse)
library(splines)
library(MASS)
library(DHARMa)


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


load(file="gam_ziP_obj_INTERACTIONS_TE_VERSION_Shots.Robj")



for (league in league.name){
  
  print(league)
  
#  plot.gam(gam.ziP.obj[[league]], pages=1)
  
  
  
  # reduced.gam.ziP.obj <- gam.ziP.obj[[league]]
  # 
  # ind.needed <- which(abs(reduced.gam.ziP.obj$model$Score.Diff) <=3)
  # reduced.gam.ziP.obj$model <-  reduced.gam.ziP.obj$model[ind.needed, ]
  # reduced.gam.ziP.obj$y <-  reduced.gam.ziP.obj$y[ind.needed]
  # reduced.gam.ziP.obj$z <-  reduced.gam.ziP.obj$z[ind.needed]
  # reduced.gam.ziP.obj$fitted.values <-  reduced.gam.ziP.obj$fitted.values[ind.needed]
  # reduced.gam.ziP.obj$linear.predictors <-  reduced.gam.ziP.obj$linear.predictors[ind.needed]
  # 
  
  
  vis.gam(#gam.ziP.obj[[league]], view = c("Score.Diff", "Minute.clean"),
    gam.ziP.obj[[league]], view = c("Score.Diff", "Minute.clean"), se=T,
          theta = 100, n.grid = 50, lwd = 0.4, 
         # xlim=c(-3,3), too.far=T
          )
  
  
  ## For the SLOPE COMPARISON PLOTS (-3:3, 45 vs 70 vs 90)
  score.range <- 3
  score.diff.vals <- c(-score.range:score.range); minute.clean.vals <- c(45,70,90)
  
  ## For the overall 3D plot
  score.range <- 8
  score.diff.vals <- c(-score.range:score.range); minute.clean.vals <- c(0:90)
  
  
  
  exp.gr.score.min <- expand.grid(Score.Diff = score.diff.vals, Minute.clean = minute.clean.vals)
  our.pred <- predict(gam.ziP.obj[[league]], newdata=data.frame(HomeAway = "Home", RedCard.Diff = 0, Weighted.Win.Prob = 0,
                                                  Score.Diff = exp.gr.score.min$Score.Diff,
                                                  Minute.clean = exp.gr.score.min$Minute.clean)
                     # , se.fit=T
                      )
  
  
  our.pred.mat <- matrix(our.pred, 
                         nrow = length(score.diff.vals), ncol = length(minute.clean.vals))
  
  library(graphics)
  
  library(rgl)
  persp(x=score.diff.vals,
        y=minute.clean.vals,
        z=our.pred.mat,
        theta=0, phi=10)
  
  ### WITHOUT BANDS
  
  plot3d(exp.gr.score.min$Score.Diff,
         exp.gr.score.min$Minute.clean,
         our.pred)
  
  ## WITH BANDS
  
  our.pred <- predict(gam.ziP.obj[[league]], newdata=data.frame(HomeAway = "Home", RedCard.Diff = 0, Weighted.Win.Prob = 0,
                                                                Score.Diff = exp.gr.score.min$Score.Diff,
                                                                Minute.clean = exp.gr.score.min$Minute.clean)
                       , se.fit=T
  )
  
  se.mat <- matrix(our.pred$se.fit, 
                   nrow = length(score.diff.vals), ncol = length(minute.clean.vals))
  
  
  zmin <- min(our.pred$fit - 2*our.pred$se.fit)
  zmax <- max(our.pred$fit + 2*our.pred$se.fit)
  
  # Prediction
  plot3d(exp.gr.score.min$Score.Diff,
         exp.gr.score.min$Minute.clean,
         our.pred$fit,
         zmin = zmin, zmax = zmax)
  
  # Lower band (prediction - 2*se)
  plot3d(exp.gr.score.min$Score.Diff,
         exp.gr.score.min$Minute.clean,
         our.pred$fit - 2*our.pred$se.fit,
         zmin = zmin, zmax = zmax,
         add=T, col="red")
  
  # Upper band (prediction + 2*se)
  plot3d(exp.gr.score.min$Score.Diff,
         exp.gr.score.min$Minute.clean,
         our.pred$fit + 2*our.pred$se.fit,
         zmin = zmin, zmax = zmax,
         add=T, col="green")
  
  
  
  
  #####
  ## Getting the 2D PLOTS of SCORE DIFFERENTIAL EFFECT DIFFERENCE,
  ## based on WHEN IN THE GAME WE ARE...
  ##
  ## Get the SCORE DIFFERENTIAL PLOTS at:
  ##    * 90th minute, vs
  ##    * 80th minute
  ##    * 70th minute, vs
  ##    * 45th min
  ##
  ##  Way too wide from about -4:4 onward in case of 25min
  ##
  ## DO: -3:3, for 45 onwards (where we have MOST TIGHT CONFIDENCE BANDS)
  #####
  
  ymin <- min(our.pred.mat - 2*se.mat); ymax <- max(our.pred.mat + 2*se.mat)
  
  plot(score.diff.vals, our.pred.mat[,1], ylim=c(ymin, ymax), type="l")
  lines(score.diff.vals, our.pred.mat[,1] + 2*se.mat[,1], ylim=c(ymin, ymax), type="l", lty=2)
  lines(score.diff.vals, our.pred.mat[,1] - 2*se.mat[,1], ylim=c(ymin, ymax), type="l", lty=2)
  
  for (j in 2:ncol(our.pred.mat)){
    lines(score.diff.vals, our.pred.mat[,j], ylim=c(ymin, ymax), type="l", col=j)
    lines(score.diff.vals, our.pred.mat[,j] + 2*se.mat[,j], ylim=c(ymin, ymax), type="l", lty=2, col=j)
    lines(score.diff.vals, our.pred.mat[,j] - 2*se.mat[,j], ylim=c(ymin, ymax), type="l", lty=2, col=j)
  }
  
  legend("topright", legend = minute.clean.vals, col=c(1:length(minute.clean.vals)), lty=1)
  
  
  
  
  vis.gam(
    gam.ziP.obj[[league]], view = c("Score.Diff"), se=T,
    theta = 100, n.grid = 50, lwd = 0.4, 
  )
  
  plot(gam.ziP.obj[[league]])
  
}

plot.gam(gam.ziP.obj[[league]])
# abline(v=c(min(our.df.cut$Score.Diff): max(our.df.cut$Score.Diff)))
