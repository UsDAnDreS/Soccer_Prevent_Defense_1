library(DHARMa)


## Whether to remove extra time, to align with minute-by-minute approach
## (plus take care of the issue where events get stacked up on 90th minute, instead of going into 91+)
remove.extra <- TRUE

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")


for (league in league.name){
  
  print(league)
  
  our.df.cut <- read.csv(paste0(league, ifelse(remove.extra, "_NO_EXTRA_TIME", ""), "_Sami_final_df_w_red_cards_WITH_BOOKING_ODDS.csv"))
  
  
  ## REMOVING NA betting data
  our.df.cut <- our.df.cut %>% filter(!is.na(Weighted.Win.Prob))
  #dim(our.df)
  
  
  ############
  ############
  
  our.df.cut  <- our.df.cut  %>%
    mutate(abs.Score.Diff = abs(Score.Diff),
           abs.RedCard.Diff = abs(RedCard.Diff))
  
  our.df.cut  <- our.df.cut  %>%
    mutate(Period.ID = group_indices(our.df.cut, .dots=c("ID", "abs.Score.Diff", "abs.RedCard.Diff"))) %>%
    arrange(Period.ID) %>%
    mutate(Period.ID = factor(Period.ID))
  
  our.df.cut <- our.df.cut %>% mutate(Period.ID = factor(Period.ID), ID = factor(ID))
  
  
  
  
  
  ######
  ## Fitting SPLINE models
  ######
  
  gam.nb.obj <- gam(Shots ~ 
                                #Corners ~
                                s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5) + log(minutes.spent + 1),
                              family = "nb", data= our.df.cut)
  
  gam.nb.obj.not.offset <- gam(Shots ~ 
                      #Corners ~
                      s(Score.Diff) + s(Weighted.Win.Prob)  + HomeAway + s(RedCard.Diff, k=5) + s(log(minutes.spent+1)),
                    family = "nb", data= our.df.cut)
  
plot(gam.nb.obj, pages=1)
plot(gam.nb.obj.not.offset, pages=1)


  # save(gam.obj, file="gam_obj_Corners.Robj")
  # save(gam.nb.obj, file="gam_nb_obj_Corners.Robj")
  # save(gam.ziP.obj, file="gam_ziP_obj_Corners.Robj")
  
  
  print(league)
  print(BIC(#glm.obj[[league]], glm.nb.obj[[league]], glm.ziP.obj[[league]],
    gam.obj[[league]], gam.nb.obj[[league]], gam.ziP.obj[[league]]))
  
  cat("\n")
}