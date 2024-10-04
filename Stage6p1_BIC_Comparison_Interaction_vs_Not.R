load(file="gam_ziP_obj_INTERACTIONS_Shots.Robj")
gam.ziP.obj_w_int <- gam.ziP.obj

load(file="gam_ziP_obj_Shots.Robj")


league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")

for (league in league.name){
  print(league)
  print(AIC(gam.ziP.obj[[league]], gam.ziP.obj_w_int[[league]]))
}


####
## CONCLUSION:
##  * INTERACTION NOT WORTH IT... all BICs/AICs are WORSE FOR INTERACTION... BY NOTABLE MARGIN
##    (Regardless of whether it was TE or TI version of interactions)
####