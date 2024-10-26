## UNIT DEVIANCE FOR POISSON DISTRIBUTION:
# https://en.wikipedia.org/wiki/Deviance_%28statistics%29
#
# d(y,mu) = 2*(y*log(y/mu) - y + mu)

library(tidyverse)

# "NO_MINUTE", "NO_SCOREDIFF", "NO_REDCARDDIFF", "NO_WINPROBDIFF", "NO_HOMEAWAY", 
# "SCOREDIFF_MINUTE_INT", "REDCARDDIFF_MINUTE_INT", "SCOREDIFF_REDCARDDIFF_INT", 
# "WINPROBEDIFF_SCOREDIFF_INT", "WINPROBEDIFF_REDCARDDIFF_INT", "WINPROBEDIFF_MINUTE_INT"

completed.models <- c("BASELINE", 
"NO_REDCARDDIFF", "NO_WINPROBDIFF", "NO_SCOREDIFF", "NO_MINUTE", "NO_HOMEAWAY",
"SCOREDIFF_MINUTE_INT", "REDCARDDIFF_MINUTE_INT",
"WINPROBEDIFF_SCOREDIFF_INT", "SCOREDIFF_REDCARDDIFF_INT", 
"WINPROBEDIFF_REDCARDDIFF_INT", "WINPROBEDIFF_MINUTE_INT")

string_add <- "NO_WINPROBDIFF"

league.name <- c("Bundesliga", "SerieA", "LaLiga", "Ligue1", "PremierLeague")



## Whether to only check the performance on non-zero counts
only.nonzeros <- FALSE


load(paste0("LEAVE_SEASON_OUT_PREDICTIONS_gam_nb_obj_", string_add, "_Corners.Robj"))
length(pred.list)
pred.list[[3]]


# mean((pred.list[[1]]$True - pred.list[[1]]$Pred)^2)
# mean(abs(pred.list[[1]]$True - pred.list[[1]]$Pred))

errors.mat <- errors.mat.mse <- list()

for (j in 1:5){
  print(j)
  errors.mat[[j]] <- list()
  
  
for (model in completed.models){
  print(model)
  load(paste0("LEAVE_SEASON_OUT_PREDICTIONS_gam_nb_obj_", model, "_Corners.Robj"))
  
error.vec <- error.vec.mse <- c()

# 15 seasons: average, sd season-to-season
# pred.list[[j]] %>%
#   mutate(Err = True-Pred) %>%
#   group_by(Year) %>%
#   summarise(MAE = mean(abs(Err)),
#             MSE = mean(Err^2)) %>%
#     .[["MAE"]]
  # summarise(Mean.MAE = mean(MAE),
  #           #Mean.MSE = mean(MSE),
  #           SD.MAE = sd(MAE),
  #           #SD.MSE = sd(MSE)
  #           ) 
  
if (only.nonzeros){
  pred.list[[j]] <- pred.list[[j]] %>% filter(True != 0)
}

  error.vec <-  pred.list[[j]] %>%
                   mutate(Err = True-Pred,
                          log.Err = log(True+1) - log(Pred+1),
                          unit.dev = 2*(ifelse(True !=0, 
                                               True*log(True/Pred) - True + Pred,
                                               - True + Pred))) %>%
                   group_by(Year) %>%
                   summarise(MAE = mean(abs(Err)),
                             RMSE = sqrt(mean(Err^2)),
                             MAE.log = mean(abs(log.Err)),
                             RMSE.log = sqrt(mean(log.Err^2)),
                             MAE.dev = mean(abs(unit.dev)),
                             RMSE.dev = sqrt(mean(unit.dev^2))) %>%
                   select(MAE, RMSE, MAE.log, RMSE.log, MAE.dev, RMSE.dev)
  
  

  if (model == completed.models[1]){
    for (i in colnames(error.vec)){
      errors.mat[[j]][[i]] <- data.frame(error.vec[, i])
      colnames(errors.mat[[j]][[i]])[ncol(errors.mat[[j]][[i]])] <- model
    }

  } else {
    for (i in colnames(error.vec)){
      errors.mat[[j]][[i]] <- cbind(errors.mat[[j]][[i]], error.vec[, i])
      colnames(errors.mat[[j]][[i]])[ncol(errors.mat[[j]][[i]])] <- model
  }
}
}
}


######
## Which metric do we want?
#####

metric <- c("MAE", "RMSE", "MAE.log", "RMSE.log", "MAE.dev", "RMSE.dev")[1]



final.error.mat <- final.error.mat.mse <- NULL
final.error.mat.sd <- final.error.mat.mse.sd <- NULL


for (j in 1:5){
final.error.mat <- rbind(final.error.mat,
                         data.frame(League=league.name[j], t(apply(errors.mat[[j]][[metric]], 2, mean))))
final.error.mat.sd <- rbind(final.error.mat.sd,
                         data.frame(League=league.name[j], t(apply(errors.mat[[j]][[metric]], 2, sd))))

# apply(errors.mat.mse[[j]], 2, mean)
# apply(errors.mat.mse[[j]], 2, sd)
}


t(final.error.mat)
t(final.error.mat.sd)



################################################################################
################################################################################
################################################################################
################################################################################
######################      SHOTS          #####################################
################################################################################
################################################################################
################################################################################
################################################################################

########################
########################
################
####  INCLUDING ALL OBSERVATIONS (ZEROS & NON-ZEROS)
################
########################
########################


######
## RESPONSE SCALE
########


### MAE errors:

##    Bundesliga => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Serie A => 1. NO MINUTE; 2. BASELINE
##    La Liga => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Ligue 1 => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE;
##    Premier League => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE;
##  NONE OF THE DIFFERENCES are of NOTABLE MAGNITUDE COMPARED TO SD => Mostly no more than 0.001, while SD ranges 0.004-0.006
##

 # [,1]         [,2]        [,3]        [,4]        [,5]           
 # League                       Bundesliga SerieA    LaLiga    Ligue1    PremierLeague
 # BASELINE                     0.1818968  0.1823999 0.1720075 0.1700826 0.1779848    
 # NO_REDCARDDIFF               0.1821462  0.1827839 0.1723654 0.1704819 0.1783211    
 # NO_WINPROBDIFF               0.1831034  0.1840389 0.1730977 0.1708706 0.1797714    
 # NO_SCOREDIFF                 0.1822265  0.1828938 0.1724522 0.1704889 0.1784705    
 # NO_MINUTE                    0.1824447  0.1796253 0.1724522 0.1705784 0.1784393    
 # NO_HOMEAWAY                  0.1819154  0.1824068 0.1720645 0.1700982 0.1779989    
 # SCOREDIFF_MINUTE_INT         0.1820199  0.1825662 0.1721151 0.1702120 0.1780880    
 # REDCARDDIFF_MINUTE_INT       0.1820312  0.1825793 0.1721193 0.1702049 0.1781062    
 # WINPROBEDIFF_SCOREDIFF_INT   0.1819096  0.1824249 0.1720333 0.1701118 0.1780070    
 # SCOREDIFF_REDCARDDIFF_INT    0.1819108  0.1824203 0.1720292 0.1701131 0.1780093    
 # WINPROBEDIFF_REDCARDDIFF_INT 0.1818995  0.1824053 0.1720087 0.1700765 0.1779815    
 # WINPROBEDIFF_MINUTE_INT      0.1820229  0.1825738 0.1721076 0.1702000 0.1780858 



### RMSE errors:

##    Bundesliga => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE
##    Serie A => 1. NO MINUTE; 2. BASELINE
##    La Liga => IDENTICAL: 1. BASELINE; 1. WINPROBEDIFF_REDCARDDIFF_INT
##    Ligue 1 => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE;
##    Premier League => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE;
##  NONE OF THE DIFFERENCES are of NOTABLE MAGNITUDE COMPARED TO SD => Mostly no more than 0.001, while SD ranges 0.006-0.010
##
t(final.error.mat.mse)
t(final.error.mat.mse.sd)


# [,1]         [,2]        [,3]        [,4]        [,5]           
# League                       "Bundesliga" "SerieA"    "LaLiga"    "Ligue1"    "PremierLeague"
# BASELINE                     "0.3790894"  "0.3780570" "0.3638392" "0.3621350" "0.3791095"    
# NO_REDCARDDIFF               "0.3792708"  "0.3783354" "0.3641056" "0.3624298" "0.3793590"    
# NO_WINPROBDIFF               "0.3799824"  "0.3792685" "0.3646766" "0.3627278" "0.3804423"    
# NO_SCOREDIFF                 "0.3793491"  "0.3783987" "0.3641556" "0.3624283" "0.3794859"    
# NO_MINUTE                    "0.3794767"  "0.3743604" "0.3641556" "0.3625176" "0.3794884"    
# NO_HOMEAWAY                  "0.3791049"  "0.3780631" "0.3638803" "0.3621478" "0.3791275"    
# SCOREDIFF_MINUTE_INT         "0.3791628"  "0.3781643" "0.3639243" "0.3622169" "0.3792019"    
# REDCARDDIFF_MINUTE_INT       "0.3791720"  "0.3781704" "0.3639204" "0.3622180" "0.3791944"    
# WINPROBEDIFF_SCOREDIFF_INT   "0.3790993"  "0.3780785" "0.3638632" "0.3621569" "0.3791358"    
# SCOREDIFF_REDCARDDIFF_INT    "0.3790996"  "0.3780753" "0.3638610" "0.3621660" "0.3791371"    
# WINPROBEDIFF_REDCARDDIFF_INT "0.3790893"  "0.3780588" "0.3638392" "0.3621340" "0.3791048"    
# WINPROBEDIFF_MINUTE_INT      "0.3791739"  "0.3781719" "0.3639202" "0.3622183" "0.3791908"    





######
## LOG-SCALE
######

### MAE errors:

##    Bundesliga => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Serie A => 1. NO MINUTE; 2. BASELINE
##    La Liga => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Ligue 1 => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE;
##    Premier League => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE;
##  NONE OF THE DIFFERENCES are of NOTABLE MAGNITUDE COMPARED TO SD => Mostly no more than 0.001, while SD ranges 0.002-0.003
##

# [,1]         [,2]         [,3]        [,4]        [,5]        [,6]           
# League                       "Bundesliga" "Bundesliga" "SerieA"    "LaLiga"    "Ligue1"    "PremierLeague"
# BASELINE                     "0.1818968"  "0.1818968"  "0.1823999" "0.1720075" "0.1700826" "0.1779848"    
# NO_REDCARDDIFF               "0.1821462"  "0.1821462"  "0.1827839" "0.1723654" "0.1704819" "0.1783211"    
# NO_WINPROBDIFF               "0.1831034"  "0.1831034"  "0.1840389" "0.1730977" "0.1708706" "0.1797714"    
# NO_SCOREDIFF                 "0.1822265"  "0.1822265"  "0.1828938" "0.1724522" "0.1704889" "0.1784705"    
# NO_MINUTE                    "0.1824447"  "0.1824447"  "0.1796253" "0.1724522" "0.1705784" "0.1784393"    
# NO_HOMEAWAY                  "0.1819154"  "0.1819154"  "0.1824068" "0.1720645" "0.1700982" "0.1779989"    
# SCOREDIFF_MINUTE_INT         "0.1820199"  "0.1820199"  "0.1825662" "0.1721151" "0.1702120" "0.1780880"    
# REDCARDDIFF_MINUTE_INT       "0.1820312"  "0.1820312"  "0.1825793" "0.1721193" "0.1702049" "0.1781062"    
# WINPROBEDIFF_SCOREDIFF_INT   "0.1819096"  "0.1819096"  "0.1824249" "0.1720333" "0.1701118" "0.1780070"    
# SCOREDIFF_REDCARDDIFF_INT    "0.1819108"  "0.1819108"  "0.1824203" "0.1720292" "0.1701131" "0.1780093"    
# WINPROBEDIFF_REDCARDDIFF_INT "0.1818995"  "0.1818995"  "0.1824053" "0.1720087" "0.1700765" "0.1779815"    
# WINPROBEDIFF_MINUTE_INT      "0.1820229"  "0.1820229"  "0.1825738" "0.1721076" "0.1702000" "0.1780858"


### RMSE errors:

##    Bundesliga => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE
##    Serie A => 1. NO MINUTE; 2. BASELINE
##    La Liga => IDENTICAL: 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE; 
##    Ligue 1 => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE;
##    Premier League => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE;
##  NONE OF THE DIFFERENCES are of NOTABLE MAGNITUDE COMPARED TO SD => Mostly no more than 0.001, while SD ranges 0.003-0.055
##

# [,1]         [,2]        [,3]        [,4]        [,5]           
# League                       "Bundesliga" "SerieA"    "LaLiga"    "Ligue1"    "PremierLeague"
# BASELINE                     "0.2459603"  "0.2461372" "0.2377721" "0.2363834" "0.2443477"    
# NO_REDCARDDIFF               "0.2460728"  "0.2463128" "0.2379380" "0.2365609" "0.2444980"    
# NO_WINPROBDIFF               "0.2465143"  "0.2469033" "0.2382996" "0.2367576" "0.2451689"    
# NO_SCOREDIFF                 "0.2461178"  "0.2463499" "0.2379677" "0.2365625" "0.2445735"    
# NO_MINUTE                    "0.2462022"  "0.2434965" "0.2379677" "0.2366211" "0.2445737"    
# NO_HOMEAWAY                  "0.2459693"  "0.2461405" "0.2377975" "0.2363884" "0.2443548"    
# SCOREDIFF_MINUTE_INT         "0.2460057"  "0.2462034" "0.2378222" "0.2364338" "0.2443965"    
# REDCARDDIFF_MINUTE_INT       "0.2460114"  "0.2462053" "0.2378184" "0.2364313" "0.2443955"    
# WINPROBEDIFF_SCOREDIFF_INT   "0.2459660"  "0.2461519" "0.2377890" "0.2363975" "0.2443643"    
# SCOREDIFF_REDCARDDIFF_INT    "0.2459669"  "0.2461484" "0.2377867" "0.2364035" "0.2443647"    
# WINPROBEDIFF_REDCARDDIFF_INT "0.2459594"  "0.2461380" "0.2377709" "0.2363807" "0.2443434"    
# WINPROBEDIFF_MINUTE_INT      "0.2460154"  "0.2462076" "0.2378190" "0.2364331" "0.2443966" 





######
## DEVIANCE
######

### MAE

# > t(final.error.mat)
# [,1]         [,2]       [,3]       [,4]       [,5]           
# League                       "Bundesliga" "SerieA"   "LaLiga"   "Ligue1"   "PremierLeague"
# BASELINE                     "2.758317"   "2.708175" "2.824484" "2.870864" "2.833988"     
# NO_REDCARDDIFF               "2.764534"   "2.717065" "2.834300" "2.882328" "2.842523"     
# NO_WINPROBDIFF               "2.788870"   "2.746835" "2.854067" "2.892570" "2.881449"     
# NO_SCOREDIFF                 "2.766496"   "2.721689" "2.838036" "2.884189" "2.848995"     
# NO_MINUTE                    "2.773654"   "2.767995" "2.838036" "2.884498" "2.846648"     
# NO_HOMEAWAY                  "2.758780"   "2.708348" "2.826052" "2.871492" "2.834617"     
# SCOREDIFF_MINUTE_INT         "2.762706"   "2.713369" "2.829201" "2.875193" "2.838615"     
# REDCARDDIFF_MINUTE_INT       "2.762908"   "2.713889" "2.829231" "2.875891" "2.838653"     
# WINPROBEDIFF_SCOREDIFF_INT   "2.758776"   "2.708775" "2.825198" "2.871168" "2.834626"     
# SCOREDIFF_REDCARDDIFF_INT    "2.758787"   "2.708710" "2.825165" "2.871221" "2.834594"     
# WINPROBEDIFF_REDCARDDIFF_INT "2.758292"   "2.708210" "2.824514" "2.871003" "2.833894"     
# WINPROBEDIFF_MINUTE_INT      "2.762849"   "2.713917" "2.829262" "2.876006" "2.838668"     
# > t(final.error.mat.sd)
# [,1]         [,2]         [,3]         [,4]         [,5]           
# League                       "Bundesliga" "SerieA"     "LaLiga"     "Ligue1"     "PremierLeague"
# BASELINE                     "0.05090607" "0.04377704" "0.03366439" "0.04557879" "0.03264089"   
# NO_REDCARDDIFF               "0.05233025" "0.04369882" "0.03382834" "0.04864377" "0.03314561"   
# NO_WINPROBDIFF               "0.05523751" "0.06087870" "0.02568819" "0.05048337" "0.03014067"   
# NO_SCOREDIFF                 "0.04990517" "0.04320849" "0.03699471" "0.04274250" "0.03117804"   
# NO_MINUTE                    "0.04805489" "0.08675872" "0.03699471" "0.04378317" "0.03348973"   
# NO_HOMEAWAY                  "0.05077260" "0.04332540" "0.03522736" "0.04472210" "0.03304655"   
# SCOREDIFF_MINUTE_INT         "0.04955486" "0.04260374" "0.03471180" "0.04557466" "0.03300110"   
# REDCARDDIFF_MINUTE_INT       "0.04998258" "0.04263151" "0.03419986" "0.04474193" "0.03198390"   
# WINPROBEDIFF_SCOREDIFF_INT   "0.05115527" "0.04347594" "0.03348751" "0.04570843" "0.03234261"   
# SCOREDIFF_REDCARDDIFF_INT    "0.05067965" "0.04355963" "0.03391309" "0.04570764" "0.03248872"   
# WINPROBEDIFF_REDCARDDIFF_INT "0.05108817" "0.04381462" "0.03323912" "0.04522279" "0.03230019"   
# WINPROBEDIFF_MINUTE_INT      "0.04992016" "0.04255958" "0.03407177" "0.04526890" "0.03189566"  



### RMSE

# [,1]         [,2]       [,3]       [,4]       [,5]           
# League                       "Bundesliga" "SerieA"   "LaLiga"   "Ligue1"   "PremierLeague"
# BASELINE                     "3.270383"   "3.201719" "3.298331" "3.345287" "3.396043"     
# NO_REDCARDDIFF               "3.274248"   "3.207242" "3.303885" "3.353161" "3.402315"     
# NO_WINPROBDIFF               "3.290759"   "3.223937" "3.315643" "3.360073" "3.425527"     
# NO_SCOREDIFF                 "3.276724"   "3.209244" "3.307638" "3.352603" "3.406273"     
# NO_MINUTE                    "3.279271"   "3.271742" "3.307638" "3.355739" "3.406185"     
# NO_HOMEAWAY                  "3.270708"   "3.201957" "3.299439" "3.345965" "3.396761"     
# SCOREDIFF_MINUTE_INT         "3.271651"   "3.204384" "3.301224" "3.347696" "3.400105"     
# REDCARDDIFF_MINUTE_INT       "3.272052"   "3.204961" "3.301588" "3.348319" "3.399980"     
# WINPROBEDIFF_SCOREDIFF_INT   "3.270753"   "3.201896" "3.298447" "3.345509" "3.396570"     
# SCOREDIFF_REDCARDDIFF_INT    "3.270519"   "3.201853" "3.298507" "3.345541" "3.396493"     
# WINPROBEDIFF_REDCARDDIFF_INT "3.270480"   "3.201841" "3.298563" "3.345449" "3.395869"     
# WINPROBEDIFF_MINUTE_INT      "3.271897"   "3.205000" "3.301450" "3.348255" "3.399732"     
# > t(final.error.mat.sd)
# [,1]         [,2]         [,3]         [,4]         [,5]           
# League                       "Bundesliga" "SerieA"     "LaLiga"     "Ligue1"     "PremierLeague"
# BASELINE                     "0.10507035" "0.09539709" "0.05749847" "0.09029838" "0.05315785"   
# NO_REDCARDDIFF               "0.10665078" "0.09595958" "0.05849970" "0.09451209" "0.05310381"   
# NO_WINPROBDIFF               "0.10813115" "0.11390925" "0.04797159" "0.09307759" "0.05425746"   
# NO_SCOREDIFF                 "0.10481112" "0.09624795" "0.06380720" "0.08691473" "0.05284399"   
# NO_MINUTE                    "0.10077190" "0.15980804" "0.06380720" "0.08690646" "0.05167784"   
# NO_HOMEAWAY                  "0.10510680" "0.09529693" "0.05922417" "0.08973911" "0.05303111"   
# SCOREDIFF_MINUTE_INT         "0.10235190" "0.09461195" "0.05925123" "0.09029580" "0.05391025"   
# REDCARDDIFF_MINUTE_INT       "0.10274024" "0.09433869" "0.05888930" "0.08884947" "0.05369174"   
# WINPROBEDIFF_SCOREDIFF_INT   "0.10515383" "0.09525876" "0.05782653" "0.09074867" "0.05298272"   
# SCOREDIFF_REDCARDDIFF_INT    "0.10478673" "0.09520352" "0.05810757" "0.09049796" "0.05293796"   
# WINPROBEDIFF_REDCARDDIFF_INT "0.10523097" "0.09548341" "0.05706224" "0.08987180" "0.05228627"   
# WINPROBEDIFF_MINUTE_INT      "0.10250983" "0.09425235" "0.05864187" "0.08938055" "0.05337831"  








########################
########################
################
####  ONLY NON-ZEROS
################
########################
########################


######
## RESPONSE SCALE
########


### MAE errors:

## BASELINE IS BEST EVERYWHERE, albeit WINPROBEDIFF_REDCARDDIFF_INT IS CLOSELY BEHIND IN ALL OF THOSE TOO
##  NONE OF THE DIFFERENCES are of NOTABLE MAGNITUDE COMPARED TO SD => Mostly no more than 0.001-0.005, while SD ranges 0.006-0.010

##    Bundesliga => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Serie A => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    La Liga => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Ligue 1 => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Premier League => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##

# [,1]         [,2]        [,3]        [,4]        [,5]           
# League                       "Bundesliga" "SerieA"    "LaLiga"    "Ligue1"    "PremierLeague"
# BASELINE                     "0.9652381"  "0.9572459" "0.9613550" "0.9662267" "0.9747252"    
# NO_REDCARDDIFF               "0.9662414"  "0.9587727" "0.9628942" "0.9679103" "0.9761299"    
# NO_WINPROBDIFF               "0.9700607"  "0.9637297" "0.9659265" "0.9694361" "0.9821180"    
# NO_SCOREDIFF                 "0.9665705"  "0.9591680" "0.9631207" "0.9679494" "0.9768204"    
# NO_MINUTE                    "0.9673970"  "0.9641055" "0.9631207" "0.9682522" "0.9767327"    
# NO_HOMEAWAY                  "0.9653157"  "0.9572756" "0.9615877" "0.9663129" "0.9748241"    
# SCOREDIFF_MINUTE_INT         "0.9657331"  "0.9578653" "0.9618612" "0.9667000" "0.9752099"    
# REDCARDDIFF_MINUTE_INT       "0.9657710"  "0.9579347" "0.9618604" "0.9667623" "0.9752373"    
# WINPROBEDIFF_SCOREDIFF_INT   "0.9653035"  "0.9573437" "0.9614747" "0.9663146" "0.9748179"    
# SCOREDIFF_REDCARDDIFF_INT    "0.9652927"  "0.9573439" "0.9614630" "0.9663056" "0.9748171"    
# WINPROBEDIFF_REDCARDDIFF_INT "0.9652495"  "0.9572702" "0.9613812" "0.9662464" "0.9747448"    
# WINPROBEDIFF_MINUTE_INT      "0.9657330"  "0.9579064" "0.9618195" "0.9667387" "0.9751595"



### RMSE errors:

## BASELINE IS BEST EVERYWHERE, albeit WINPROBEDIFF_REDCARDDIFF_INT IS CLOSELY BEHIND IN ALL OF THOSE TOO
##  NONE OF THE DIFFERENCES are of NOTABLE MAGNITUDE COMPARED TO SD => Mostly no more than 0.001-0.010, while SD ranges 0.009-0.017

##    Bundesliga => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Serie A => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    La Liga => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Ligue 1 => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Premier League => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##

# [,1]         [,2]       [,3]       [,4]       [,5]           
# League                       "Bundesliga" "SerieA"   "LaLiga"   "Ligue1"   "PremierLeague"
# BASELINE                     "1.025549"   "1.014961" "1.015168" "1.020528" "1.040282"     
# NO_REDCARDDIFF               "1.026455"   "1.016321" "1.016546" "1.022091" "1.041550"     
# NO_WINPROBDIFF               "1.029974"   "1.020834" "1.019361" "1.023506" "1.046997"     
# NO_SCOREDIFF                 "1.026808"   "1.016702" "1.016816" "1.022109" "1.042210"     
# NO_MINUTE                    "1.027511"   "1.023158" "1.016816" "1.022439" "1.042152"     
# NO_HOMEAWAY                  "1.025624"   "1.014991" "1.015380" "1.020614" "1.040384"     
# SCOREDIFF_MINUTE_INT         "1.025979"   "1.015525" "1.015636" "1.020967" "1.040755"     
# REDCARDDIFF_MINUTE_INT       "1.026019"   "1.015588" "1.015641" "1.021021" "1.040764"     
# WINPROBEDIFF_SCOREDIFF_INT   "1.025607"   "1.015050" "1.015270" "1.020612" "1.040376"     
# SCOREDIFF_REDCARDDIFF_INT    "1.025599"   "1.015049" "1.015262" "1.020609" "1.040374"     
# WINPROBEDIFF_REDCARDDIFF_INT "1.025559"   "1.014978" "1.015184" "1.020543" "1.040288"     
# WINPROBEDIFF_MINUTE_INT      "1.025990"   "1.015570" "1.015613" "1.021003" "1.040702" 



######
## LOG-SCALE
######

### MAE errors:

## BASELINE IS BEST EVERYWHERE, albeit WINPROBEDIFF_REDCARDDIFF_INT IS CLOSELY BEHIND IN ALL OF THOSE TOO
##  NONE OF THE DIFFERENCES are of NOTABLE MAGNITUDE COMPARED TO SD => Mostly no more than 0.0001-0.0010, while SD ranges 0.0030-0.0050

##    Bundesliga => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Serie A => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    La Liga => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Ligue 1 => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Premier League => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##

# [,1]         [,2]        [,3]        [,4]        [,5]           
# League                       "Bundesliga" "SerieA"    "LaLiga"    "Ligue1"    "PremierLeague"
# BASELINE                     "0.6024931"  "0.5979555" "0.6050826" "0.6086096" "0.6064828"    
# NO_REDCARDDIFF               "0.6033056"  "0.5991780" "0.6063353" "0.6099915" "0.6076100"    
# NO_WINPROBDIFF               "0.6064286"  "0.6032000" "0.6088321" "0.6112568" "0.6125227"    
# NO_SCOREDIFF                 "0.6035687"  "0.5995435" "0.6065585" "0.6100600" "0.6082101"    
# NO_MINUTE                    "0.6042849"  "0.6025892" "0.6065585" "0.6102752" "0.6081054"    
# NO_HOMEAWAY                  "0.6025556"  "0.5979789" "0.6052737" "0.6086807" "0.6065621"    
# SCOREDIFF_MINUTE_INT         "0.6029174"  "0.5984807" "0.6055169" "0.6090170" "0.6068966"    
# REDCARDDIFF_MINUTE_INT       "0.6029473"  "0.5985361" "0.6055138" "0.6090687" "0.6069143"    
# WINPROBEDIFF_SCOREDIFF_INT   "0.6025440"  "0.5980353" "0.6051816" "0.6086785" "0.6065597"    
# SCOREDIFF_REDCARDDIFF_INT    "0.6025406"  "0.5980343" "0.6051734" "0.6086748" "0.6065609"    
# WINPROBEDIFF_REDCARDDIFF_INT "0.6024982"  "0.5979696" "0.6050980" "0.6086232" "0.6064918"    
# WINPROBEDIFF_MINUTE_INT      "0.6029214"  "0.5985183" "0.6054889" "0.6090552" "0.6068648" 



######
## LOG-SCALE
######

### RMSE errors:

## BASELINE IS BEST NEARLY(!) EVERYWHERE (one is identical, another one loses), albeit WINPROBEDIFF_REDCARDDIFF_INT IS CLOSELY BEHIND IN ALL OF THOSE TOO
##  NONE OF THE DIFFERENCES are of NOTABLE MAGNITUDE COMPARED TO SD => Mostly no more than 0.0010-0.0040, while SD ranges 0.0030-0.0050

##    Bundesliga => IDENTICAL PRETTY MUCH: 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Serie A => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    La Liga => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Ligue 1 => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Premier League => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE; 


# [,1]         [,2]        [,3]        [,4]        [,5]           
# League                       "Bundesliga" "SerieA"    "LaLiga"    "Ligue1"    "PremierLeague"
# BASELINE                     "0.6174991"  "0.6126860" "0.6185777" "0.6220409" "0.6228160"    
# NO_REDCARDDIFF               "0.6182140"  "0.6137430" "0.6196706" "0.6232824" "0.6237980"    
# NO_WINPROBDIFF               "0.6210194"  "0.6173053" "0.6219227" "0.6244249" "0.6281880"    
# NO_SCOREDIFF                 "0.6184722"  "0.6141161" "0.6199361" "0.6233649" "0.6243820"    
# NO_MINUTE                    "0.6191182"  "0.6174096" "0.6199361" "0.6235547" "0.6242699"    
# NO_HOMEAWAY                  "0.6175564"  "0.6127075" "0.6187481" "0.6221085" "0.6228919"    
# SCOREDIFF_MINUTE_INT         "0.6178849"  "0.6131728" "0.6189855" "0.6224233" "0.6232144"    
# REDCARDDIFF_MINUTE_INT       "0.6179135"  "0.6132216" "0.6189837" "0.6224683" "0.6232215"    
# WINPROBEDIFF_SCOREDIFF_INT   "0.6175420"  "0.6127571" "0.6186628" "0.6221009" "0.6228888"    
# SCOREDIFF_REDCARDDIFF_INT    "0.6175440"  "0.6127542" "0.6186579" "0.6221028" "0.6228908"    
# WINPROBEDIFF_REDCARDDIFF_INT "0.6174998"  "0.6126920" "0.6185828" "0.6220497" "0.6228118"    
# WINPROBEDIFF_MINUTE_INT      "0.6178988"  "0.6132146" "0.6189741" "0.6224638" "0.6231943"






################################################################################
################################################################################
################################################################################
################################################################################
######################      CORNERS        #####################################
################################################################################
################################################################################
################################################################################
################################################################################


## RESPONSE SCALE

## MAE
## NOTE: "NO MINUTE" & "NO WINPROBEDIFF" IS STILL UNAVAILABLE

##    Bundesliga => 1. BASELINE; 2. WINPROBEDIFF_REDCARDDIFF_INT
##    Serie A => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE;
##    La Liga => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE;
##    Ligue 1 => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. BASELINE;
##    Premier League => 1. WINPROBEDIFF_REDCARDDIFF_INT; 2. NO_HOMEAWAY
##  NONE OF THE DIFFERENCES are of NOTABLE MAGNITUDE COMPARED TO SD => Mostly no more than 0.001, while SD ranges 0.001-0.004


# League                       "Bundesliga" "SerieA"     "LaLiga"     "Ligue1"     "PremierLeague"
 # BASELINE                     0.09654862 0.10015904 0.09902463 0.09399310 0.10224868   
 # NO_REDCARDDIFF               0.09658422 0.10020813 0.09907141 0.09404693 0.10228953   
 # NO_WINPROBDIFF               0.09678779 0.10045485 0.09923122 0.09415591 0.10258632
 # NO_SCOREDIFF                 0.09666477 0.10042563 0.09923649 0.09418771 0.10239020  
 # NO_MINUTE                    0.09659228 0.10020511 0.09906729 0.09402563 0.10228735   
 # NO_HOMEAWAY                  0.09655172 0.10016359 0.09904560 0.09399752 0.10224806   
 # SCOREDIFF_MINUTE_INT         0.09656894 0.10019004 0.09905011 0.09402056 0.10226620   
 # REDCARDDIFF_MINUTE_INT       0.09656727 0.10018292 0.09903703 0.09400800 0.10226273   
 # WINPROBEDIFF_SCOREDIFF_INT   0.09655460 0.10016836 0.09903609 0.09400967 0.10225460   
 # SCOREDIFF_REDCARDDIFF_INT    0.09655408 0.10016450 0.09903719 0.09400853 0.10226163   
 # WINPROBEDIFF_REDCARDDIFF_INT 0.09655017 0.10015891 0.09902192 0.09399298 0.10224341   
 # WINPROBEDIFF_MINUTE_INT      0.09656872 0.10018019 0.09903359 0.09400658 0.10225585


# League                       "Bundesliga"  "SerieA"      "LaLiga"      "Ligue1"      "PremierLeague"
# BASELINE                     "0.002291086" "0.002103753" "0.003753825" "0.001033994" "0.002226063"  
# NO_REDCARDDIFF               "0.002291792" "0.002142331" "0.003779980" "0.001010092" "0.002218367"  
# NO_SCOREDIFF                 "0.002286813" "0.002132044" "0.003803172" "0.001006083" "0.002129950"  
# NO_HOMEAWAY                  "0.002275946" "0.002094985" "0.003761796" "0.001020000" "0.002194684"  
# SCOREDIFF_MINUTE_INT         "0.002259940" "0.002076163" "0.003757645" "0.001013549" "0.002166139"  
# REDCARDDIFF_MINUTE_INT       "0.002265304" "0.002080851" "0.003735908" "0.001017381" "0.002193379"  
# WINPROBEDIFF_SCOREDIFF_INT   "0.002306601" "0.002100890" "0.003767616" "0.001006470" "0.002215754"  
# SCOREDIFF_REDCARDDIFF_INT    "0.002288075" "0.002096136" "0.003769158" "0.001027885" "0.002216067"  
# WINPROBEDIFF_REDCARDDIFF_INT "0.002305022" "0.002106546" "0.003750935" "0.001027407" "0.002216936"  
# WINPROBEDIFF_MINUTE_INT      "0.002281897" "0.002082944" "0.003727178" "0.001020368" "0.002181382"

