# https://cran.r-project.org/web/packages/PLNmodels/vignettes/PLN.html

# NOTE: !!! DON'T FORGET ACCOUNTING FOR THE RED CARDS !!!
load("final_df_w_red_cards.csv")

library(PLNmodels)
library(tidyverse)

# remotes::install_github("pln-team/PLNmodels")
# remotes::install_github("astamm/nloptr")
# install.packages('cli')

our.data <- prepare_data(
  counts = final.df %>% select(Shots, Corners, Goals),
  covariates = final.df %>% select(Score.Diff, RedCard.Diff, HomeAway, minutes.spent),
  offset = "none"
)

our.PLN <- PLN(Abundance ~ Score.Diff + RedCard.Diff + HomeAway + minutes.spent,
               data = our.data,
               control=PLN_param(config_post = list(variational_var = TRUE)))
our.PLN$model_par
our.PL

library(corrplot)

our.PLN %>% sigma() %>% cov2cor()

our.coef <- coef(our.PLN)
our.B <- our.coef 
attr(our.coef, "variance_variational") <- NULL
attr(our.coef, "vcov_variational") <- NULL
our.coef
standard_error(our.PLN)

# p-values & conf. int., via WALD TEST
2*pnorm(abs(our.coef/standard_error(our.PLN)), lower.tail=F)
alpha <- 0.05
our.coef -
  c(qnorm(1-alpha/2) * standard_error(our.PLN))
our.coef +
  c(qnorm(1-alpha/2) * standard_error(our.PLN))

