
library(tidyverse)
library(bayesAB)

set.seed(123456)

A_MW_IMDB <- rpois(10000, 2)
B_MotB_IMDB <- rpois(10000, 3)

AB_IMDB <- bayesTest(A_MW_IMDB, B_MotB_IMDB, priors =
                       c('shape' = 4, 'rate' = 4),
                     distribution = 'poisson')

A_MW_RT <- rpois(10000, 3)
B_MotB_RT <- rpois(10000, 2)

AB_RT <- bayesTest(
  A_MW_RT, 
  B_MotB_RT, 
  priors = c('shape' = 4, 'rate' = 4), 
  distribution = 'poisson')



AB <- combine(AB_IMDB, AB_RT, f = `+`)

plot(AB)



