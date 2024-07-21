library(bayesAB)
library(tidyverse)

bs <- read_csv("kildare_bs.csv")


pre_theft <-  bs %>% 
  filter(Year < 2015) %>% 
  select(Equity) %>% 
  unlist() %>% 
  as.integer()
post_theft <- bs %>% 
  filter(Year >= 2015) %>% 
  select(Equity) %>% 
  unlist() %>% 
  as.integer()

AB_NPV <- bayesTest(
  pre_theft,
  post_theft,
  priors = c("shape" = 10, 
             "rate" = 10/mean(bs$Equity)),  
  distribution = 'poisson'
)

plot(AB_NPV)


