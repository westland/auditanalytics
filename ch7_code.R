library(tidyverse)
library(bayesAB)

set.seed(123)
A_binom <- rbinom(100, 1, .45)
B_binom <- rbinom(100, 1, .49)

AB1 <- bayesTest(A_binom,
                 B_binom,
                 priors = 
                   c('alpha' = 1, 'beta' = 1),
                 distribution = 'bernoulli')

# summary(AB1)
plot(AB1)


#_____________________________________

set.seed(123)
A_norm <- rnorm(100, 6, 1)
B_norm <- rnorm(100, 5, 5)


AB2 <- bayesTest(
  A_norm,
  B_norm,
  priors = c(
    'mu' = 5,
    'lambda' = 1,
    'alpha' = 3,
    'beta' = 1
  ),
  distribution = 'normal'
)





AB3 <-
  combine(
    AB1,
    AB2,
    f = `*`,
    params = c('Probability', 'Mu'),
    newName = 'Expectation'
  )
##print(AB3)
#summary(AB3)
plot(AB3)



#____________________________________________


set.seed(456)
A_binom <- rbinom(100, 1, .35)
B_binom <- rbinom(100, 1, .30)
A_norm <- rnorm(100, 5.5, 4)
B_norm <- rnorm(100, 5, 3)


## Summarize the posterior means 
#and variances from the first experiment,
#and insert these into the priors of the new A/B test

mu1 <- mean(c(AB1$posteriors$Probability$A,
              AB1$posteriors$Probability$A))
var1 <- var(c(AB1$posteriors$Probability$A,
              AB1$posteriors$Probability$A))

## From Table 2 in Chapter 3 on Priors

alpha1 <- (mu1^2-mu1^3-mu1*var1)/var1
beta1 <-  (mu1-1)*(mu1^2-mu1+var1)/var1


mu2 <- mean(c(AB2$posteriors$Sig_Sq$A,
              AB2$posteriors$Sig_Sq$B))
var2 <- mean(c(AB2$posteriors$Sig_Sq$A,
               AB2$posteriors$Sig_Sq$B))


## From Table 2 in Chapter 3 on Priors
lambda2 <- 1; alpha2 <- 2
mu2 <- mu2
beta2 <- var2


AB1a <- bayesTest(A_binom,
                  B_binom,
                  priors = 
                    c('alpha' = alpha1,
                      'beta' = beta1),
                  distribution = 'bernoulli')

AB2a <- bayesTest(
  A_norm,
  B_norm,
  priors = c(
    'mu' = mu2,
    'lambda' = lambda2,
    'alpha' = alpha2,
    'beta' = beta2
  ),
  distribution = 'normal'
)

AB3 <-
  combine(
    AB1a,
    AB2a,
    f = `*`,
    params = c('Probability', 'Mu'),
    newName = 'Expectation'
  )
#print(AB3)
#summary(AB3)
plot(AB3)



#___________________________________________________________

library(tidyverse)
library(EnvStats)  # for the Pareto distribution


set.seed(123)
dt <- sample(seq(as.Date('2024-02-01'), 
                 as.Date('2024-05-31'), by = "day"), 10000, replace = TRUE)

NR <- rbernoulli(10000, .3) %>% 
  as.data.frame()
colnames(NR) <- "type"
NR <- NR %>%
  mutate(
    userType = ifelse(type == TRUE,
                      "New Visitor",
                      "Returning Visitor"))

NR <- cbind(NR,dt)
NR_T <- NR %>% filter(type == TRUE)
NR_F <- NR %>% filter(type == FALSE)

## Don't differentiate between the userType; 
# let set.seed() preselect whether A or B is the winner
sales_T <- abs(rnorm(nrow(NR_T),
                     25,100)) %>%
  floor()
sales_F <- abs(rnorm(nrow(NR_F),
                     25,100)) %>% 
  floor()

daysSinceLastSession_T <- 0
daysSinceLastSession_F <-
  rpareto(nrow(NR_T), 1, .5) 
daysSinceLastSession_F <- 
  ifelse(
    daysSinceLastSession_F > 100,
    runif(1, 1, 5), 
    daysSinceLastSession_F) %>% floor()

nrt <-   cbind(NR_T, 
               cbind(sales_T, 
                     daysSinceLastSession_T)) %>% 
  rename(
    sales = sales_T,
    daysSinceLastSession = 
      daysSinceLastSession_T)

nrf <-   cbind(NR_F, cbind(sales_F,
                           daysSinceLastSession_F)) %>% 
  rename(
    sales = sales_F,
    daysSinceLastSession =
      daysSinceLastSession_F)

NR <- rbind(nrt,nrf) %>% 
  arrange(dt) %>% 
  rename(date = dt) 


library(bayesAB)


nr_new <- NR %>% 
  filter(userType == "New Visitor")
nr_rtn  <-  NR %>% 
  filter(userType == "Returning Visitor")

AB_new_rtn <- bayesTest(
  nr_new$sales,
  nr_rtn$sales,
  priors = c("shape" = 9, "rate" = 3),
  distribution = 'poisson'
)


plot(AB_new_rtn)


#_______________________________________________

library(bayesAB)

nr_short <- NR %>% 
  filter(daysSinceLastSession <= 2)
nr_long <- NR %>% 
  filter(daysSinceLastSession > 2)



AB_delay <- bayesTest(
  nr_short$sales,
  nr_long$sales,
  priors = c("shape" = 9, "rate" = 3),  
  
  distribution = 'poisson'
)

plot(AB_delay)



#___________________________________________


library(bayesAB)

nr_new <- NR %>% filter(userType ==
                          "New Visitor" &
                          daysSinceLastSession <= 2)
nr_rtn <-   NR %>% 
  filter(daysSinceLastSession > 2)
nr_short <- NR %>% 
  filter(daysSinceLastSession <= 2)
nr_long <- NR %>% 
  filter(daysSinceLastSession > 2)

AB_1st <- bayesTest(
  nr_new$sales,
  nr_rtn$sales,
  priors = c("shape" = 9, "rate" = 3),
  ## gives a distribution for number 
  #of days to return centered on 3
  distribution = 'poisson'
)


AB_2nd <- bayesTest(
  nr_short$sales,
  nr_long$sales,
  priors = c("shape" = 9, "rate" = 3),   
  
  distribution = 'poisson'
)



AB_combine <-
  combine(
    AB_1st,
    AB_2nd,
    f = `+`,
    params = 
      c('Lambda', 'Lambda'), 
    newName = 
      'Combined Sales Posterior'
  )


plot(AB_combine)



