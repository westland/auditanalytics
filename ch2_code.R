# Load the required packages
library(bayesAB)
library(dplyr)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Simulate data for group A
n_A <- 1000
conversion_rate_A <- 0.40
data_A <- rbinom(n_A, 1, conversion_rate_A)

# Define the range of conversion rates for group B
conversion_rate_B_values <- seq(0, 1, by = 0.01)
n_B <- 1000

# Initialize vectors to store results
p_values <- A_est <- B_est<-  
  numeric(length(conversion_rate_B_values))
bayesian_probabilities <- 
  numeric(length(conversion_rate_B_values))

# Informative priors
prior_alpha_A <- 10  # Prior successes 
prior_beta_A <- 90   # Prior failures 


# Loop over each conversion rate for group B
for (i in seq_along(conversion_rate_B_values)) {
  conversion_rate_B <- conversion_rate_B_values[i]
  
  # Generate data for group B
  data_B <- rbinom(n_B, 1, conversion_rate_B)
  
  # Frequentist A/B test
  freq_test <- prop.test(c(sum(data_A),
                           sum(data_B)), 
                         c(n_A, n_B))
  p_values[i] <- freq_test$p.value
  A_est[i] <- freq_test$estimate[[1]]
  B_est[i] <- freq_test$estimate[[2]]
  
  # Bayesian A/B test
  bayes_test <- bayesAB::bayesTest(
    data_A,
    data_B,
    priors = c('alpha' = prior_alpha_A,
               'beta' = prior_beta_A),
    distribution = 'bernoulli'
  )
  
  # Extract the probability that B is better than A
  bayesian_probabilities[i] <- 
    max(bayes_test$posteriors$Probability$B)
}

# Create a data frame for plotting
results_df <- data.frame(
  conversion_rate_B = 
    conversion_rate_B_values,
  p_value = p_values,
  bayesian_probability =
    bayesian_probabilities,
  A_values = A_est, 
  B_values = B_est
)


ggplot(results_df) +
  geom_line(aes(x = conversion_rate_B, 
                y = p_value), color = 'darkgrey') +
  geom_line(aes(x = conversion_rate_B, 
                y = bayesian_probability), 
            color = 'black') +
  geom_hline(yintercept = 0.4, 
             linetype = 'dashed', 
             color = 'lightgrey') +
  geom_vline(xintercept = 0.4, 
             linetype = 'dashed', 
             color = 'lightgrey') +
  labs(title = " ", 
       #'Bayesian A/B Probabilities (black) versus 
       #'Frequentist p-values (grey)',
       x = 'Conversion Rate B',
       y = 'Probability B > A & p-value') +
  theme_bw() 


#_________________________________________________________

library(ggplot2)
library(tidyr)
library(reshape2)
library(tidyverse)
library(kableExtra)

stats <- data.frame()
hypt <-  vector()
for (j in 1:2000) {
  hypt[j] <- i <-  0
  for (i in 1:100) {
    k <- j * 5
    A <- rnorm(k, 110000, 300000)
    B <- rnorm(k, 100000, 300000)
    if (t.test(x = A, y = B, 
               paired = TRUE)$statistic < 0)
      hypt[j] <- hypt[j] + 1
    stats[i, j] <- t.test(x = A, y = B,
                          paired = TRUE)$p.value
  }
}

meanp <- apply(stats, 2, mean)
mp <- data.frame(meanp, seq(5, 10000, 5))
colnames(mp) <- c("p", "sample_size")

hyp <- cbind(hypt, seq(5, 10000, 5)) %>% 
  as.data.frame() 
colnames(hyp) <- c("pct_false", "sample_size")
to_graph <- 
  inner_join(mp, hyp,
             by="sample_size") %>%  
  mutate(pct_false = pct_false/100)

long_graph <- pivot_longer(to_graph, 
                           cols = c("p","pct_false"))
long_graph <- long_graph %>% 
  mutate(name = ifelse(name == "p", 
                       "p-value", 
                       "Wrong Decisions"))
ggplot(data=long_graph, aes(x=sample_size, 
                            y=value, 
                            colour=name)) +
  geom_smooth() +
  scale_colour_grey(start = 0, end = .6) + 
  theme_bw() +
  #  ggtitle("More Information Lowers the Proportion of Wrong Decisions") +
  xlab("Sample Size") + 
  ylab("Proportion") + 
  theme(legend.title=element_blank())



