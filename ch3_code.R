library(bayesAB)
library(tidyverse)

# Set the lambda values for datasets A and B
lambda_A <- 45
lambda_B <- 46

# Define the size of each dataset
size_A <- 1000  
# Number of data points in dataset A
size_B <- 1000 
# Number of data points in dataset B

# Generate Dataset A with Poisson distribution
dataset_A <- rpois(size_A, lambda_A)

# Generate Dataset B with Poisson distribution
dataset_B <- rpois(size_B, lambda_B)

# Optionally shuffle data to randomize the order
set.seed(42)  # Setting seed for reproducibility
dataset_A <- sample(dataset_A)
dataset_B <- sample(dataset_B)


# Scenario 1: Weak prior 
test_weak_prior <- bayesTest(
  A = dataset_A,
  B = dataset_B,
  priors = c("shape" = 1, "rate" = 1),
  dist = 'poisson'
)

plot(test_weak_prior)

