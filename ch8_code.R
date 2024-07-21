# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(knitr)
library(kableExtra)

# Define a function to calculate 
#log-normal parameters, 
#plot PDF and CDF, and compute NPV
plot_lognormal_npv <- 
  function(
    mean_life_expectancy,    
    sd_life_expectancy, 
    color, 
    discount_rate = 0.05, 
    prior_mean, prior_sd) {
    # Convert mean and sd to log-normal parameters
    mu <- 
      log(mean_life_expectancy^2 / 
            sqrt(sd_life_expectancy^2 + 
                   mean_life_expectancy^2))
    sigma <- 
      sqrt(log(1 + (sd_life_expectancy^2 / 
                      mean_life_expectancy^2)))
    
    # Generate a sequence of ages
    ages <- seq(0, 120, by = 0.1)
    
    # Calculate the probability density
    #function (PDF) and cumulative density function (CDF)
    pdf_values <- dlnorm(ages, 
                         meanlog = mu, 
                         sdlog = sigma)
    cdf_values <- plnorm(ages,
                         meanlog = mu, 
                         sdlog = sigma)
    
    # Calculate NPV of $1,000,000 payout 
    #at the time of death
    npv_values <- 10000 * 
      exp(-discount_rate * ages) * pdf_values
    npv <- sum(npv_values) * 
      (ages[2] - ages[1])  
    # Integrate over the age range
    
    # Bayesian analysis manually
    data <- rlnorm(length(ages), 
                   meanlog = mu, 
                   sdlog = sigma)
    n <- length(data)
    data_mean <- mean(data)
    data_var <- var(data)
    
    # Calculate posterior mean and variance
    post_var <- 1 / 
      (1 / prior_sd^2 + n / data_var)
    post_mean <- post_var * 
      (prior_mean / prior_sd^2 + n * 
         data_mean / data_var)
    posterior_sd <- sqrt(post_var)
    
    # Calculate NPV from posterior values
    posterior_npv_values <- 
      10000 * exp(-discount_rate * ages) * 
      dlnorm(ages,                             
             meanlog = log(post_mean), 
             sdlog = posterior_sd)
    posterior_npv <- 
      sum(posterior_npv_values) * 
      (ages[2] - ages[1])
    
    # Create a data frame for plotting
    plot_data <- data.frame(
      Age = ages, 
      PDF = pdf_values, 
      CDF = cdf_values)
    
    # Plot the PDF
    pdf_plot <- ggplot(plot_data, 
                       aes(x = Age, y = PDF)) +
      geom_line(color = color) +
      ggtitle(paste("Age at Death")) +
      xlim(0,25) + 
      xlab("Age") +
      ylab("Probability Density") +
      theme_minimal()
    
    # Plot the CDF
    cdf_plot <- ggplot(plot_data, aes(
      x = Age, 
      y = CDF)) +
      geom_line(color = color) +
      ggtitle(paste("Age at Death")) +
      xlab("Age") +
      ylab("Cumulative Probability") +
      xlim(0,25) + 
      theme_minimal()
    
    list(pdf_plot = pdf_plot, 
         cdf_plot = cdf_plot, 
         npv = npv, 
         posterior_npv = posterior_npv)
  }


#______________________________________________________


# Scenario 2: Average age of 
#death is 15 with standard deviation 10
scenario2 <- 
  plot_lognormal_npv(mean_life_expectancy = 15, 
                     sd_life_expectancy = 10, 
                     color = "black", 
                     prior_mean = 20,
                     prior_sd = 10)

# Create a data frame for the results
results <- data.frame(
  Scenario = c("Dried Food", "Fresh Food"),
  #  Mean_Age = c(5, 15),
  #  SD_Age = c(10, 10),
  NPV = 
    round(c(scenario1$npv, 
            scenario2$npv), 2),
  Posterior_NPV = 
    round(c(scenario1$posterior_npv,
            scenario2$posterior_npv), 2)
)



# Display the results in a table
kable(
  results,
  caption = 
    "Comparative Actuarial 
  Costs of Pet Insurance",
  col.names = c(
    "Diet",
    #   "Mean Age",
    #   "SD Age",
    "Frequentist NP Cost of 
    Insurance Claim ($)",
    "Bayesian Posterior NP 
    Cost of Insurance Claim ($)"
  )
) %>%
  kable_styling(full_width = F,
                position = "center"
  ) %>%
  column_spec(1, width = "3cm") %>%
  #  column_spec(2, width = "2cm") %>%
  #  column_spec(3, width = "2cm") %>%
  column_spec(2, width = "3cm") %>%
  column_spec(3, width = "3cm")

# Arrange the plots


library(grid)
library(gridExtra)



grid.arrange(
  scenario1$pdf_plot,
  scenario2$pdf_plot,
  ncol = 2,
  top = textGrob(
    "Mortality Distributions for Dried Food 
    (left) and Fresh Food (right)",
    gp = gpar(fontsize = 14, font = 3)
  )
)




grid.arrange(
  scenario1$cdf_plot,
  scenario2$cdf_plot,
  ncol = 2,
  top = textGrob(
    "Mortality Distributions for Dried Food
    (left) and Fresh Food (right)",
    gp = gpar(fontsize = 14, font = 3)
  )
)



#______________________________________________________



library(tidyverse)
library(readr)
library(bayesAB)

set.seed(123)


girls_names <- read_csv(("girls_names.csv"),
                        col_names = FALSE)[, 2]


sdlog <- 2
meanlog <- 
  
  prof_A <- rpois(1000, 6) # mean 5.5
prof_B <- rpois(1000, 5) # mean 3.7
ron_rate <- rpois(1000, 7) # mean 6.0

prof_A <-ifelse(prof_A > 11, rpois(1000, 6), prof_A)
prof_B <-ifelse(prof_B > 1, rpois(1000, 5), prof_B)
ron_rate <-ifelse(ron_rate > 11, rpois(1000, 7), ron_rate)
prof_A <-ifelse(prof_A > 11, 10, prof_A)
prof_B <-ifelse(prof_B > 11,10, prof_B)
ron_rate <-ifelse(ron_rate > 11, 10, ron_rate)

## this tibble is the "ground truth" 
#dating pool on MatchRate; both sides 
#will in exceptional circumstances assign a rating higher than 10

dating_pool <- tibble(girls_names, prof_A, prof_B, ron_rate)

colnames(dating_pool) <- 
  c("lady", "Profile_A", 
    "Profile_B", 
    "Ladies_rating")
write.csv(dating_pool, 
          file="DATABASE_dating_pool.csv")


# Melt the data for easier plotting with ggplot2
library(reshape2)
dating_pool <- melt(dating_pool)

# Plotting
ggplot(dating_pool, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "white", 
                 color = "black") +  
  # Adjust binwidth as needed
  facet_wrap(~ variable, scales = "free") +  
  # Separate plot for each variable
  theme_minimal() +
  labs(x = "Ratings (higher is better)", y = "Count") +
  theme(legend.title = element_blank())


#____________________________________________________


dating_pool <- 
  read.csv("DATABASE_dating_pool.csv")

# Bayesian A/B test
bayes_test <- bayesAB::bayesTest(
  prof_A,
  prof_B,
  priors = c("shape" = 1, "rate" = 1),
  distribution = 'poisson'
)

dating_pool <- data.frame(
  dating_pool$lady,
  bayes_test$posteriors$Lambda$A, 
  bayes_test$posteriors$Lambda$B, 
  dating_pool$Ladies_rating
)
colnames(dating_pool) <- 
  c("lady", 
    "Profile_A", 
    "Profile_B", 
    "Ladies_rating") 

library(reshape2)
dating_pool <- melt(dating_pool)

# Plotting
ggplot(dating_pool, aes(x = value)) +
  geom_histogram(binwidth = .05, 
                 fill = "white", color = "black") +   
  # Adjust binwidth as needed
  facet_wrap(~ variable, scales = "free") + 
  # Separate plot for each variable
  theme_minimal() +
  labs(x = 
         "Ratings (higher is better)", y = "Count") +
  theme(legend.title = element_blank())

#_________________________________________________________

dating_pool <- 
  read.csv("DATABASE_dating_pool.csv")


# Required libraries
library(tidyverse)

set.seed(123)

ron_benchmark <- 8  
# the minimum rating for which 
# either Ron or the ladies will consider dating
lady_benchmark <- 5


max_attempts <- 1000
nights <- tibble()

for (i in 1:1000) {
  i <- j <- 1
  
  # Profile A
  repeat {
    tonight_pool <- 
      dating_pool[sample(nrow(dating_pool),
                         30, 
                         replace = TRUE), ]
    match_A <- subset(tonight_pool, 
                      Profile_A > ron_benchmark & 
                        Ladies_rating > lady_benchmark)
    if (nrow(match_A) > 0) {
      break
    }
    i <- i + 1
    if (i >= max_attempts) {
      print("Max attempts reached
            without finding a 
            match for Profile A.")
      break
    }
  }
  
  # Profile B
  repeat {
    tonight_pool <- 
      dating_pool[sample(nrow(dating_pool), 
                         30, 
                         replace = TRUE), ]
    match_B <- subset(tonight_pool,
                      Profile_B > ron_benchmark & 
                        Ladies_rating > lady_benchmark)
    if (nrow(match_B) > 0) {
      break
    }
    j <- j + 1
    if (j >= max_attempts) {
      print("Max attempts 
            reached without finding a match for Profile B.")
      break
    }
  }
  
  nights <- rbind(nights, tibble(Prof_A = i, Prof_B = j))
}

colnames(nights) <- c("Profile A", "Profile B")

# Reshaping and plotting
nights_long <- pivot_longer(nights, everything(),
                            names_to = "profile", 
                            values_to = "nights")
ggplot(nights_long, aes(x = nights)) +
  geom_histogram(binwidth = 1, 
                 fill = "white", color = "black") +
  facet_wrap(~profile, scales = "free") +
  theme_minimal() +
  labs(x = "Number of Nights for
       Ron to Meet his Match", 
       y = "How Often (Number of
       Nights out of 1000)") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(trans = 'log10')



