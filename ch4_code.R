library(tidyverse)
library(ggplot2)

# Define the parameters for the two normal distributions
mean1 <- 600
sd1 <- 200
mean2 <- 400
sd2 <- 200


q_B <- qnorm(seq(0,1,.01), 
             mean =  mean2, sd = sd2)
sq <- seq(0,1,.01)
q_B <- cbind(sq,q_B) %>% 
  as.data.frame()  

# prob=.85 implies .15 in tail  



q_A <- qnorm(seq(0,1,.01), 
             mean =  mean1, sd = sd1)
q_A <- cbind(sq,q_A) %>% 
  as.data.frame()

## prob=.98 implies .02 in tail


# Create a sequence of x values
x <- seq(00, 1500, length.out = 1000)

# Calculate the density values for both distributions
density1 <- dnorm(x, 
                  mean = mean1,
                  sd = sd1)
density2 <- dnorm(x, 
                  mean = mean2, 
                  sd = sd2)

# Create a data frame to hold the values
df <- data.frame(x = c(x, x),
                 density = 
                   c(density1, density2),
                 group = 
                   factor(rep(c("Choice A", 
                                "Choice B"),
                              each = length(x))))

# Plot the density functions
ggplot(df, aes(x = x, y = density, color = group)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("grey", "black")) +
  theme_minimal() +  
  geom_vline(xintercept = 800,
             linetype = "dashed") +
  labs(title = 
         "Density Functions of Two Posterior Distributions",
       x = "Unit sales",
       y = "Density") +
  theme(legend.title = element_blank()) + xlim(0,1500)  

#___________________________________________________



library(tidyverse)
library(ggplot2)

# Define the profit function
profit_function <- function(units) {
  profit <- 4 * units - 1000
  return(profit)
}

# Define the density functions for the two distributions
density_1 <- function(x) {
  dnorm(x, mean = 600, sd = 200)
}

density_2 <- function(x) {
  dnorm(x, mean = 400, sd = 200)
}

# Generate data for plotting
units_sold <- seq(0, 1000, by = 1)
profit_1 <- 
  density_1(units_sold) * 
  profit_function(units_sold) * units_sold
profit_2 <- 
  density_2(units_sold) * 
  profit_function(units_sold) * units_sold

# Create a data frame for ggplot
data <- data.frame(
  Units = rep(units_sold, 2),
  Profit = c(profit_1, profit_2),
  Choice = rep(c("Choice A", "Choice B"), 
               each = length(units_sold))
)

# Plot the data
ggplot(data, aes(x = Units, y = Profit, color = Choice)) +
  geom_line(lwd=1) +
  scale_color_manual(values = c("grey50", "grey80")) +
  labs(
    title = "Expected Total Profit for Number of Units Sold",
    x = "Number of Units Sold",
    y = "Expected Total Profit",
    caption = "Profit maximizing choice"
  ) +
  theme_minimal()


