library(wordcloud)
library(tidyverse)
library(tidytext)
library(tokenizers)
library(tm)
library(readr)
library(knitr)

reviews <- read_csv("DATABASE_reviews.csv") %>%
  select(UserId, Score, Text)  

set.seed(123)
mkt <- sample(seq_len(nrow(reviews)), 
              size = nrow(reviews) / 2)
A <- reviews %>% filter(Score < 4) 
B <- reviews %>% filter(Score >= 4) 




# NLP Analysis ________________________________________________


# Load necessary libraries
library(dplyr)
library(tidyr)
library(tokenizers)
library(tidytext)

# Check and preprocess text data
preprocess_text <- function(data) {
  if (is.data.frame(data) && "Text" %in% names(data)) {
    data$Text <- as.character(data$Text)
    Encoding(data$Text) <- "UTF-8"
    text_stuff <- tokenize_words(data$Text) %>%
      unlist() %>%
      as.data.frame(stringsAsFactors = FALSE)
    colnames(text_stuff) <- "word"
    return(text_stuff)
  } else {
    stop("Input data must be a
         data frame with a 'Text' column")
  }
}

# Compute net sentiment
compute_net_sentiment <- function(text_stuff) {
  sentiments <- get_sentiments("nrc")
  net_sentiment <- text_stuff %>%
    inner_join(sentiments, by = "word") %>%
    count(sentiment) %>%
    pivot_wider(names_from = sentiment, 
                values_from = n, 
                values_fill = list(n = 0)) %>%
    mutate(net_positive = positive - negative,
           proportion_positive = 
             positive / negative - 1)
  return(net_sentiment)
}



# Process data A and B if they are correctly formatted
text_stuff_A <- preprocess_text(A)
net_sentiment_A <- compute_net_sentiment(text_stuff_A)

text_stuff_B <- preprocess_text(B)
net_sentiment_B <- compute_net_sentiment(text_stuff_B)

# Combine results and create a summary table
net_sentiment <- rbind(net_sentiment_A, net_sentiment_B)
product <- data.frame(Product = c("A", "B"),
                      stringsAsFactors = FALSE)
net_sentiment <- cbind(product, net_sentiment)

# Print the results
print(net_sentiment)


#________________________________________________


# Load necessary libraries
library(dplyr)
library(tidyr)
library(tokenizers)
library(tidytext)

# Check and preprocess text data
preprocess_text <- function(data) {
  if (is.data.frame(data) && "Text" %in% names(data)) {
    Encoding(data$Text) <- "UTF-8"
    text_stuff <- tokenize_words(data$Text) %>%
      unlist() %>%
      as.data.frame()
    colnames(text_stuff) <- "word"
    return(text_stuff)
  } else {
    stop("Input data must be a data 
         frame with a 'Text' column")
  }
}

# Compute net sentiment
compute_net_sentiment <- function(text_stuff) {
  net_sentiment <- text_stuff %>%
    inner_join(get_sentiments("nrc"), by = "word") %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(net_positive = positive - negative,
           proportion_positive = 
             positive / negative - 1)
  return(net_sentiment)
}

# Process data A and B if they are correctly formatted
text_stuff_A <- preprocess_text(A)
net_sentiment_A <- 
  compute_net_sentiment(text_stuff_A)

text_stuff_B <- preprocess_text(B)
net_sentiment_B <- 
  compute_net_sentiment(text_stuff_B)

# Combine results and create a summary table
net_sentiment <- 
  rbind(net_sentiment_A, net_sentiment_B)
product <- data.frame(Product = c("A", "B"))
net_sentiment <- 
  cbind(product, net_sentiment)

# Print the results
print(net_sentiment)



#______________________________________

# Load necessary libraries
library(dplyr)
library(tidyr)
library(tokenizers)
library(tidytext)

# Function to check and preprocess text data
preprocess_text <- function(data) {
  # Validate that the input is a data frame
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }
  
  # Validate that the data frame contains a 'Text' column
  if (!"Text" %in% names(data)) {
    stop("Data frame must contain a 'Text' column.")
  }
  
  # Set the text encoding to UTF-8 and preprocess
  Encoding(data$Text) <- "UTF-8"
  text_stuff <- tokenize_words(data$Text) %>%
    unlist() %>%
    as.data.frame()
  colnames(text_stuff) <- "word"
  return(text_stuff)
}

# Function to compute net sentiment
compute_net_sentiment <- function(text_stuff) {
  net_sentiment <- text_stuff %>%
    inner_join(get_sentiments("nrc"), by = "word") %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(net_positive = positive - negative,
           proportion_positive = positive / negative - 1)
  return(net_sentiment)
}


# Process text data from A, ensuring it's properly formatted
if (is.data.frame(A) && "Text" %in% names(A)) {
  text_stuff_A <- preprocess_text(A)
  net_sentiment_A <- 
    compute_net_sentiment(text_stuff_A)
  
  # Combine results and create a summary table
  net_sentiment <- rbind(net_sentiment_A) 
  # Add more datasets as needed
  product <- data.frame(Product = c("A")) 
  # Extend with more products as needed
  net_sentiment <- cbind(product, net_sentiment)
  
  # Print the results
  print(net_sentiment)
} else {
  print("Error: The data 'A' is not a 
        data frame or lacks a 'Text' column.")
}



# WordClouds _______________________________________



A <- A[ceiling(runif(10000,0,nrow(A))),]  
B <- B[ceiling(runif(10000,0,nrow(B))),]

cat("Wordcloud for Product A")

Encoding(A$Text) = "UTF-8" 
text_stuff <- tokenize_words(A$Text) %>% 
  unlist() %>%
  as.data.frame()

colnames(text_stuff) <- "word"

stuff_sentiment <-
  text_stuff %>% 
  inner_join(get_sentiments("nrc"), by = "word")

stp <- get_stopwords()
stp <- rbind(stp, "good", "food", "love", 
             "sweet", "chocolate", "bad", "sugar",
             "found", "weight", "smell", "money")

frequency_A <- text_stuff %>% anti_join(stp) %>% 
  inner_join(stuff_sentiment) %>% count(word)
frequency_A  %>%
  with(wordcloud(
    word,
    colors = "black",
    rot.per = 0.15,
    n,
    max.words = 70
  ))

cat("Wordcloud for Product B")

Encoding(A$Text) = "UTF-8" 
text_stuff <- tokenize_words(B$Text) %>% 
  unlist() %>%
  as.data.frame()

colnames(text_stuff) <- "word"

stuff_sentiment <-
  text_stuff %>%
  inner_join(get_sentiments("nrc"), 
             by = "word")

frequency_B <- text_stuff %>% anti_join(stp) %>% 
  inner_join(stuff_sentiment) %>% count(word)
frequency_B  %>%
  with(wordcloud(
    word,
    colors = "black",
    rot.per = 0.15,
    n,
    max.words = 70
  ))




# Top 10 Words for Each Sentiment  ___________________________  



cat("Top 10 Words by Sentiment for Product A")

Encoding(A$Text) = "UTF-8" 
text_stuff <- tokenize_words(A$Text) %>% 
  unlist() %>%
  as.data.frame()

colnames(text_stuff) <- "word"


stuff_sentiment <-
  text_stuff %>%  anti_join(stp) %>% 
  inner_join(get_sentiments("nrc"), by = "word")

text_counts <- stuff_sentiment %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

text_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  scale_fill_grey(start = 0, end = .2) +
  theme_minimal() +  
  # Optional: starts with a minimal theme
  theme(axis.text.
        x = element_text(angle = 45, hjust = 1)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()


cat("Top 10 Words by Sentiment for Product B")

Encoding(B$Text) = "UTF-8" 
text_stuff <- tokenize_words(B$Text) %>% 
  unlist() %>%
  as.data.frame()

colnames(text_stuff) <- "word"


stuff_sentiment <-
  text_stuff %>% anti_join(stp) %>%  
  inner_join(get_sentiments("nrc"), by = "word")

text_counts <- stuff_sentiment %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

text_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  scale_fill_grey(start = 0, end = .2) +
  theme_minimal() +  
  # Optional: starts with a minimal theme
  theme(axis.text.x = element_text(
    angle = 45, hjust = 1)
  ) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()


#____________________________________________________


library(tidyverse)
library(tidytext)
library(tokenizers)
library(tm)
library(readr)
library(knitr)


Encoding(A$Text) = "UTF-8"
text_stuff <- tokenize_words(A$Text) %>%
  unlist() %>%
  as.data.frame()
colnames(text_stuff) <- "word"
stuff_sentiment_A <-
  text_stuff %>%  
  inner_join(get_sentiments("nrc"), by = "word")  %>% 
  count(sentiment, word)

sum_sent <- stuff_sentiment_A %>%  
  group_by(sentiment) %>% 
  summarize(mu = mean(n), sig = sd(n))
sent_stats_A <-
  data.frame(sum_sent$sentiment,
             sum_sent$mu , 
             sum_sent$sig)
sent_stats_A


Encoding(B$Text) = "UTF-8"
text_stuff <- tokenize_words(B$Text) %>%
  unlist() %>%
  as.data.frame()
colnames(text_stuff) <- "word"
stuff_sentiment_B <-
  text_stuff %>% 
  inner_join(get_sentiments("nrc"), 
             by = "word")  %>% 
  count(sentiment, word)

sum_sent <- stuff_sentiment_B %>%  
  group_by(sentiment) %>% 
  summarize(mu = mean(n),
            sig = sd(n))
sent_stats_B <- 
  data.frame(sum_sent$sentiment,
             sum_sent$mu , 
             sum_sent$sig)
sent_stats_B


#____________________________________________________



library(bayesAB)

snt <- unique(stuff_sentiment_A$sentiment)
sent_stats <- data.frame()
for (i in snt) {
  sent_A <- stuff_sentiment_A %>% 
    filter(sentiment == i)
  sent_B <- stuff_sentiment_B %>%
    filter(sentiment == i)
  AB1 <- bayesTest(
    sent_A$n,
    sent_B$n,
    priors = c(
      "mu" = 0,
      "lambda" = 1,
      "alpha" = 1,
      "beta" = 1
    ),
    distribution = 'normal'
  )
  cat("Sentiment = ", i)  
  #print(AB1)
  #summary(AB1)
  #print(plot(AB1))
  
  # Load the necessary library
  library(ggplot2)
  
  # Example data vectors
  Choice_A <- AB1$posteriors$Mu$A  
  # Generate some normal data
  Choice_B <- AB1$posteriors$Mu$B  
  # Generate some normal data 
  
  # Create a data frame for ggplot
  data <- data.frame(value = c(data1, data2),
                     group = 
                       factor(rep(c("Choice A",
                                    "Choice B"), 
                                  each = 100)))
  
  # Create the density plot
  p <- ggplot(data, aes(x = value, group = group)) +
    geom_density(aes(linetype = group), size = 1) +  
    # Use different linetypes for each group
    scale_linetype_manual(values = c("solid", "dashed")) +  
    # Assign solid and dashed lines
    facet_wrap(~ group, scales = "free_y") + 
    # Facet by group, allowing free scales on the y-axis
    labs(title = "Density Plot by Group",
         x = i, y = "Density") +
    theme_minimal() +  
    # Using a minimal theme for better visibility
    theme(text = element_text(color = "black"),  
          # Ensure text is black for contrast
          panel.grid.major = element_blank(),   
          # Remove major grid lines
          panel.grid.minor = element_blank(),  
          # Remove minor grid lines
          panel.background = element_blank(),   
          # Remove panel background
          plot.background = element_blank(),    
          # Transparent plot background
          strip.background = element_blank(),  
          # Transparent facet strip background
          strip.text = element_text(color = "black")) 
  
  print(p)
  
  
}

#______________________________________________________



# Load the necessary library
library(ggplot2)

# Example data vectors
data1 <- rnorm(100, mean = 50, sd = 10) 
# Generate some normal data
data2 <- rnorm(100, mean = 60, sd = 15)  
# Generate some normal data with different parameters

# Create a data frame for ggplot
data <- data.frame(value = c(data1, data2), group =
                     factor(rep(c("Data1", 
                                  "Data2"), 
                                each = 100)))

# Create the density plot
ggplot(data) +
  geom_density(aes(x = value, fill = group, 
                   color = group), alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red")) +  
  # Set custom fill colors for the groups
  scale_color_manual(values = c("blue", "red")) + 
  # Set custom line colors for the groups
  labs(title = "Density Plot",
       x = "Value",
       y = "Density") +
  theme_minimal()  
# Using a minimal theme for better visibility

#_____________________________________________________



# Load the necessary library
library(ggplot2)

# Example data vectors
Choice_A <- AB1$posteriors$Mu$A 
# Generate some normal data
Choice_B <- AB1$posteriors$Mu$B 
# Generate some normal data with different parameters

# Create a data frame for ggplot
data <- data.frame(value = c(data1, data2),
                   group =
                     factor(rep(c("Choice A", "Choice B"), 
                                each = 100)))

# Create the density plot
ggplot(data, aes(x = value, group = group)) +
  geom_density(aes(linetype = group), size = .3) +  
  # Different linetypes for each group
  scale_linetype_manual(values =
                          c("solid", "dashed")) +  
  # Assign solid and dashed lines
  labs(title = "Density Plot",
       x = "Value", 
       y = "Density") +
  theme_minimal() +  
  # Using a minimal theme for better visibility
  theme(text = element_text(color = "black"),  
        # Ensure text is black for contrast
        panel.grid.major = element_blank(),  
        # Remove major grid lines
        panel.grid.minor = element_blank(),  
        # Remove minor grid lines
        panel.background = element_blank(),  
        # Remove panel background
        plot.background = element_blank())    
# Transparent plot background


#________________________________________

library(bayesAB)
library(readr)

priors <- 
  read_csv(
    "DATABASE_priors_for_NLP_case_study.csv"
  )[1:10,c(2,5:8)]



snt <- unique(stuff_sentiment_A$sentiment)
sent_stats <- data.frame()
for (i in c(
  "anger",
  joy,
  "anticipation",
  "disgust",
  "fear", 
  "sadness",
  "surprise",
  "trust",
  "negative",
  "positive"
)) {
  sent_A <-
    stuff_sentiment_A %>% filter(sentiment == i)
  sent_B <- 
    stuff_sentiment_B %>% filter(sentiment == i)
  prr <- priors %>% filter(Sentiment == i)
  AB1 <- bayesTest(
    sent_A$n,
    sent_B$n,
    priors = c(
      "mu" = prr$mu,
      "lambda" = prr$lambda,
      "alpha" = prr$alpha,
      "beta" = prr$beta
    ),
    distribution = 'normal'
  )
  
  
  cat("Sentiment = ", i)  
  #print(AB1)
  #summary(AB1)
  #print(plot(AB1))
  
  # Load the necessary library
  library(ggplot2)
  
  # Example data vectors
  data1 <- AB1$posteriors$Mu$A 
  # Generate some normal data
  data2 <- AB1$posteriors$Mu$A  
  # Generate some normal data with different parameters
  
  # Create a data frame for ggplot
  data <- data.frame(value = c(data1, data2),
                     group = 
                       factor(rep(c("Data1",
                                    "Data2"), 
                                  each = 100)))
  
  # Create the density plot
  ggplot(data, aes(x = value, 
                   fill = group, 
                   alpha = 0.5)) +
    geom_density(adjust = 1.5) +  
    # Adjust parameter on how smooth you want the curve to be
    scale_fill_manual(values = c("blue", "red")) +  
    # Set custom colors for the groups
    scale_alpha_manual(values = c(0.5, 0.5)) + 
    # Set transparency level
    labs(title = "Density Plot", 
         x = "Value", 
         y = "Density") +
    theme_minimal()  
  # Using a minimal theme for better visibility
  
  
}

#________________________________________________________



library(kableExtra)
library(tidyverse)
library(tidytext)
library(tokenizers)
library(tm)
library(readr)
library(knitr)


reviews <- read_csv("DATABASE_reviews.csv") %>% 
  select(UserId, Score, Text)

set.seed(123)
mkt <- sample(seq_len(nrow(reviews)), 
              size = nrow(reviews) / 2)
A <- reviews %>% filter(Score < 4)
B <- reviews %>% filter(Score >= 4)

Encoding(A$Text) = "UTF-8"
text_stuff <- tokenize_words(A$Text) %>%
  unlist() %>%
  as.data.frame()
colnames(text_stuff) <- "word"
stuff_sentiment_A <-
  text_stuff %>%  
  inner_join(get_sentiments("nrc"), by = "word")  %>% 
  count(sentiment, word)

sum_sent <- 
  stuff_sentiment_A %>% 
  group_by(sentiment) %>% 
  summarize(mu = mean(n), sig = sd(n))
sent_stats_A <-
  data.frame(sum_sent$sentiment,
             sum_sent$mu ,
             sum_sent$sig)
sent_stats_A %>% kbl(
  caption="Sentiment A Statistics")


Encoding(B$Text) = "UTF-8"
text_stuff <- tokenize_words(B$Text) %>%
  unlist() %>%
  as.data.frame()
colnames(text_stuff) <- "word"
stuff_sentiment_B <-
  text_stuff %>%
  inner_join(get_sentiments("nrc"), by = "word")  %>% 
  count(sentiment, word)

sum_sent <- stuff_sentiment_B %>% 
  group_by(sentiment) %>% 
  summarize(mu = mean(n), sig = sd(n))
sent_stats_B <- 
  data.frame(sum_sent$sentiment, 
             sum_sent$mu , 
             sum_sent$sig)
sent_stats_B %>% kbl(caption="Sentiment B Statistics")






