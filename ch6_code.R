library(bayesAB)
library(tidyverse)
library(readr)

coin <- 
  read_csv("DATABASE_xisBTC_yisETH_coin.csv") %>% 
  select(btc=High.x, eth=High.y, date=Date)

# Example price series for Asset A and Asset B
price_btc <-  coin$btc
#  c(100, 102, 105, 110, 108, 115) # Example prices for Asset A
price_eth <- coin$eth
# c(50, 52, 55, 60, 58, 65)      # Example prices for Asset B

compute_incremental_roi <- function(prices) {
  # Calculate ROI between each successive data point
  roi <- diff(prices) / head(prices, -1)
  return(roi)
}

# Compute ROI for both series
roi_a <- compute_incremental_roi(price_btc)
roi_b <- compute_incremental_roi(price_eth)


df_btc <- data.frame(roi_a,"BTC")
colnames(df_btc) <- c("ROI", "Cryptocurrency")
df_eth <- data.frame(roi_b,"ETH")
colnames(df_eth) <- c("ROI", "Cryptocurrency")
df_coin <- rbind(df_btc,df_eth)

df_coin %>% ggplot(aes(ROI,color=Cryptocurrency))+
  geom_density(lwd=1)+theme_minimal()

# Perform the Bayes A/B test
ab_test_result <- bayesTest(
  A = roi_a,
  B = roi_b,
  priors = c("mu" = 0, 
             "lambda" = 1, 
             "alpha" = 2, 
             "beta" = .05),
  n_samples = 1e+05,
  dist = 'normal'
)

plot(ab_test_result)
#summary(ab_test_result)

#___________________________________________________________


library(tidyverse)
library(reshape2)
library(readr)


portfolio <- 
  read_csv("DATABASE_portfolio.csv")[-60,] 
# obs 60 is an average
portfolio[is.na(portfolio)] <- 0

compute_incremental_roi <- function(prices) {
  # Calculate ROI between each successive data point
  roi <- diff(prices) / head(prices, -1)
  return(roi)
}

roi <- 
  apply(portfolio[,2:10], 2, compute_incremental_roi) %>% 
  as.data.frame() %>% 
  cbind(portfolio$Year[2:59]) %>% 
  .[-1,]

# Define shades of grey and linetypes
grey_shades <- c("black",
                 "grey80", "grey50",
                 "grey20","grey60","grey30")
line_types <- c("solid", "longdash",
                "dotted", "twodash",
                "dotdash", "F1")

melt(roi[,1:6,10]) %>% 
  ggplot(aes(value, color=variable,
             linetype = variable)) +
  geom_density(size=.5) +
  scale_color_manual(values = grey_shades) +
  scale_linetype_manual(values = line_types) +
  theme_minimal() + xlim(-.5,.5) +
  theme(legend.position = "top",
        text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Densities of Asset Price ROIs",
       x = "Value", y = "Density")

#______________________________________________________

library(tidyverse)
library(kableExtra)
library(bayesAB)


library(fitdistrplus)
fitto <- tibble()
for(i in 1:6){
  fit <- tibble(
    colnames(roi)[i],
    descdist(roi[,i], 
             method='unbiased',
             print=, graph=F)$mean,
    descdist(roi[,i], 
             method='unbiased',
             print=F, graph=F)$sd,
    descdist(roi[,i],
             method='unbiased',
             print=F, graph=F)$skewness,
    descdist(roi[,i],
             method='unbiased',
             print=F, graph=F)$kurtosis
  )
  
  fitto <- rbind(fit, fitto)
}

colnames(fitto) <- c("Asset", 
                     "Mean", "SD", 
                     "Skewness", "Kurtosis")

fitto %>% kable(
  caption = "Fit statistics of
  each asset's time series", 
  digits=3,
  ,booktabs=T
)




table <- tibble()
colnames(table) <- c("assetA", 
                     "assetB", 
                     "meanA",
                     "meanB")
for (j in 1:5) {
  for (k in (j + 1):6) {
    roi_test_result <- bayesTest(
      A = roi[, j],
      B = roi[, k],
      priors = c(
        "mu" = 0,
        "lambda" = 1,
        "alpha" = 2,
        "beta" = .05
      ),
      n_samples = 1e+05,
      dist = 'normal'
    )
    
    line <- data.frame(colnames(roi)[j],
                       colnames(roi)[k], mean(t(as.numeric(
                         unlist(roi_test_result$posteriors$Mu$A)
                       ))), mean(t(as.numeric(
                         unlist(roi_test_result$posteriors$Mu$B)
                       ))))
    colnames(line) <- c("assetA",
                        "assetB", 
                        "meanA", 
                        "meanB")
    table <- rbind(table, line)
    
  }
}

result <- max(unlist(table[,3:4]))
winner <- table %>% 
  filter(meanA == result) %>% .[1,c(1,3)]
winner %>% kable(
  caption="The asset class with the highest ROI", 
  digits=5,
  col.names = 
    c("Asset", "ROI"),
  booktabs=T
)




