
### Setup ######################################################################

library(xts)
library(qrmdata)
library(qrmtools)
library(moments)


### Stock prices ###############################################################

## Dow Jones stock price data
data("DJ_const")
str(DJ_const)

## We extract a time period and take the 3 stocks
DJdata <- DJ_const['2000-01-01/2015-12-31', 11:13]

## Use plot for zoo objects to get multiple plots
plot.zoo(DJdata, xlab = "Time", main = "DJ (3 component series)")
X <- returns(DJdata) # or diff(log(DJdata))[-1,]
head(X)
plot.zoo(X, xlab = "Time", main = "Log-returns of 3 DJ component series")


## Calculate descriptive statistics for each return series #####################
desc_stats <- data.frame(
  Statistic = c("N (observations)", "Mean (%)", "Std. Deviation (%)", "Minimum (%)", 
                "Maximum (%)", "Skewness", "Kurtosis"),
  Stock1 = c(
    nrow(X),
    mean(X[,1], na.rm = TRUE) * 100,
    sd(X[,1], na.rm = TRUE) * 100,
    min(X[,1], na.rm = TRUE) * 100,
    max(X[,1], na.rm = TRUE) * 100,
    skewness(X[,1]),
    kurtosis(X[,1])
  ),
  Stock2 = c(
    nrow(X),
    mean(X[,2], na.rm = TRUE) * 100,
    sd(X[,2], na.rm = TRUE) * 100,
    min(X[,2], na.rm = TRUE) * 100,
    max(X[,2], na.rm = TRUE) * 100,
    skewness(X[,2]),
    kurtosis(X[,2])
  ),
  Stock3 = c(
    nrow(X),
    mean(X[,3], na.rm = TRUE) * 100,
    sd(X[,3], na.rm = TRUE) * 100,
    min(X[,3], na.rm = TRUE) * 100,
    max(X[,3], na.rm = TRUE) * 100,
    skewness(X[,3]),
    kurtosis(X[,3])
  )
)

## Set column names to the actual stock names
colnames(desc_stats) <- c("Statistic", colnames(X))

## Format the table with appropriate decimal places
desc_stats_formatted <- desc_stats
# Keep N as integer
desc_stats_formatted[1, 2:4] <- format(desc_stats[1, 2:4], big.mark = ",")
# Round percentages to 4 decimal places
desc_stats_formatted[2:5, 2:4] <- format(round(desc_stats[2:5, 2:4], 4), nsmall = 4)
# Round skewness and kurtosis to 4 decimal places
desc_stats_formatted[6:7, 2:4] <- format(round(desc_stats[6:7, 2:4], 4), nsmall = 4)

## Print the formatted table
print(desc_stats_formatted, right = TRUE)


### 3*3 Scatter Plot Matrix ####################################################
pairs(as.matrix(X),
      main = "Scatter Plot Matrix of Log Returns",
      pch = 20,
      col = rgb(0, 0, 1, 0.3),  # Semi-transparent blue points
      cex = 0.5)


### Ljung-Box Test of Serial Independence ######################################

## Perform Ljung-Box test on returns and absolute returns
LB.raw <- apply(X, 2, Box.test, lag = 10, type = "Ljung-Box")
LB.abs <- apply(abs(X), 2, Box.test, lag = 10, type = "Ljung-Box")

## Extract p-values
p.LB.raw <- sapply(LB.raw, `[[`, "p.value")
p.LB.abs <- sapply(LB.abs, `[[`, "p.value")

## Extract test statistics
stat.LB.raw <- sapply(LB.raw, `[[`, "statistic")
stat.LB.abs <- sapply(LB.abs, `[[`, "statistic")

## Create summary table
LB.results <- data.frame(
  Stock = colnames(X),
  Returns_Statistic = round(stat.LB.raw, 4),
  Returns_P_value = round(p.LB.raw, 6),
  Returns_Significant = ifelse(p.LB.raw < 0.05, "Yes", "No"),
  Abs_Returns_Statistic = round(stat.LB.abs, 4),
  Abs_Returns_P_value = round(p.LB.abs, 6),
  Abs_Returns_Significant = ifelse(p.LB.abs < 0.05, "Yes", "No")
)

## Print results
print(LB.results, row.names = FALSE)


## Auto-correlations of returns and absolute returns ###########################
par(mfrow = c(1, 3))
acf(X$HD, main = "ACF: Returns - HD", lag.max = 30, col = "blue", lwd = 2)
acf(X$IBM, main = "ACF: Returns - IBM", lag.max = 30, col = "blue", lwd = 2)
acf(X$INTC, main = "ACF: Returns - INTC", lag.max = 30, col = "blue", lwd = 2)
par(mfrow = c(1, 1))

par(mfrow = c(1, 3))
acf(abs(X$HD), main = "ACF: |Returns| - HD", lag.max = 30, col = "darkgreen", lwd = 2)
acf(abs(X$IBM), main = "ACF: |Returns| - IBM", lag.max = 30, col = "darkgreen", lwd = 2)
acf(abs(X$INTC), main = "ACF: |Returns| - INTC", lag.max = 30, col = "darkgreen", lwd = 2)
par(mfrow = c(1, 1))

## Monthly Volatilities ########################################################

monthly_vol_HD <- apply.monthly(X$HD, sd)
monthly_vol_IBM <- apply.monthly(X$IBM, sd)
monthly_vol_INTC <- apply.monthly(X$INTC, sd)

## Combine into one xts object
monthly_vol <- merge(HD = monthly_vol_HD, 
                     IBM = monthly_vol_IBM, 
                     INTC = monthly_vol_INTC)

## Convert to percentage
monthly_vol <- monthly_vol * 100

## Plot combined (all three in one figure)
plot.zoo(monthly_vol,
         main = "Monthly Volatility of Returns",
         xlab = "Time",
         ylab = "Volatility (%)",
         col = c("blue", "red", "darkgreen"),
         lwd = 2,
         plot.type = "single")
legend("topright",
       legend = colnames(monthly_vol),
       col = c("blue", "red", "darkgreen"),
       lty = 1,
       lwd = 2,
       bty = "n")


### Q-Q Plots Against Normal Distribution Using qrmtools #######################

mu.HD <- mean(X$HD, na.rm = TRUE)
sig.HD <- sd(X$HD, na.rm = TRUE)

mu.IBM <- mean(X$IBM, na.rm = TRUE)
sig.IBM <- sd(X$IBM, na.rm = TRUE)

mu.INTC <- mean(X$INTC, na.rm = TRUE)
sig.INTC <- sd(X$INTC, na.rm = TRUE)

## Option 1: Three separate Q-Q plots
par(mfrow = c(1, 3))
qq_plot(X$HD, FUN = function(p) qnorm(p, mean = mu.HD, sd = sig.HD),
        main = "Q-Q Plot: HD Returns vs Normal", col = "blue", pch = 20)
qq_plot(X$IBM, FUN = function(p) qnorm(p, mean = mu.IBM, sd = sig.IBM),
        main = "Q-Q Plot: IBM Returns vs Normal", col = "blue", pch = 20)
qq_plot(X$INTC, FUN = function(p) qnorm(p, mean = mu.INTC, sd = sig.INTC),
        main = "Q-Q Plot: INTC Returns vs Normal", col = "blue", pch = 20)

par(mfrow = c(1, 1))
