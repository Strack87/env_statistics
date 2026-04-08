##River Nile at Ashwan ####

#The Nile data-set data(Nile) contains measurements of the annual flow of the river Nile at Ashwan from 1871-1970.
# Your task is to analyse the period 1889 to 1959. Use a command like 
#mynile <- window(Nile, start = 1889, end = 1959)
#to subset the data and keep the time-series structure. See also ?Nile for more information and some examples 
#how this series could be analysed in R.


## 1. Descriptive Statistics ####
# load database
mynile <- window(Nile, start = 1889, end = 1959)
#plot to visualise the data
plot(mynile)

#define database as data.frame to split columns as year and flow
mynile_df <- data.frame(
  year = as.numeric(time(mynile)),
  flow = as.numeric(mynile)
)

#separate the flow for year 1919
flow_1919 <- mynile[time(mynile) == 1919]

#read the mean_annual_flow
summary(mynile_df$flow)

#second method to determine the mean_annual_flow
median_annual_flow <- median(mynile_df$flow)

#plot the year and discharge
plot(mynile_df, 
    type = "l",
    col = "blue",
    xlab = "Year",
    ylab = "Discharge (m³/s)",
    main = "River Nile at Ashwan Annual Discharge"
  )
n <- 3
lines(mynile_df$year, filter(mynile_df$flow, rep(1/n, n)), col = "red", lty = 2, lwd = 2)
# add extra orange horizontal line at the median level
abline(h=median_annual_flow,
  col = "orange",
  lwd = 2)
# fit a linear model: flow ~ year
lm_nile <- lm(flow ~ year, data = mynile_df)
summary(lm_nile)  # slope is the 'year' coefficient
round(coefficients(lm_nile), 3)

abline(lm_nile,
  col = "green")
# add a text to display the mean_annual_flow as MAF
text(1887, median_annual_flow - 30,
  "MAF = 848 m³/s",
  col = "orange",
  adj = c(0, 0))
#mark the flow for year 1919 with a start
points(mynile_df$year[mynile_df$year == 1919], mynile_df$flow[mynile_df$year == 1919],
    pch = "*",
    col = "red",
    cex = 2)


## 2. Linear Trend and Filtering ####

# apply a 3-year moving average filter
mynile_ma3 <- filter(mynile_df$flow, filter = rep(1/3, 3), sides = 2)

# smallest value of the moving average
mynile_min <- round(min(mynile_ma3, na.rm = TRUE),3)


# fit a linear model: flow ~ year
lm_nile <- lm(flow ~ year, data = mynile_df)
summary(lm_nile)  # slope is the 'year' coefficient
round(coefficients(lm_nile), 3)

abline(lm_nile,
  col = "green")

# add dotted red line for moving average
lines(mynile_ma3, col = "red", lwd = 2, lty = 2)

# add green line for linear trend

# add legend
legend("topright",
  legend = c("Annual flow", "Median", "3-yr moving avg", "Linear trend"),
  col    = c("blue", "orange", "red", "green"),
  lty    = c(1, 1, 2, 1),
  lwd    = 2)


## 3. Autocorrelation
#what is the autocorrelation of the series by lag 3?

afc_result <- acf(mynile_df$flow)
lag_3_acf <- afc_result$acf[4]                  
negative_min_lag <- min(which(afc_result$acf < 0)) - 1  
negative_min_lag <- min(which(afc_result$acf < 0)) - 1  


pafc_result <- pacf(mynile_df$flow)
lag_6_pafc <- round(pafc_result$acf[6],3)                      

## 4. ARIMA Models
arima322 <- arima(mynile_df$flow, order = c(3,2,2))
ar1 <- round(arima322$coef["ar1"], 3)                                  
aic_model <- round(AIC(arima322),3)                                     

tsdiag(arima322)
#install.packages("forecast")
library(forecast)
plot(forecast(arima322, h = 3))

tinytex::is_tinytex()
