#Exercise 1.1, print results
p <- numeric(50)
s <- numeric(50)
for (x in 1:50){
  p[x] <- 2 ^ x
  s[x] <- x ^ x
}
print(p)
print(s)
# Verify if 2 ^ x = x *2
for (x in 1:50){
  if( 2 ^ x == x * 2){
    cat("x =", x, "\n")
  }
}
# Exercise 1.2
x <- seq(0, 2 * pi, by = 0.1)
sinu = round(sin(x), 3)
cosi = round(cos(x), 3)
tang_r = tan(x)
tang_div = sin(x)/cos(x)
diff_tan = tang_r - tang_div

cat("Lenght: sinu =", length(sinu), "| cosi =", length(cosi),"| tang_r =", 
                      length(tang_r), "| tang_div =", length(tang_div), "| diff_tan =", length(diff_tan) )

results <- data.frame(
  x = round(x, 3),
  sinu = round(sin(x), 3),
  cosi = round(cos(x), 3),
  tang_r = tan(x),
  tang_div = sin(x)/cos(x),
  diff_tan = tang_r - tang_div
)
print(results)

compare <- results$tang_r == results$tang_div
count_equal <- sum(compare)
cat("There are", count_equal, "out of", length(x), "values that are equal.")

difference <- abs(results$tang_r - results$tang_div)
max_difference <- max(difference)
cat("Max difference between tang_r and tang_div is", max_difference)

#Exercise 1.3
if (!require("lfstat")) install.packages("lfstat")
if (!require("DT")) install.packages("DT")

library(lfstat)
library(DT)

data(ngaruroro)
streamflow <- ngaruroro

knitr::kable(head(streamflow, 10))
DT::datatable(streamflow)
str(streamflow)
head(streamflow)
tail(streamflow)
class(streamflow)

flow_1999 <- streamflow[streamflow$year == 1999, ]
length(flow_1999$day)
plot(flow_1999$day, flow_1999$flow,
  type = "h",
  main = "Flow record of year 1999",
  xlab = "Day",
  ylab = "Q [m3/s]"
)

plot(flow_1999)

mean_flow <- mean(flow_1999$flow, na.rm = "TRUE")
cat("Mean flow value is ", round(mean_flow, 4), ".", sep="")

max_flow <- max(flow_1999$flow)
max_day_index <- which.max(flow_1999$flow)
max_day <- flow_1999$day[max_day_index]
cat("The annual maximum flow recorded on ", flow_1999$day[max_day_index],".",flow_1999$month[max_day_index],".", 
flow_1999$year[max_day_index]," was ", flow_1999$flow[max_day_index], " m${3}$/s.",sep = "")
