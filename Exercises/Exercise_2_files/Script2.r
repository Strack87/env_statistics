
#setting up the directory
base.path.file <- ("c:/Users/andrei.strachinaru/OneDrive/BOKU/S2026/Environmental_Statistics/5_Exercises/Exercises/Exercise_2_files/")

#setting up the file
q1 <- scan(paste0(base.path.file, "file0.txt"))

cat("Lenght of data is: ", length(q1), ".", sep="")

#define year 1976
year_1976 <- q1[1:366]
cat("Number of days of year 1976 is:", length(year_1976), "days.")

#define the quantile q95_1976 and store it
q95_1976 <- quantile(year_1976, probs = 0.95)
cat("The threshold for quantile q95_1976 is: ", q95_1976,"m³/s.", sep="")

#define the values that overpasses the required quantile
extreme_values_1976 <- which(year_1976 > q95_1976)
cat("The values above the threshold for year 1976 are:", year_1976[extreme_values_1976])

#plot the Discharge of year 1976
plot(year_1976,
    type = "l")

#plot the points that are above q95_1976
points(extreme_values_1976, year_1976[extreme_values_1976],
    pch = "*")

#plot the Discharge of year 1976
plot(year_1976,
    type = "l",
    lwd = 2, #bolding the line
    lty = 1, #dashing the line
    col = "lightblue", #coloring the line
    xlab = "Days of year 1976",
    ylab = "Discharge (m³/s)",
    main = "Pegel Ibm/Moosache - Discharge gauge",)


#define the maximum line at quantile q95_1976
abline(h = q95_1976, 
    col = "red",
    lwd = 2)
text(1, 0.95, 
    "quantile 0.95",
    col = "red",
    adj = c(-1, -.1) )

#plot the points
points(extreme_values_1976, year_1976[extreme_values_1976],
    pch = "*",
    col = "red",
    lwd = 2)

legend("topright",
    legend = c("Discharge", 
                  paste0("0.95 Quantile (", round(q95_1976, 3), " m³/s)"),
                  "Extreme values"),
    col    = c("steelblue", "darkred", "red"),
    lty    = c(1, 2, NA),
    pch    = c(NA, NA, 8),
    lwd    = c(1.5, 1.2, 2),
    y.intersp = 1.2)

#defining year 1967 form the file
year_1977 <- q1[367:732]
cat("NUmber of days of year 1977 is:",length(year_1977), "days.")

#define the quantile q95_1976 for year 1977 and store it in a variable
q95_1977 <- quantile(year_1977, probs = 0.95)
cat("The threshold for quantile q95 for year 1977 is: ", q95_1977, "m³/s.", sep="")

#define the extreme values that overpass q95_1977
extreme_values_1977 <- which(year_1977 > q95_1977)
cat("The values above the threshold for year 1976 are:", year_1977[extreme_values_1977], ".")

#plot the Discharge of year 1976
plot(year_1977,
    type = "l",
    lwd = 2, #bolding the line
    lty = 1, #dashing the line
    col = "lightblue", #coloring the line
    xlab = "Days of year 1977",
    ylab = "Discharge (m³/s)",
    main = "Pegel Ibm/Moosache - Discharge gauge",)


#define the maximum line at quantile q95_1976
abline(h = q95_1977, 
    col = "red",
    lwd = 2)
text(1, 0.95, 
    "quantile 0.95",
    col = "red",
    adj = c(-4, -2) )

#plot the points
points(extreme_values_1977, year_1977[extreme_values_1977],
    pch = "*",
    col = "red",
    lwd = 2)

legend("topright",
    legend = c("Discharge", 
                  paste0("0.95 Quantile (", round(q95_1977, 3), " m³/s)"),
                  "Extreme values"),
    col    = c("steelblue", "darkred", "red"),
    lty    = c(1, 2, NA),
    pch    = c(NA, NA, 8),
    lwd    = c(1.5, 1.2, 2),
    y.intersp = 1.5)

