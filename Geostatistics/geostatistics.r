# Data Import ####

# Load the geoR package, which provides all geostatistical tools used below.
# install.packages("geoR")  # run once if not yet installed
library(geoR)

# Set working directory to the folder containing this Rmd document
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# Read the KOLA data from a tab-separated text file with a header row.
# The file must be in the same directory as this Rmd file.
KOLA95 <- read.table(
  "KOLA95_Chor.txt",
  sep    = "\t",   # columns are separated by tabs
  header = TRUE    # first row contains column names
)

# Identify the column index of "Al" to use in as.geodata()
al_col <- which(names(KOLA95) == "Al")
cat("Column index of Al:", al_col, "\n")
cat("Total number of samples:", nrow(KOLA95), "\n")

# Convert the data frame to a geoR "geodata" object.
# coords.col = 2:3 uses columns XCOO and YCOO as spatial coordinates (UTM metres).
# data.col   = al_col selects the Al column as the target variable.
s100 <- as.geodata(
  KOLA95,
  coords.col = 2:3,          # XCOO (col 2) and YCOO (col 3)
  data.col   = al_col        # Al concentrations (single column — no data.names needed)
)

# Quick summary of the geodata object: spatial extent + data statistics
summary(s100)

# Explorative Statistics — Choosing a Transformation ####

# Histogram of the raw Al concentrations
hist(
  s100$data,
  breaks = 30,
  main   = "Histogram of Al (original scale)",
  xlab   = "Al concentration (mg/kg)",
  col    = "steelblue",
  border = "white"
)
# Add a vertical line at the mean to illustrate the skewness
abline(v = mean(s100$data), col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste("Mean =", round(mean(s100$data), 1)),
       col = "red", lty = 2, lwd = 2, cex = 0.9)

# Log-transform the Al values and inspect the resulting distribution
hist(
  log(s100$data),
  breaks = 30,
  main   = "Histogram of log(Al)",
  xlab   = "log(Al concentration (mg/kg))",
  col    = "darkorange",
  border = "white"
)
abline(v = mean(log(s100$data)), col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste("Mean =", round(mean(log(s100$data)), 3)),
       col = "red", lty = 2, lwd = 2, cex = 0.9)

# Decision: the log-transformed values are approximately symmetric (bell-shaped),
# while the original values are strongly right-skewed.
# --> We apply the log-transformation before geostatistical modelling.
s100$data <- log(s100$data)

# Verify the transformation was applied correctly
cat("Summary of log(Al):\n")
print(summary(s100$data))

# Spatial Plot of AI Values ####
# Build the 4-panel plot manually so we can add legends and annotations.
# top-left: data locations coloured by quartile; top-right: log(Al) vs Easting;
# bottom-left: log(Al) vs Northing; bottom-right: histogram of log(Al).

x_coord <- s100$coords[, 1]         # Easting (m)
y_coord <- s100$coords[, 2]         # Northing (m)
z_val   <- s100$data                # log(Al) in log(mg/kg)

# Classify data into quartile classes for colour coding
qbreaks <- quantile(z_val, probs = c(0, 0.25, 0.5, 0.75, 1))
qclass  <- cut(z_val, breaks = qbreaks, include.lowest = TRUE,
               labels = c("Q1 (low)", "Q2", "Q3", "Q4 (high)"))
qcols   <- c("blue", "green3", "orange", "red")

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# (1) Top-left: data locations coloured by quartile of log(Al)
plot(x_coord, y_coord,
     col  = qcols[as.integer(qclass)],
     pch  = 16,
     xlab = "Easting (m)",
     ylab = "Northing (m)",
     main = "Sample locations by log(Al) quartile")
legend("topright", legend = levels(qclass),
       col = qcols, pch = 16, cex = 0.8,
       title = "log(Al) [log(mg/kg)]",
       bty = "n",
       x.intersp = 0.3, y.intersp = 0.9)

# (2) Top-right: log(Al) vs Easting, with lowess trend
plot(x_coord, z_val,
     pch  = 16, col = "steelblue",
     xlab = "Easting (m)",
     ylab = "log(Al) [log(mg/kg)]",
     main = "log(Al) vs Easting")
lines(lowess(x_coord, z_val), col = "red", lwd = 2)
legend("topleft", legend = c("Samples", "lowess trend"),
       col = c("steelblue", "red"), pch = c(16, NA), lty = c(NA, 1),
       lwd = c(NA, 2), bty = "n", cex = 0.8)

# (3) Bottom-left: log(Al) vs Northing, with lowess trend
plot(y_coord, z_val,
     pch  = 16, col = "steelblue",
     xlab = "Northing (m)",
     ylab = "log(Al) [log(mg/kg)]",
     main = "log(Al) vs Northing")
lines(lowess(y_coord, z_val), col = "red", lwd = 2)
legend("topright", legend = c("Samples", "lowess trend"),
       col = c("steelblue", "red"), pch = c(16, NA), lty = c(NA, 1),
       lwd = c(NA, 2), bty = "n", cex = 0.8)

# (4) Bottom-right: histogram of log(Al) with mean marked
z_mean <- mean(z_val)
hist(z_val,
     breaks = 20,
     col    = "grey80",
     border = "white",
     xlab   = "log(Al) [log(mg/kg)]",
     main   = "Histogram of log(Al)")
abline(v = z_mean, col = "red", lty = 3, lwd = 2)
legend("topright",
       legend = c(sprintf("Mean = %.3f", z_mean)),
       col = "red", lty = 3, lwd = 2, bty = "n", cex = 0.8)

par(mfrow = c(1, 1))   # reset layout

# One panel per quartile: isolates the spatial distribution of each class so
# low- and high-value clusters can be inspected independently. Background grey
# points show all other samples for context.
qlevels <- levels(qclass)
qranges <- sprintf("(%.3f, %.3f]", qbreaks[-5], qbreaks[-1])
qdescr  <- c(
  "lowest 25% of log(Al) values — typically found in the NW part of the study area",
  "second quartile of log(Al) — transitional / background concentrations",
  "third quartile of log(Al) — moderately elevated concentrations",
  "highest 25% of log(Al) — hotspots concentrated in the SE part of the study area"
)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

for (i in seq_along(qlevels)) {
  sel <- qclass == qlevels[i]

  # Background: all samples in light grey for spatial context
  plot(x_coord, y_coord,
       col  = "grey85",
       pch  = 16,
       cex  = 0.6,
       xlab = "Easting (m)",
       ylab = "Northing (m)",
       main = paste0(qlevels[i], " — ", qdescr[i]))

  # Highlight only the samples belonging to this quartile
  points(x_coord[sel], y_coord[sel],
         col = qcols[i], pch = 16, cex = 1.1)

  legend("topright",
         legend = c(paste0(qlevels[i], " ", qranges[i]), "Other samples"),
         col    = c(qcols[i], "grey85"),
         pch    = 16,
         bty    = "n",
         cex    = 0.8,
         title  = "log(Al) [log(mg/kg)]")

  mtext(sprintf("n = %d of %d samples", sum(sel), length(z_val)),
        side = 3, line = 0.2, cex = 0.75, col = "grey30")
}

par(mfrow = c(1, 1))   # reset layout
