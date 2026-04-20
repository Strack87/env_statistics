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
  " NW area",
  " transitional c%",
  " moderately elevated c%",
  " SE area"
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


# Empirical Variogram Analysis {#variogram} ####

## Variogram Cloud
cloud1 <- variog(s100, option = "cloud", max.dist = 400000)

# Widen the left margin so the expression-based y-label is fully visible.
# plot.variogram() ignores user pch/col, so draw an empty canvas and add the
# points ourselves — this guarantees the legend symbol matches the plot.
op <- par(mar = c(4, 5.2, 3, 1))
plot(cloud1,
     type = "n",
     main = "Variogram cloud — log(Al)",
     xlab = "Separation distance h (m)",
     ylab = expression((1/2) * (z(x[i]) - z(x[j]))^2 ~ "[log(mg/kg)"^2 * "]")
)
points(cloud1$u, cloud1$v, pch = 1, col = "black", cex = 0.4)
legend("topleft",
       legend = "Sample pair",
       pch    = 1,
       col    = "black",
       bty    = "n",
       cex    = 0.85,
       title  = "log(Al) variogram cloud")
par(op)   # restore previous margin settings


## Empirical (Binned) Variogram

bin1 <- variog(s100, uvec = seq(0, 400000, l = 11))

# plot.variogram() ignores user-supplied pch/col, so draw an empty canvas
# first and add the markers ourselves — this guarantees the legend matches.
plot(bin1,
     type = "n",
     main = "Empirical variogram — log(Al)",
     xlab = "Separation distance h (m)",
     ylab = expression(paste("Semivariance ", gamma, "(h)  [log(mg/kg)"^2, "]"))
)
points(bin1$u, bin1$v, pch = 16, col = "darkblue")

# Annotate each bin with the number of pairs it contains (key diagnostic:
# bins with few pairs are unreliable).
text(bin1$u, bin1$v,
     labels = bin1$n,
     pos    = 1,        # above each point
     cex    = 0.7,
     col    = "grey40")
legend("bottomright",
       legend = c("Binned semivariance", "Pair count above point"),
       col    = c("darkblue", "grey40"),
       pch    = c(16, NA),
       text.col = c("black", "grey40"),
       bty    = "n",
       cex    = 0.85,
       title  = "log(Al) variogram")

# Fitting a Theoretical Variogram Model {#variofit} ####
# Fit a nested nugget + spherical model to the empirical variogram.
# ini.cov.pars: initial values for c(sill sigma^2, range phi); nugget = tau^2 start
# weights = "npairs": weight each lag class by the number of pairs (more robust)
vfit <- variofit(
  bin1,
  cov.model  = "spherical",
  ini.cov.pars = c(0.15, 250000),  # initial sill and range
  nugget     = 0.05,               # initial nugget
  weights    = "npairs"
)

# Print the fitted parameters
print(vfit)

# Extract and report the key variogram parameters
nugget <- vfit$nugget
sill   <- vfit$cov.pars[1]   # partial sill (sigma^2)
range  <- vfit$cov.pars[2]   # range (phi)

cat("\n--- Fitted variogram parameters ---\n")
cat(sprintf("  Nugget  (tau^2):         %.4f\n", nugget))
cat(sprintf("  Partial sill (sigma^2):  %.4f\n", sill))
cat(sprintf("  Total sill:              %.4f\n", nugget + sill))
cat(sprintf("  Range   (phi):           %.0f m\n", range))
cat(sprintf("  Nugget/Sill ratio:       %.1f %%\n", 100 * nugget / (nugget + sill)))

# plot.variogram() ignores user pch/col, so build the plot manually
op <- par(mar = c(4, 5.2, 3, 1))
plot(bin1,
     type = "n",
     main = "Variogram model fit — log(Al)",
     xlab = "Separation distance h (m)",
     ylab = expression(paste("Semivariance ", gamma, "(h)  [log(mg/kg)"^2, "]"))
)
points(bin1$u, bin1$v, pch = 16, col = "darkblue")

# Fitted theoretical semivariogram (what the red line represents)
lines(vfit, col = "red", lwd = 2)

# Reference lines for the fitted parameters
abline(h = nugget,          col = "grey50", lty = 2)
abline(h = nugget + sill,   col = "grey50", lty = 2)
abline(v = range,           col = "grey50", lty = 2)

legend("bottomright",
       legend = c(
         "Empirical variogram (binned)",
         "Fitted model: nugget + spherical",
         sprintf("Nugget   tau^2 = %.4f", nugget),
         sprintf("Sill     tau^2+sigma^2 = %.4f", nugget + sill),
         sprintf("Range    phi = %.0f m", range)
       ),
       col = c("black", "red", "grey50", "grey50", "grey50"),
       pch = c(1, NA, NA, NA, NA),
       lty = c(NA, 1, 2, 2, 2),
       lwd = c(NA, 2, 1, 1, 1),
       bty = "n", cex = 0.85,
       title = "log(Al) variogram")
par(op)

# Spatial Prediction: Kriging {#kriging} ####

## Prediction Grid

# Determine the spatial extent of the sampling domain
x_range <- range(s100$coords[, 1])
y_range <- range(s100$coords[, 2])
cat("X range (Easting):  ", x_range, "\n")
cat("Y range (Northing): ", y_range, "\n")

# Create a 50×50 regular grid (2,500 prediction locations) across the study area.
# A coarser grid (l=50) is used to keep computation time reasonable;
# increase l (e.g. to 100) for a finer, smoother map.
pred.grid <- expand.grid(
  seq(x_range[1], x_range[2], l = 50),   # 50 equally spaced X values
  seq(y_range[1], y_range[2], l = 50)    # 50 equally spaced Y values
)
cat("Number of prediction grid points:", nrow(pred.grid), "\n")

# Visual check: plot the prediction grid overlaid on sample locations
plot(pred.grid, pch = ".", col = "grey70",
     xlab = "Easting (m)", ylab = "Northing (m)",
     main = "Prediction grid (grey) and sample locations (blue)")
points(s100$coords, pch = 16, col = "steelblue", cex = 0.6)
legend("topright",
       legend = c(sprintf("Prediction grid (n = %d)", nrow(pred.grid)),
                  sprintf("Sample locations (n = %d)", nrow(s100$coords))),
       col    = c("grey70", "steelblue"),
       pch    = c(20, 16),
       pt.cex = c(1.2, 0.9),
       bty    = "n",
       cex    = 0.85)

## Kriging Estimation
# Perform ordinary kriging using the fitted variogram model (vfit).
# krige.conv() applies global neighbourhood (all data used for each prediction).
# For large grids this can be slow — consider local neighbourhood for speed.
kc <- krige.conv(
  s100,
  loc   = pred.grid,
  krige = krige.control(obj.m = vfit)  # use the fitted variogram model
)

cat("Kriging complete.\n")
cat("Predicted values range (log scale):", round(range(kc$predict), 3), "\n")
cat("Back-transformed range (mg/kg):    ", round(range(exp(kc$predict)), 1), "\n")

# Build a colour palette and matching breaks over the full prediction range
n_col  <- 64
pal    <- hcl.colors(n_col, palette = "YlOrRd", rev = TRUE)
zlim   <- range(kc$predict)
breaks <- seq(zlim[1], zlim[2], length.out = n_col + 1)

# Split the device: wide map panel on the left, narrow colour bar on the right
layout(matrix(c(1, 2), nrow = 1), widths = c(5, 1))
op <- par(mar = c(4, 4, 3, 1))

# Main kriging map
image(kc,
      loc    = pred.grid,
      col    = pal,
      breaks = breaks,
      xlab   = "Easting (m)",
      ylab   = "Northing (m)",
      main   = "Kriging map — log(Al) [KOLA data]"
)
# Overlay the original sample locations so we can relate predictions to data
points(s100$coords, pch = 16, cex = 0.4, col = "black")

# --- Colour bar ---
# Small top/bottom margins so the bar extends nearly the full figure height,
# independent of the map panel's title/axis margins.
par(mar = c(4, 0.5, 3, 4))
plot.new()
plot.window(xlim = c(0, 1), ylim = zlim, xaxs = "i", yaxs = "i")
rect(0, breaks[-(n_col + 1)], 1, breaks[-1], col = pal, border = NA)
axis(4, las = 1, cex.axis = 0.8)
mtext("log(Al) [log(mg/kg)]", side = 4, line = 2.6, cex = 0.85)

# Restore layout
par(op)
layout(1)

# The kriging variance kc$krige.var quantifies prediction uncertainty.
# High variance occurs in data-sparse regions (edge effects, gaps in sampling).
image(kc,
      values = kc$krige.var,  # plot variance instead of predictions
      loc    = pred.grid,
      xlab   = "Easting (m)",
      ylab   = "Northing (m)",
      main   = "Kriging variance map — log(Al)"
)
points(s100$coords, pch = 16, cex = 0.4, col = "black")


# A filled contour plot provides an alternative, often cleaner visualisation.
contour(kc,
        filled      = TRUE,
        coords.data = s100$coords,
        color       = heat.colors,
        values      = kc$predict,
        main        = "Kriging map (contour) — log(Al)",
        xlab        = "Easting (m)",
        ylab        = "Northing (m)"
)

# Voluntary Exercise: Sensitivity of the Kriging Map to the Variogram Model {#voluntary}####


# IMPORTANT: we must NOT call variofit() here. variofit() re-optimises sill and
# range starting from our "initial" values, so (a), (b) and (c) would all
# converge to the same best-fit model and produce identical maps.
# Instead we pass the desired parameters DIRECTLY to krige.control() via
# cov.pars / nugget, which uses them as fixed values for kriging.

# --- Helper: kriging with a fully specified variogram (no fitting) ---
krige_fixed <- function(cov_pars, nugget_val, model = "spherical") {
  krige.conv(
    s100,
    loc   = pred.grid,
    krige = krige.control(
      cov.model = model,
      cov.pars  = cov_pars,   # c(sigma^2, phi) — partial sill and range
      nugget    = nugget_val  # tau^2
    )
  )
}

# Base parameters from the fitted model
s2   <- sill           # partial sill
phi  <- range          # range
tau2 <- nugget         # nugget

# (a) Spherical, no nugget: sill = total sill, same range
kc_a <- krige_fixed(cov_pars = c(s2 + tau2, phi),       nugget_val = 0)

# (b) Spherical, no nugget, half the sill
kc_b <- krige_fixed(cov_pars = c((s2 + tau2) / 2, phi), nugget_val = 0)

# (c) Spherical, no nugget, half the range
kc_c <- krige_fixed(cov_pars = c(s2 + tau2, phi / 2),   nugget_val = 0)

# (d) Pure nugget effect: sigma^2 -> 0, all variance is nugget
kc_d <- krige_fixed(cov_pars = c(1e-10, 1),             nugget_val = s2 + tau2)

cat("Alternative models computed successfully.\n")

# Plot all four alternative maps side by side for comparison
par(mfrow = c(2, 2), mar = c(3, 3, 2, 1), mgp = c(1.8, 0.6, 0))

image(kc_a, loc = pred.grid, xlab = "Easting", ylab = "Northing",
      main = "(a) Spherical, no nugget")
points(s100$coords, pch = ".", cex = 0.5)

image(kc_b, loc = pred.grid, xlab = "Easting", ylab = "Northing",
      main = "(b) No nugget, half sill")
points(s100$coords, pch = ".", cex = 0.5)

image(kc_c, loc = pred.grid, xlab = "Easting", ylab = "Northing",
      main = "(c) No nugget, half range")
points(s100$coords, pch = ".", cex = 0.5)

image(kc_d, loc = pred.grid, xlab = "Easting", ylab = "Northing",
      main = "(d) Pure nugget effect")
points(s100$coords, pch = ".", cex = 0.5)

par(mfrow = c(1, 1))  # reset layout
