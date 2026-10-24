# =============================================================================
# RQA examples: absolute tip location vs. error/deviation for progressive reaches
# Consolidated static settings at top. Each block demonstrates a specific point.
# Plots show the first ~100 points of each manufactured trajectory for visual reference.
# =============================================================================

library(crqa)

# ---- Static settings used across all examples (unless overridden) ----
delay       <- 1
embed       <- 1
rescale     <- 0
normalize   <- 0
mindiagline <- 2
minvertline <- 2
tw          <- 5          # Theiler window to exclude trivial temporal neighbors
method      <- "rqa"
metric      <- "euclidean"
datatype    <- "continuous"

# =============================================================================
# Example 1: Basic progressive + small sine
# Demonstrates that a progressive coordinate produces little true recurrence
# while the corresponding error series captures the repeating oscillation.
# =============================================================================
t <- 1:1000
location <- t + sin(t)                    # progressive, no exact returns
error    <- abs(sin(t)) / sqrt(2)         # perpendicular distance to y = t

plot(location[1:100], type = "l", main = "Example 1: Progressive + small sine",
     xlab = "t", ylab = "location")
abline(0, 1, col = "red", lty = 2)

radius_loc  <- 0.01 * diff(range(location))
radius_err  <- 0.01 * diff(range(error))

rqa_loc <- crqa(location, location,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_loc,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

rqa_err <- crqa(error, error,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_err,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

cat("=== Example 1: Basic progressive + sine (0.01 * range) ===\n")
cat("Location RR:", round(rqa_loc$RR, 2), "  DET:", round(rqa_loc$DET, 1), "\n")
cat("Error    RR:", round(rqa_err$RR, 2), "  DET:", round(rqa_err$DET, 1), "\n\n")

plot_rp(rqa_loc$RP, title = "Example 1: Location RP")
plot_rp(rqa_err$RP, title = "Example 1: Error RP")

# =============================================================================
# Example 2: Same series, deliberately mismatched relative radii
# Shows how a tighter absolute radius on the large-range location series
# suppresses adjacency artifacts while a larger relative radius on error
# still captures the oscillation.
# =============================================================================
# (reuses location/error from Example 1)

plot(location[1:100], type = "l", main = "Example 2: Same series (mismatched radii)",
     xlab = "t", ylab = "location")
abline(0, 1, col = "red", lty = 2)

radius_loc <- 0.002 * diff(range(location))
radius_err <- 0.05  * diff(range(error))

rqa_loc <- crqa(location, location,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_loc,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

rqa_err <- crqa(error, error,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_err,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

cat("=== Example 2: Mismatched relative radii ===\n")
cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2), "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2), "\n\n")

plot_rp(rqa_loc$RP, title = "Example 2: Location RP")
plot_rp(rqa_err$RP, title = "Example 2: Error RP")

# =============================================================================
# Example 3: Same series, fixed fraction of each series' own range
# Standard comparison using identical relative radius (frac = 0.02).
# High DET on the location series is the diagnostic of threshold-driven adjacency:
# the radius (recurrence threshold) is 0.02 × the large range of the progressive
# series, so it is large in absolute units. Nearby points along the smooth
# progressive path therefore fall inside the radius even though the values
# never truly return. This manufactures long diagonal lines and high DET that
# do not reflect dynamical recurrence of a performance feature.
# =============================================================================
# (reuses location/error from Example 1)
x
# =============================================================================
# Example 4: Curved progressive + larger sine
# Location = 5*sin(t) + t^1.1. Even with a curved trend the progressive
# coordinate still yields low RR; error retains the advantage.
# =============================================================================
t <- 1:200
location <- 5 * sin(t) + t^1.1
error    <- abs(location - t) / sqrt(2)

plot(location, type = "l", main = "Example 4: Curved progressive + larger sine",
     xlab = "t", ylab = "location")
abline(0, 1, col = "red", lty = 2)

frac <- 0.02
radius_loc <- frac * diff(range(location))
radius_err <- frac * diff(range(error))

rqa_loc <- crqa(location, location,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_loc,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

rqa_err <- crqa(error, error,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_err,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

cat("=== Example 4: Curved progressive + larger sine ===\n")
cat("Fraction of range:", frac, "\n")
cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2),
    " maxL:", rqa_loc$maxL, "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2),
    " maxL:", rqa_err$maxL, "\n\n")

plot_rp(rqa_loc$RP, title = "Example 4: Location RP")
plot_rp(rqa_err$RP, title = "Example 4: Error RP")

# =============================================================================
# Example 5: Same curved series + modest noise
# Adds reproducible noise (sd = 1.5). The quantitative RR advantage of
# error persists under modest noise.
# =============================================================================
JustAVector <- seq(1, 200, 1)
set.seed(42)
noise <- rnorm(length(JustAVector), sd = 1.5)
location <- (sin(JustAVector) * 5) + JustAVector^1.1 + noise
error <- abs(location - JustAVector) / sqrt(2)

plot(location, type = "l", main = "Example 5: Curved progressive + sine + noise",
     xlab = "t", ylab = "location")
abline(0, 1, col = "red", lty = 2)

frac <- 0.02
radius_loc <- frac * diff(range(location))
radius_err <- frac * diff(range(error))

rqa_loc <- crqa(location, location,
               delay = delay, embed = embed,
               rescale = rescale, radius = radius_loc,
               normalize = normalize,
               mindiagline = mindiagline, minvertline = minvertline,
               tw = tw, method = method, metric = metric,
               datatype = datatype)

rqa_err <- crqa(error, error,
               delay = delay, embed = embed,
               rescale = rescale, radius = radius_err,
               normalize = normalize,
               mindiagline = mindiagline, minvertline = minvertline,
               tw = tw, method = method, metric = metric,
               datatype = datatype)

cat("=== Example 5: Curved progressive + sine + noise ===\n")
cat("Fraction of range:", frac, "\n")
cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2),
    " maxL:", rqa_loc$maxL, "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2),
    " maxL:", rqa_err$maxL, "\n\n")

plot_rp(rqa_loc$RP, title = "Example 5: Location RP")
plot_rp(rqa_err$RP, title = "Example 5: Error RP")

# =============================================================================
# Example 6: Strictly parallel trajectory (constant offset + sine)
# Location = t + 10 + 5*sin(t). Pure offset + oscillation. With own-range
# radii the location series still shows adjacency-driven high DET.
# =============================================================================
JustAVector <- seq(1, 200, 1)
location <- (sin(JustAVector) * 5) + JustAVector + 10
error <- abs(location - JustAVector) / sqrt(2)

plot(location, type = "l", main = "Example 6: Parallel trajectory (offset + sine)",
     xlab = "t", ylab = "location")
abline(0, 1, col = "red", lty = 2)

frac <- 0.02
radius_loc <- frac * diff(range(location))
radius_err <- frac * diff(range(error))

rqa_loc <- crqa(location, location,
               delay = delay, embed = embed,
               rescale = rescale, radius = radius_loc,
               normalize = normalize,
               mindiagline = mindiagline, minvertline = minvertline,
               tw = tw, method = method, metric = metric,
               datatype = datatype)

rqa_err <- crqa(error, error,
               delay = delay, embed = embed,
               rescale = rescale, radius = radius_err,
               normalize = normalize,
               mindiagline = mindiagline, minvertline = minvertline,
               tw = tw, method = method, metric = metric,
               datatype = datatype)

cat("=== Example 6: Parallel offset + sine (own-range radii) ===\n")
cat("Fraction of range:", frac, "\n")
cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2),
    " maxL:", rqa_loc$maxL, "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2),
    " maxL:", rqa_err$maxL, "\n\n")

plot_rp(rqa_loc$RP, title = "Example 6: Location RP")
plot_rp(rqa_err$RP, title = "Example 6: Error RP")

# =============================================================================
# Example 7: Same parallel series, common absolute radius
# Removes the range-scaling confound by using one absolute tolerance
# (derived from the error series) for both. Location RR collapses;
# error retains the structure from the repeated peaks.
# =============================================================================
JustAVector <- seq(1, 2000, 1)
location <- (sin(JustAVector) * 5) + JustAVector + 10
error <- abs(location - JustAVector) / sqrt(2)

plot(location[1000:1], type = "l", main = "Example 7: Parallel trajectory (common radius)",
     xlab = "t", ylab = "location")
abline(0, 1, col = "red", lty = 2)

frac <- 0.02
radius_common <- frac * diff(range(error))

rqa_loc <- crqa(location, location,
               delay = delay, embed = embed,
               rescale = rescale, radius = radius_common,
               normalize = normalize,
               mindiagline = mindiagline, minvertline = minvertline,
               tw = tw, method = method, metric = metric,
               datatype = datatype)

rqa_err <- crqa(error, error,
               delay = delay, embed = embed,
               rescale = rescale, radius = radius_common,
               normalize = normalize,
               mindiagline = mindiagline, minvertline = minvertline,
               tw = tw, method = method, metric = metric,
               datatype = datatype)

cat("=== Example 7: Parallel series, common absolute radius ===\n")
cat("Common absolute radius:", round(radius_common, 4), "\n")
cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2),
    " maxL:", rqa_loc$maxL, "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2),
    " maxL:", rqa_err$maxL, "\n")

plot_rp(rqa_loc$RP, title = "Example 7: Location RP")
plot_rp(rqa_err$RP, title = "Example 7: Error RP")


# =============================================================================
# Example 8: Strictly progressive path + perpendicular sine
# Ideal path is pure progress along one axis. The sine is orthogonal to it.
# Location = the progressive coordinate (no oscillation in that series).
# Error    = absolute perpendicular deviation (the sine itself).
# Demonstrates the radius artifact on a clean progressive coordinate while
# the error series retains the repeating structure.
# =============================================================================
t <- 1:1000
location <- t                          # pure progressive coordinate
error    <- abs(sin(t * 2 * pi / 40))  # perpendicular oscillation, ~25 cycles

plot(location[1000:1], type = "l",
     main = "Example 8: Progressive path + perpendicular sine",
     xlab = "t", ylab = "location")
abline(0, 1, col = "red", lty = 2)

frac <- 0.02
radius_loc <- frac * diff(range(location))
radius_err <- frac * diff(range(error))

rqa_loc <- crqa(location, location,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_loc,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

rqa_err <- crqa(error, error,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_err,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

cat("=== Example 8: Progressive path + perpendicular sine ===\n")
cat("Fraction of range:", frac, "\n")
cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2),
    " maxL:", rqa_loc$maxL, "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2),
    " maxL:", rqa_err$maxL, "\n\n")

plot_rp(rqa_loc$RP, title = "Example 8: Location RP")
plot_rp(rqa_err$RP, title = "Example 8: Error RP")

# =============================================================================
# Example 8: Parallel/offset trajectory with perpendicular error
# location = progressive + offset + sine (same construction as your Example 7)
# error    = absolute perpendicular distance to the ideal line y = t
# Uses the reversed plot window you indicated.
# =============================================================================
JustAVector <- seq(1, 2000, 1)
location <- (sin(JustAVector) * 5) + JustAVector + 10
error    <- abs(location - JustAVector) / sqrt(2)

plot(location[2000:1], type = "l",
     main = "Example 8: Parallel trajectory (perpendicular error)",
     xlab = "t (reversed window)", ylab = "location")
abline(0, 1, col = "red", lty = 2)

frac <- 0.02
radius_loc <- frac * diff(range(location))
radius_err <- frac * diff(range(error))

rqa_loc <- crqa(location, location,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_loc,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

rqa_err <- crqa(error, error,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_err,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

cat("=== Example 8: Parallel trajectory, perpendicular error ===\n")
cat("Fraction of range:", frac, "\n")
cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2),
    " maxL:", rqa_loc$maxL, "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2),
    " maxL:", rqa_err$maxL, "\n\n")

plot_rp(rqa_loc$RP, title = "Example 8: Location RP")
plot_rp(rqa_err$RP, title = "Example 8: Error RP")

# =============================================================================
# Example 8: Nearly constant progress + perpendicular movement
# Progress stays roughly flat; the sine lives in the perpendicular direction.
# Location = absolute progressive coordinate (nearly constant).
# Error    = absolute perpendicular deviation.
# =============================================================================
JustAVector <- seq(1, 2000, 1)
location <- (sin(JustAVector) * 5) + JustAVector + 10
error    <- abs(location - JustAVector) / sqrt(2)

location <- 100 + 0.002 * sin(JustAVector)          # nearly constant progress
perp     <- abs(location - JustAVector) / sqrt(2) # movement carried perpendicular to the path

location <- progress
error    <- abs(perp)

plot(JustAVector, type = "l",
     main = "Example 8: Nearly constant progress (perpendicular movement)",
     xlab = "t", ylab = "location (progress)")
line(location)
abline(h = mean(location), col = "red", lty = 2)

frac <- 0.02
radius_loc <- frac * diff(range(location))
radius_err <- frac * diff(range(error))

rqa_loc <- crqa(location, location,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_loc,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

rqa_err <- crqa(error, error,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_err,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

cat("=== Example 8: Nearly constant progress + perpendicular movement ===\n")
cat("Fraction of range:", frac, "\n")
cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2),
    " maxL:", rqa_loc$maxL, "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2),
    " maxL:", rqa_err$maxL, "\n\n")

plot_rp(rqa_loc$RP, title = "Example 8: Location RP")
plot_rp(rqa_err$RP, title = "Example 8: Error RP")