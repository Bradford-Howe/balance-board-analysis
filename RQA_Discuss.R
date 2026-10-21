library(crqa)





# Plot with no recrrence for location but continual patterned recurrence for
# error. Error would be calculated by measuring the perpendicular distance from
# the perfect line to each point.

JustAVector = seq(1,2000,1)

# Manufatured trajectory
plot((sin(JustAVector)*5)+JustAVector[c(1:200)]+10)
# Theoretical pefect line
abline(0,1)

# ---- Build the two series ----
t = 1:1000
location = t + sin(t)                    # progressive, no exact returns
error    = abs(sin(t)) / sqrt(2)         # perpendicular distance to y = t

# Optional quick visual
# plot(t, location, type = "l", main = "Location (progressive)")
# abline(0, 1, col = "red", lty = 2)
# plot(t, error, type = "l", main = "Error (|sin|)")

# ---- Shared RQA parameters ----
# Start simple: no embedding, small radius relative to each series range.
# You can later replace radius with a value from optimizeParam if you prefer.
delay  = 1
embed  = 1
rescale = 0
normalize = 0
mindiagline = 2
minvertline = 2
tw = 5          # Theiler window to exclude trivial temporal neighbors
method = "rqa"
metric = "euclidean"
datatype = "continuous"

# Radius as a fraction of each series' range (tune these)
radius_loc  = 0.01 * diff(range(location))
radius_err  = 0.01 * diff(range(error))

# ---- Run ----
rqa_loc = crqa(location, location,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_loc,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

rqa_err = crqa(error, error,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_err,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

# ---- Compare ----
cat("Location RR:", rqa_loc$RR, "  DET:", rqa_loc$DET, "\n")
cat("Error    RR:", rqa_err$RR, "  DET:", rqa_err$DET, "\n")



#####################

t = 1:1000
location = t + sin(t)
error    = abs(sin(t)) / sqrt(2)

# Shared settings
delay = 1; embed = 1; rescale = 0; normalize = 0
mindiagline = 2; minvertline = 2; tw = 5
method = "rqa"; metric = "euclidean"; datatype = "continuous"

# Tighter radius on the large-range location series
radius_loc = 0.002 * diff(range(location))
radius_err = 0.05  * diff(range(error))   # larger relative radius so the sine is captured

rqa_loc = crqa(location, location, delay, embed, rescale, radius_loc,
                normalize, mindiagline, minvertline, tw,
                method = method, metric = metric, datatype = datatype)

rqa_err = crqa(error, error, delay, embed, rescale, radius_err,
                normalize, mindiagline, minvertline, tw,
                method = method, metric = metric, datatype = datatype)

cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2), "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2), "\n")


#############################################
#############################################
#############################################


library(crqa)

t = 1:1000
location = t + sin(t)
error    = abs(sin(t)) / sqrt(2)

# Shared settings
delay = 1
embed = 1
rescale = 0
normalize = 0
mindiagline = 2
minvertline = 2
tw = 5
method = "rqa"
metric = "euclidean"
datatype = "continuous"

# Fixed percentage of each series' own range
frac = 0.02          # 2 % of range; try 0.01 or 0.05 if you want to see sensitivity
radius_loc = frac * diff(range(location))
radius_err = frac * diff(range(error))

rqa_loc = crqa(location, location,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_loc,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

rqa_err = crqa(error, error,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_err,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

cat("Fraction of range:", frac, "\n")
cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2),
    " maxL:", rqa_loc$maxL, "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2),
    " maxL:", rqa_err$maxL, "\n")


###################################3
###################################
###################################

t = 1:200
location = 5 * sin(t) + t^1.1
error    = abs(location - t) / sqrt(2)

# Shared settings
delay = 1
embed = 1
rescale = 0
normalize = 0
mindiagline = 2
minvertline = 2
tw = 5
method = "rqa"
metric = "euclidean"
datatype = "continuous"

# Fixed percentage of each series' own range
frac = 0.02
radius_loc = frac * diff(range(location))
radius_err = frac * diff(range(error))

rqa_loc = crqa(location, location,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_loc,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

rqa_err = crqa(error, error,
                delay = delay, embed = embed,
                rescale = rescale, radius = radius_err,
                normalize = normalize,
                mindiagline = mindiagline, minvertline = minvertline,
                tw = tw, method = method, metric = metric,
                datatype = datatype)

cat("Fraction of range:", frac, "\n")
cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2),
    " maxL:", rqa_loc$maxL, "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2),
    " maxL:", rqa_err$maxL, "\n")

##################################
##################################
##################################



JustAVector = seq(1, 200, 1)
set.seed(42)                          # reproducible noise
noise = rnorm(length(JustAVector), sd = 1.5)
location = (sin(JustAVector) * 5) + JustAVector^1.1 + noise
error = abs(location - JustAVector) / sqrt(2)

# Plot in the same style
plot(location, type = "l", main = "Noisy progressive trajectory",
     xlab = "t", ylab = "location")
abline(0, 1, col = "red", lty = 2)

# Shared RQA settings
delay = 1
embed = 1
rescale = 0
normalize = 0
mindiagline = 2
minvertline = 2
tw = 5
method = "rqa"
metric = "euclidean"
datatype = "continuous"

frac = 0.02
radius_loc = frac * diff(range(location))
radius_err = frac * diff(range(error))

rqa_loc = crqa(location, location,
               delay = delay, embed = embed,
               rescale = rescale, radius = radius_loc,
               normalize = normalize,
               mindiagline = mindiagline, minvertline = minvertline,
               tw = tw, method = method, metric = metric,
               datatype = datatype)

rqa_err = crqa(error, error,
               delay = delay, embed = embed,
               rescale = rescale, radius = radius_err,
               normalize = normalize,
               mindiagline = mindiagline, minvertline = minvertline,
               tw = tw, method = method, metric = metric,
               datatype = datatype)

cat("Fraction of range:", frac, "\n")
cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2),
    " maxL:", rqa_loc$maxL, "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2),
    " maxL:", rqa_err$maxL, "\n")

##########################################
##########################################
##########################################
##########################################JustAVector = seq(1, 200, 1)

# Parallel trajectory: constant offset + sine, no noise
location = (sin(JustAVector) * 5) + JustAVector + 10
error = abs(location - JustAVector) / sqrt(2)

# Plot in the same style
plot(location, type = "l", main = "Parallel trajectory (offset + sine)",
     xlab = "t", ylab = "location")
abline(0, 1, col = "red", lty = 2)

# Shared RQA settings
delay = 1
embed = 1
rescale = 0
normalize = 0
mindiagline = 2
minvertline = 2
tw = 5
method = "rqa"
metric = "euclidean"
datatype = "continuous"
frac = 0.02

radius_loc = frac * diff(range(location))
radius_err = frac * diff(range(error))

rqa_loc = crqa(location, location,
               delay = delay, embed = embed,
               rescale = rescale, radius = radius_loc,
               normalize = normalize,
               mindiagline = mindiagline, minvertline = minvertline,
               tw = tw, method = method, metric = metric,
               datatype = datatype)

rqa_err = crqa(error, error,
               delay = delay, embed = embed,
               rescale = rescale, radius = radius_err,
               normalize = normalize,
               mindiagline = mindiagline, minvertline = minvertline,
               tw = tw, method = method, metric = metric,
               datatype = datatype)

cat("Fraction of range:", frac, "\n")
cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2),
    " maxL:", rqa_loc$maxL, "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2),
    " maxL:", rqa_err$maxL, "\n")


########################################
########################################
########################################


JustAVector = seq(1, 2000, 1)

# Parallel trajectory: constant offset + sine, no noise
location = (sin(JustAVector) * 5) + JustAVector + 10
error = abs(location - JustAVector) / sqrt(2)

# Plot
plot(location[1:100], type = "l", main = "Parallel trajectory (offset + sine)",
     xlab = "t", ylab = "location")
abline(0, 1, col = "red", lty = 2)

# Shared RQA settings
delay = 1
embed = 1
rescale = 0
normalize = 0
mindiagline = 2
minvertline = 2
tw = 5
method = "rqa"
metric = "euclidean"
datatype = "continuous"

# Same absolute tolerance for both series
# (using the radius that 0.02 * range would have given the error series)
frac = 0.02
radius_common = frac * diff(range(error))
# Or pick a fixed absolute value, e.g. radius_common = 0.5

rqa_loc = crqa(location, location,
               delay = delay, embed = embed,
               rescale = rescale, radius = radius_common,
               normalize = normalize,
               mindiagline = mindiagline, minvertline = minvertline,
               tw = tw, method = method, metric = metric,
               datatype = datatype)

rqa_err = crqa(error, error,
               delay = delay, embed = embed,
               rescale = rescale, radius = radius_common,
               normalize = normalize,
               mindiagline = mindiagline, minvertline = minvertline,
               tw = tw, method = method, metric = metric,
               datatype = datatype)

cat("Common absolute radius:", round(radius_common, 4), "\n")
cat("Location  RR:", round(rqa_loc$RR, 2),
    " DET:", round(rqa_loc$DET, 1),
    " meanL:", round(rqa_loc$L, 2),
    " maxL:", rqa_loc$maxL, "\n")
cat("Error     RR:", round(rqa_err$RR, 2),
    " DET:", round(rqa_err$DET, 1),
    " meanL:", round(rqa_err$L, 2),
    " maxL:", rqa_err$maxL, "\n")