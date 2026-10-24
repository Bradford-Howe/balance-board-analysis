library(crqa)

d <- read.csv("ball_aligned_all.csv")
trials <- unique(d$trial)

held_out <- "T0001"
others   <- setdiff(trials, held_out)

# Leave-one-out average X and Z
avg <- aggregate(cbind(X, Z) ~ point, 
                 data = d[d$trial %in% others, ], 
                 FUN = mean)
avg <- avg[order(avg$point), ]

held <- d[d$trial == held_out, ]
held <- held[order(held$point), ]

# Deviation series
e_X <- avg$X - held$X
e_Z <- avg$Z - held$Z

# RQA helper
run_rqa <- function(series) {
   rad <- 0.01 * diff(range(series))
   crqa(series, series,
        delay = 1, embed = 1, rescale = 0, radius = rad,
        normalize = 0, mindiagline = 2, minvertline = 2,
        tw = 5, method = "rqa", metric = "euclidean",
        datatype = "continuous")
}

# RQA on raw and residual for both axes
rqa_X  <- run_rqa(held$X)
rqa_eX <- run_rqa(e_X)
rqa_Z  <- run_rqa(held$Z)
rqa_eZ <- run_rqa(e_Z)

cat("=== Held-out", held_out, "===\n")
cat("Raw X   RR:", round(rqa_X$RR, 2),  " DET:", round(rqa_X$DET, 1),  " maxL:", rqa_X$maxL, "\n")
cat("e_X     RR:", round(rqa_eX$RR, 2), " DET:", round(rqa_eX$DET, 1), " maxL:", rqa_eX$maxL, "\n")
cat("Raw Z   RR:", round(rqa_Z$RR, 2),  " DET:", round(rqa_Z$DET, 1),  " maxL:", rqa_Z$maxL, "\n")
cat("e_Z     RR:", round(rqa_eZ$RR, 2), " DET:", round(rqa_eZ$DET, 1), " maxL:", rqa_eZ$maxL, "\n")

# Plots: Raw X / e_X and Raw Z / e_Z
op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(held$X, type = "l", main = "Raw X (progressive)", ylab = "ft", xlab = "point")
lines(avg$X, col = "red", lty = 2)
legend("topright", legend = c("held-out", "LOO average"), col = c("black", "red"), lty = c(1, 2), bty = "n")

plot(e_X, type = "l", main = "e_X = X_avg - X_held", ylab = "ft", xlab = "point")
abline(h = 0, col = "gray", lty = 3)

plot(held$Z, type = "l", main = "Raw Z", ylab = "ft", xlab = "point")
lines(avg$Z, col = "red", lty = 2)
legend("topright", legend = c("held-out", "LOO average"), col = c("black", "red"), lty = c(1, 2), bty = "n")

plot(e_Z, type = "l", main = "e_Z = Z_avg - Z_held", ylab = "ft", xlab = "point")
abline(h = 0, col = "gray", lty = 3)

par(op)