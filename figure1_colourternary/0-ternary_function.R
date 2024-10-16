
#THIS TERNARY PLOT FUNCTION HAS BEEN MODIFIED FROM STATDA:
#https://rdrr.io/cran/StatDA/src/R/ternary.R

# Ternary plot function
ternary<-function (x, nam = NULL, grid = TRUE, ...)
{
  val = 0.6
  if (is.null(nam)) {
    nam <- dimnames(x)[[2]]
  }
  s <- rowSums(x)
  if (any(s <= 0))
    stop("each row of the input `object' must have a positive sum")
  dat <- x/s
  xp <- dat[, 2] + dat[, 3]/2
  yp <- dat[, 3] * sqrt(3)/2
  par(pty = "s")
  plot(xp, yp, xlim = c(0, 1), ylim = c(0, 1), frame.plot = FALSE,
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...)
  segments(0, 0, 1, 0)
  segments(0, 0, 1/2, sqrt(3)/2)
  segments(1/2, sqrt(3)/2, 1, 0)
  mtext(nam[1], side = 1, line = 1, at = -0.05, cex = 3)
  mtext(nam[2], side = 1, line = 1, at = 1.05, cex = 3)
  text(0.5, 0.9, nam[3], cex = 3)
  if (grid == TRUE) {
    segments(0.2, 0, 0.1, sqrt(0.03), col = grey(val), lty = "dashed")
    segments(0.4, 0, 0.2, sqrt(0.12), col = grey(val), lty = "dashed")
    segments(0.6, 0, 0.3, sqrt(0.27), col = grey(val), lty = "dashed")
    segments(0.8, 0, 0.4, sqrt(0.48), col = grey(val), lty = "dashed")
    segments(0.2, 0, 0.6, sqrt(0.48), col = grey(val), lty = "dashed")
    segments(0.4, 0, 0.7, sqrt(0.27), col = grey(val), lty = "dashed")
    segments(0.6, 0, 0.8, sqrt(0.12), col = grey(val), lty = "dashed")
    segments(0.8, 0, 0.9, sqrt(0.03), col = grey(val), lty = "dashed")
    segments(0.1, sqrt(0.03), 0.9, sqrt(0.03), col = grey(val),
             lty = "dashed")
    segments(0.2, sqrt(0.12), 0.8, sqrt(0.12), col = grey(val),
             lty = "dashed")
    segments(0.3, sqrt(0.27), 0.7, sqrt(0.27), col = grey(val),
             lty = "dashed")
    segments(0.4, sqrt(0.48), 0.6, sqrt(0.48), col = grey(val),
             lty = "dashed")
    text(0.95, 0.21, "20", col = grey(val), cex = 1.75, srt = 60)
    text(0.86, 0.35, "40", col = grey(val), cex = 1.75, srt = 60)
    text(0.75, 0.54, "60", col = grey(val), cex = 1.75, srt = 60)
    text(0.64, 0.72, "80", col = grey(val), cex = 1.75, srt = 60)
    text(0.05, 0.21, "80", col = grey(val), cex = 1.75, srt = 300)
    text(0.14, 0.35, "60", col = grey(val), cex = 1.75, srt = 300)
    text(0.25, 0.54, "40", col = grey(val), cex = 1.75, srt = 300)
    text(0.36, 0.72, "20", col = grey(val), cex = 1.75, srt = 300)
    text(0.2, -0.02, "20", col = grey(val), cex = 1.75, srt = 60)
    text(0.4, -0.02, "40", col = grey(val), cex = 1.75, srt = 60)
    text(0.6, -0.02, "60", col = grey(val), cex = 1.75, srt = 60)
    text(0.8, -0.02, "80", col = grey(val), cex = 1.75, srt = 60)
  }
}


save(ternary,file="./figure2_colourternary/ternary_function.RData")
