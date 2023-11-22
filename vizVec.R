# Quickly visualise distribution of values in a numeric vector (silent)

vizVec <- function(vector, plot.col = "#0000FF88", label.outliers = T, shapiro = F){
  stopifnot(is.numeric(vector))
  old.mfrow <- par()$mfrow
  old.mar <- par()$mar
  par(mfrow = c(2, 2), mar = c(5.1, 4.1, 3.1, 0.6))
  dv <- density(vector)
  bs <- boxplot.stats(vector)
  x_range <- range(dv$x)
  plot(vector, main = "raw", axes = F, xlab = "index", col = "#00000066")
  if(length(bs$out) > 0 && label.outliers == T){
    points(which(vector %in% boxplot.stats(vector)$out), boxplot.stats(vector)$out, pch = 16, col = plot.col)
    text(which(vector %in% boxplot.stats(vector)$out), boxplot.stats(vector)$out, which(vector %in% boxplot.stats(vector)$out), pos = 2, cex = 0.8)
  }
  internal_axis_ticks()
  y_max <- max(hist(vector, plot = F)$density, density(vector)$y)
  MASS::truehist(vector, main = "MASS::truehist + density", col = plot.col, xlim = x_range, ylim = c(0, y_max), axes = F, xlab = paste("n = ", dv$n, "  density bw = ", round(dv$bw, 1)))
  internal_axis_ticks()
  lines(dv, lwd = 3, col = "#00000099")
  car::qqPlot(vector, main = "car::qqPlot", col.lines = plot.col, las = 1, col = "#00000066", pch = 3)
  norm_info <- qqnorm(vector, plot.it = F)
  shap_res <- shapiro.test(vector)
  if(shapiro == T){
    text(min(norm_info$x), max(norm_info$y), "Shapiro-Wilk", adj = 0)
    text(min(norm_info$x), ((max(norm_info$y) - min(norm_info$y)) * 0.95) + min(norm_info$y), paste0("W = ", round(shap_res$statistic, 3), ", p = ", round(shap_res$p.value, 3)), adj = 0)
  }
  boxplot(vector, horizontal = T, main = "boxplot", col = plot.col, ylim = x_range, outline = F, notch = T, whisklty = 1, staplelty = 0, axes = F, xlab = "vector")
  points(mean(vector), 1, pch = 4, cex = 4, lwd = 2)
  internal_axis_ticks(1:4, 1)
  stripchart(vector, add = T, method = "jitter", col = "#00000066", pch = 1)
  if(length(bs$out > 0) && label.outliers == T){
    arrows(boxplot.stats(vector)$out, 0.70, boxplot.stats(vector)$out, 0, length = 0, angle = 0, col = plot.col)
    text(boxplot.stats(vector)$out, 0.75, which(vector %in% boxplot.stats(vector)$out), cex = 0.8)
  }
  par(mfrow = old.mfrow, mar = old.mar)
}

# vizVec(mtcars$qsec) 
# vizVec(iris$Sepal.Width)
