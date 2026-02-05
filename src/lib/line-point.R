# ============================================================
# File: line-point.R
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-02-05
# Last modified: 2026-02-05
# License: MIT
# ============================================================

line <- function(m, n) {
  function(x) {
    m * x + n
  }
}

point <- function(x, y) {
  c(x, y)
}

classify_proj <- function(l, p) {
  p_x <- p[1]
  p_y <- p[2]
  
  y <- l(p_x)
  
  sign(p_y - y)
}

classify_det <- function(l, p) {
  o = c(0, l(0))
  q = c(1, l(1))
  v_l = q - o
  v_p = q - p
  
  mat <- cbind(v_p, v_l)
  
  sign(det(mat))
}

plot_lp <- function(l, p) {
  x_vals <- c(0, 3)
  y_vals <- l(x_vals)
  
  plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2,
       xlim = c(0, 3), ylim = c(0, 3),
       xlab = "x", ylab = "y")
  
  points(p[1], p[2], col = "red", pch = 19, cex = 1.5)
}

classify <- classify_det

# main
main <- function() {
  x <- 1
  y <- .5

  m <- 1
  n <- 0

  p <- point(x, y)
  l <- line(m, n)

  plot_lp(l, p)
  classify(l, p)
}

main()
