# ============================================================
# File: 1-parallelogram-area.R
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-02-05
# Last modified: 2026-02-09
# License: MIT
# ============================================================

calculateAreaFromVec <- function (v1, v2) {
  dot = sum(v1 * v2)
  m1_sq <- sum(v1^2)
  m2_sq <- sum(v2^2)
  
  sqrt(m1_sq * m2_sq - dot^2)
}

plotFromVec <- function (v1, v2) {
  p_x <- c(0, v1[1], v1[1] + v2[1], v2[1])
  p_y <- c(0, v1[2], v1[2] + v2[2], v2[2])
  
  plot(NULL, xlim=c(0, max(p_x) + 1), ylim=c(0, max(p_y) + 1), 
       xlab="X", ylab="Y", main="Parallelogram Visualization", asp=1)
  
  polygon(p_x, p_y, col=rgb(0.2, 0.5, 1, 0.3), border="blue", lwd=2)
  
  arrows(0, 0, v1[1], v1[2], col="red", lwd=3, length=0.1)
  arrows(0, 0, v2[1], v2[2], col="darkgreen", lwd=3, length=0.1)
  
  text(v1[1], v1[2], "v1", pos=4, col="red")
  text(v2[1], v2[2], "v2", pos=2, col="darkgreen")
  grid()
}

main <- function() {
  v1 <- c(1, 2)
  v2 <- c(2, 1)
  
  plotFromVec(v1, v2)
  area <- calculateAreaFromVec(v1, v2)
  print(paste("area = ", area))
}

main()