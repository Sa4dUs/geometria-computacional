# ============================================================
# File: 7-determinant-method.R
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-04-26
# Last modified: 2026-04-26
# License: MIT
# ============================================================

orientation <- function(A, B, C) {
  
  #The vector we are using to compare the orientation
  dx1 <- B[1] - A[1]
  dy1 <- B[2] - A[2]
  
  dx2 <- C[1] - A[1]
  dy2 <- C[2] - A[2]
  
  #This is the dot podruct
  D <- dx1 * dy2 - dy1 * dx2
  
  if (D > 0) {
    position <- "C está a la izquierda de AB"
  } else if (D < 0) {
    position <- "C está a la derecha de AB"
  } else {
    position <- "C está alineado con A y B"
  }
  
  if (D < 0) {
    rotation <- "Giro a la derecha"
  } else if (D > 0) {
    rotation <- "Giro a la izquierda"
  } else {
    rotation <- "Sin giro (colineales)"
  }
  
  return(list(
    det = D,
    position = position,
    rotation = rotation
  ))
}


plot_orientation <- function(A, B, C, titulo="") {
  
  res <- orientation(A, B, C)
  
  plot(0, 0, type="n",
       xlim=range(c(A[1], B[1], C[1])) + c(-1,1),
       ylim=range(c(A[2], B[2], C[2])) + c(-1,1),
       xlab="X", ylab="Y",
       main=titulo)
  
  points(A[1], A[2], pch=19)
  points(B[1], B[2], pch=19)
  points(C[1], C[2], pch=19, col="red")
  
  text(A[1], A[2], "A", pos=3)
  text(B[1], B[2], "B", pos=3)
  text(C[1], C[2], "C", pos=3, col="red")
  
  segments(A[1], A[2], B[1], B[2], lwd=2)
  
  segments(A[1], A[2], C[1], C[2], lty=2)
  segments(B[1], B[2], C[1], C[2], lty=2)
  
  legend("topright",
         legend=c(
           paste("Det =", res$det),
           res$position,
           res$rotation
         ),
         bty="n")
}

A1 <- c(0, 0)
B1 <- c(4, 0)
C1 <- c(2, 3)

print(orientation(A1, B1, C1))
plot_orientation(A1, B1, C1)


A2 <- c(0, 0)
B2 <- c(4, 0)
C2 <- c(2, -2)

print(orientation(A2, B2, C2))
plot_orientation(A2, B2, C2)

A3 <- c(0, 0)
B3 <- c(4, 4)
C3 <- c(2, 2)

print(orientation(A3, B3, C3))
plot_orientation(A3, B3, C3)

