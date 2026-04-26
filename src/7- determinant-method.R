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