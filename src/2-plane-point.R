# ============================================================
# File: 2-plane-point.R
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-02-09
# Last modified: 2026-02-16
# License: MIT
# ============================================================

# o is a point and v and u are vectors
plane <- function(o, u, v) {
  list(o = o, u = u, v = v)
}

classify_normal <- function(pl, p) {
  u <- pl$u
  v <- pl$v
  # vectorial product u x v
  normal <- c(
    u[2] * v[3] - u[3] * v[2],
    u[3] * v[1] - u[1] * v[3],
    u[1] * v[2] - u[2] * v[1]
  )
  
  v_p <- p - pl$o
  
  dot_product <- sum(v_p * normal)
  
  # if the dot produtct has the same direction the sign is positive, else is negative
  return(sign(dot_product))
}

classify_det_3d <- function(pl, p) {
  mat <- cbind(pl$u, pl$v, p - pl$o)
  sign(det(mat))
}

plotPP <- function(o, u, v, p) {
  n <- c(u[2]*v[3] - u[3]*v[2], 
         u[3]*v[1] - u[1]*v[3], 
         u[1]*v[2] - u[2]*v[1])
  
  x_seq <- seq(-2, 2, length.out = 30)
  y_seq <- seq(-2, 2, length.out = 30)
  
  z_matrix <- outer(x_seq, y_seq, function(x, y) {
    if (abs(n[3]) < 1e-9) return(NA)
    o[3] - (n[1]*(x - o[1]) + n[2]*(y - o[2])) / n[3]
  })
  
  res <- persp(x_seq, y_seq, z_matrix, 
               phi = 20, theta = 45,
               xlim = c(-2, 2), ylim = c(-2, 2), zlim = c(-2, 2),
               col = rgb(0.68, 0.85, 0.9, 0.6), 
               shade = 0.4, border = NA,
               ticktype = "detailed",
               xlab = "X", ylab = "Y", zlab = "Z",
               main = "Plane, Normal and Projection")
  
  draw_element <- function(start, end, col = "black", lwd = 1, lty = 1, is_arrow = FALSE) {
    p1 <- trans3d(start[1], start[2], start[3], pmat = res)
    p2 <- trans3d(end[1], end[2], end[3], pmat = res)
    if(is_arrow) {lassify_det_3d <- function(pl, p) {
  mat <- cbind(pl$u, pl$v, p - pl$o)
  sign(det(mat))
}

plotPP <- function(o, u, v, p) {
  n <- c(u[2]*v[3] - u[3]*v[2], 
         u[3]*v[1] - u[1]*v[3], 
         u[1]*v[2] - u[2]*v[1])
  
  x_seq <- seq(-2, 2, length.out = 30)
  y_seq <- seq(-2, 2, length.out = 30)
  
  z_matrix <- outer(x_seq, y_seq, function(x, y) {
    if (abs(n[3]) < 1e-9) return(NA)
    o[3] - (n[1]*(x - o[1]) + n[2]*(y - o[2])) / n[3]
  })
  
  res <- persp(x_seq, y_seq, z_matrix, 
               phi = 20, theta = 45,
               xlim = c(-2, 2), ylim = c(-2, 2), zlim = c(-2, 2),
               col = rgb(0.68, 0.85, 0.9, 0.6), 
               shade = 0.4, border = NA,
               ticktype = "detailed",
               xlab = "X", ylab = "Y", zlab = "Z",
               main = "Plane, Normal and Projection")
  
  draw_element <- function(start, end, col = "black", lwd = 1, lty = 1, is_arrow = FALSE) {
    p1 <- trans3d(start[1], start[2], start[3], pmat = res)
    p2 <- trans3d(end[1], end[2], end[3], pmat = res)
    if(is_arrow) {
      arrows(p1$x, p1$y, p2$x, p2$y, col = col, lwd = lwd, length = 0.1, lty = lty)
    } else {
      segments(p1$x, p1$y, p2$x, p2$y, col = col, lwd = lwd, lty = lty)
    }
  }
  
  n_unit <- n / sqrt(sum(n^2))
  dist <- sum((p - o) * n_unit)
  p_proj <- p - dist * n_unit
  
  draw_element(o, o + u, col = "darkgreen", lwd = 2, is_arrow = TRUE)
  draw_element(o, o + v, col = "orange", lwd = 2, is_arrow = TRUE)
  draw_element(o, o + n, col = "blue", lwd = 3, is_arrow = TRUE)
  
  draw_element(p, p_proj, col = "red", lty = 3, lwd = 2)
  
  p_coords <- trans3d(p[1], p[2], p[3], pmat = res)
  pproj_coords <- trans3d(p_proj[1], p_proj[2], p_proj[3], pmat = res)
  
  points(p_coords, pch = 16, col = "red", cex = 1.5)
  points(pproj_coords, pch = 1, col = "black", cex = 1.2)
  
  return(res)
}

main <- function() {
  o <- runif(3, -1, 1)
  u <- runif(3, -1, 1)
  v <- runif(3, -1, 1)
  p <- runif(3, -1, 1)
  plotPP(o, u, v, p)
  
  pl <- plane(o, u, v)
  cat("classify_normal:", classify_normal(pl, p), "\n")
  cat("classify_det:   ", classify_det_3d(pl, p), "\n")
}

main()
      arrows(p1$x, p1$y, p2$x, p2$y, col = col, lwd = lwd, length = 0.1, lty = lty)
    } else {
      segments(p1$x, p1$y, p2$x, p2$y, col = col, lwd = lwd, lty = lty)
    }
  }
  
  n_unit <- n / sqrt(sum(n^2))
  dist <- sum((p - o) * n_unit)
  p_proj <- p - dist * n_unit
  
  draw_element(o, o + u, col = "darkgreen", lwd = 2, is_arrow = TRUE)
  draw_element(o, o + v, col = "orange", lwd = 2, is_arrow = TRUE)
  draw_element(o, o + n, col = "blue", lwd = 3, is_arrow = TRUE)
  
  draw_element(p, p_proj, col = "red", lty = 3, lwd = 2)
  
  p_coords <- trans3d(p[1], p[2], p[3], pmat = res)
  pproj_coords <- trans3d(p_proj[1], p_proj[2], p_proj[3], pmat = res)
  
  points(p_coords, pch = 16, col = "red", cex = 1.5)
  points(pproj_coords, pch = 1, col = "black", cex = 1.2)
  
  return(res)
}

main <- function() {
  o <- runif(3, -1, 1)
  u <- runif(3, -1, 1)
  v <- runif(3, -1, 1)
  p <- runif(3, -1, 1)
  plotPP(o, u, v, p)
  
  pl <- plane(o, u, v)
  cat("classify_normal:", classify_normal(pl, p), "\n")
  cat("classify_det:   ", classify_det_3d(pl, p), "\n")
}

main()