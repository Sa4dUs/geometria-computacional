# ============================================================
# File: 7-gift-wrapping.R
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-04-26
# Last modified: 2026-04-26
# License: MIT
# ============================================================


orientation <- function(a, b, c) {
  (b[1] - a[1]) * (c[2] - a[2]) -
    (b[2] - a[2]) * (c[1] - a[1])
}

convex_hull_gift_wrapping <- function(points) {
  
  points <- as.matrix(points)
  storage.mode(points) <- "double"
  
  n <- nrow(points)
  
  if (n < 3) {
    stop("Se necesitan al menos 3 puntos")
  }
  
  leftmost <- which.min(points[,1])
  
  hull <- c()
  p <- leftmost
  
  repeat {
    hull <- c(hull, p)
    
    q <- ifelse(p == 1, 2, 1)
    
    for (i in 1:n) {
      
      if (i == p) next
      
      if (orientation(points[p,], points[i,], points[q,]) > 0) {
        q <- i
      }
    }
    
    p <- q
    
    if (p == leftmost) break
  }
  
  return(hull)
}

points <- matrix(c(
  1, 2,
  1.5, 2.5,
  2, 2,
  2.5, 2.5,
  3, 1.5,
  2, 1,
  1.53, 1.66,
  1.2, 1
), ncol = 2, byrow = TRUE)


hull_idx <- convex_hull_gift_wrapping(points)

hull_points_list <- lapply(seq_along(hull_idx), function(i) {
  list(
    name = paste0("P", i),
    x = points[hull_idx[i], 1],
    y = points[hull_idx[i], 2]
  )
})

print(hull_points_list)


plot(points, col = "blue", pch = 19, main = "Convex Hull - Gift Wrapping")

polygon(rbind(points[hull_idx, ], points[hull_idx[1], ]),
        col = NA, border = "red", lwd = 2)

points(points[hull_idx, ], col = "red", pch = 19)

for (i in seq_along(hull_idx)) {
  text(points[hull_idx[i], 1],
       points[hull_idx[i], 2],
       labels = paste0("P", i),
       pos = 3,
       col = "black")
}