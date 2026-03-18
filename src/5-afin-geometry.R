


translatePoint <-function(point,vector ){
    #if translation vector has more dimension we could translate also
    if (length(point) != length(vector)){
        stop("The vector length must be equal to the point length")
    }
    return (point +  vector)
}


rotatePointIn2d <- function(point, center, angle) {
  #Angle enters in degrees
  theta <- angle * (pi / 180)
  
  #Rotation matrix 2x2
  matrix <- matrix(c(cos(theta),  sin(theta), 
                    -sin(theta), cos(theta)), nrow = 2)
  
  res <- (point - center) %*% matrix + center
  return(as.vector(res))
}

rotatePointIn3d <- function(point, center, angle, axis = "z") {
  theta <- angle * (pi / 180)
  s <- sin(theta)
  c <- cos(theta)
  
  matrix <- switch(tolower(axis),
    "x" = matrix(c(1, 0, 0,  0, c, s,  0, -s, c), nrow = 3),
    "y" = matrix(c(c, 0, -s, 0, 1, 0,  s, 0, c), nrow = 3),
    "z" = matrix(c(c, s, 0, -s, c, 0,  0, 0, 1), nrow = 3),
    stop("Axis not valid. Use 'x', 'y' o 'z'.")
  )
  
  res <- (point - center) %*% matrix + center
  return(as.vector(res))
}

#For entering the straight line the inputs are the coefficients
simetricPoint2d <- function(point, coefs) {
  A <- coefs[1]; B <- coefs[2]; C <- coefs[3]
  
  n <- c(A, B)
  n_norm_sq <- sum(n^2)
  
  distance <- (sum(n * point) + C) / n_norm_sq
  
  simetric <- point - 2 * distance * n
  
  return(as.vector(simetric))
}

simetricPoint3d <- function(point, coefs) {
  normal <- coefs[1:3]
  D <- coefs[4]
  
  n_norm_sq <- sum(normal^2)
  
  factor <- (sum(normal * point) + D) / n_norm_sq
  simetric <- point - 2 * factor * normal
  
  return(as.vector(simetric))
}

homothetyPoint <- function(point, center, k) {
  n <- length(point)
  
  if (length(center) != n) {
    stop("Point and center must have the same dimension")
  }

  matrix_h <- diag(k, n, n)
  
  res <- (point - center) %*% matrix_h + center
  
  return(as.vector(res))
}

P2 <- matrix(c(1, 2, 
               3, 4, 
               0, 0), ncol = 2, byrow = TRUE)

res_trans_2d <- t(apply(P2, 1, translatePoint, vector = c(-1, 4)))

res_rot_2d <- t(apply(P2, 1, rotatePointIn2d, center = c(0,0), angle = 45))

res_sim_2d <- t(apply(P2, 1, simetricPoint2d, coefs = c(2, -1, -1)))

res_hom_2d <- t(apply(P2, 1, homothetyPoint, center = c(2, -2), k = 5))