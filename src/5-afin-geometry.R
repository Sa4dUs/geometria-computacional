library(scatterplot3d)

translatePoint2D <- function(point, vector) {
  if (length(point) != length(vector)) stop("El vector y el punto deben tener la misma longitud")
  
  result <- point + vector
  
  xlim <- range(c(point[1], result[1], 0)) + c(-1, 1)
  ylim <- range(c(point[2], result[2], 0)) + c(-1, 1)
  
  plot(rbind(point, result), type="n", asp=1, xlim=xlim, ylim=ylim,
       main="2D Translation", xlab="X", ylab="Y")
  grid()
  abline(h = 0, v = 0, col = "gray")
  
  points(point[1], point[2], col="blue", pch = 16)
  points(result[1], result[2], col="red", pch = 16)
  arrows(point[1], point[2], result[1], result[2], lwd=2)
  
  text(point[1], point[2], "P", pos=3, col="blue")
  text(result[1], result[2], "P'", pos=3, col="red")
  
  return(result)
}

translatePoint3D <- function(point, vector) {
  result <- point + vector
  datos <- rbind(matrix(point, nrow=1), matrix(result, nrow=1))
  
  s3d <- scatterplot3d(datos, pch=16, main="3D Translation", angle=55)
  s3d$points3d(point[1], point[2], point[3], col="blue", pch=16)
  s3d$points3d(result[1], result[2], result[3], col="red", pch=16)
  
  p1 <- s3d$xyz.convert(point[1], point[2], point[3])
  p2 <- s3d$xyz.convert(result[1], result[2], result[3])
  arrows(p1$x, p1$y, p2$x, p2$y, col="black", lwd=2, length=0.1)
  
  return(result)
}

rotatePointIn2d <- function(point, center, angle) {
  theta <- angle * (pi / 180)
  
  matrix_rot <- matrix(c(cos(theta), -sin(theta), 
                         sin(theta),  cos(theta)), nrow = 2, byrow = TRUE)
  
  res <- matrix_rot %*% (point - center) + center
  result <- as.vector(res)

  r <- sqrt(sum((point - center)^2)) * 0.5
  start_angle <- atan2(point[2]-center[2], point[1]-center[1])
  t_seq <- seq(0, theta, length.out = 100)
  arc_x <- center[1] + r * cos(start_angle + t_seq)
  arc_y <- center[2] + r * sin(start_angle + t_seq)
  
  all_x <- c(point[1], result[1], center[1], arc_x)
  all_y <- c(point[2], result[2], center[2], arc_y)
  
  plot(all_x, all_y, type="n", asp=1, xlab="X", ylab="Y", 
       xlim=range(all_x) + c(-0.5, 0.5), ylim=range(all_y) + c(-0.5, 0.5),
       main=paste("2D Rotation (", angle, "°)", sep=""))
  grid(); abline(h = 0, v = 0, col = "gray")
  
  points(point[1], point[2], col = "blue", pch = 16)
  points(result[1], result[2], col = "green", pch = 16)
  points(center[1], center[2], col = "red", pch = 16)
  
  segments(center[1], center[2], point[1], point[2], lty = 2)
  segments(center[1], center[2], result[1], result[2], lty = 2)
  
  lines(arc_x, arc_y, col = "purple", lwd = 2)
  
  mid_index <- floor(length(t_seq)/2)
  r_offset <- 0.25 * r
  text(center[1] + (r + r_offset) * cos(start_angle + t_seq[mid_index]),
       center[2] + (r + r_offset) * sin(start_angle + t_seq[mid_index]),
       paste0(angle, "°"), col = "purple", cex = 0.9)
  
  text(point[1], point[2], "P", pos = 3, col = "blue")
  text(result[1], result[2], "P'", pos = 3, col = "green")
  text(center[1], center[2], "C", pos = 3, col = "red")
  
  return(result)
}
rotatePointIn3d <- function(point, center, angle, axis = "z") {
  theta <- angle * (pi / 180)
  s <- sin(theta)
  c <- cos(theta)
  
  matrix_rot <- switch(tolower(axis),
                       "x" = matrix(c(1, 0, 0,  0, c, s,  0, -s, c), nrow = 3),
                       "y" = matrix(c(c, 0, -s, 0, 1, 0,  s, 0, c), nrow = 3),
                       "z" = matrix(c(c, s, 0, -s, c, 0,  0, 0, 1), nrow = 3),
                       stop("Eje no válido. Usa 'x', 'y' o 'z'.")
  )
  
  res <- (point - center) %*% matrix_rot + center
  result <- as.vector(res)

  datos <- rbind(matrix(point, nrow=1), matrix(result, nrow=1), matrix(center, nrow=1))
  s3d <- scatterplot3d(datos, type="n", main=paste("3D Rotation (Axis", toupper(axis), ")"), angle=55)
  
  s3d$points3d(point[1], point[2], point[3], col="blue", pch=16)
  s3d$points3d(result[1], result[2], result[3], col="green", pch=16)
  s3d$points3d(center[1], center[2], center[3], col="red", pch=16)
  
  p_start <- s3d$xyz.convert(point[1], point[2], point[3])
  p_end   <- s3d$xyz.convert(result[1], result[2], result[3])
  p_cent  <- s3d$xyz.convert(center[1], center[2], center[3])
  
  segments(p_cent$x, p_cent$y, p_start$x, p_start$y, lty=2)
  segments(p_cent$x, p_cent$y, p_end$x, p_end$y, lty=2)
  
  return(result)
}


simetricPoint2D <- function(point, coefs) {
  normal <- coefs[1:2]
  D <- coefs[3]
  factor <- (sum(normal * point) + D) / sum(normal^2)
  result <- as.vector(point - 2 * factor * normal)
  
  xlim <- range(c(point[1], result[1], 0)) + c(-2, 2)
  ylim <- range(c(point[2], result[2], 0)) + c(-2, 2)
  
  plot(rbind(point, result), type="n", asp=1, xlim=xlim, ylim=ylim,
       main="2D Symmetry", xlab="X", ylab="Y")
  grid(); abline(h=0, v=0, col="gray")
  
  if(coefs[2] != 0){
    abline(a = -coefs[3]/coefs[2], b = -coefs[1]/coefs[2], lwd=2, col="darkgray")
  } else {
    abline(v = -coefs[3]/coefs[1], lwd=2, col="darkgray")
  }
  
  points(point[1], point[2], col="blue", pch=16)
  points(result[1], result[2], col="orange", pch=16)
  segments(point[1], point[2], result[1], result[2], lty=2)
  
  text(point[1], point[2], "P", pos=3, col="blue")
  text(result[1], result[2], "P'", pos=3, col="orange")
  
  return(result)
}

simetricPoint3D <- function(point, coefs) {
  normal <- coefs[1:3]
  D <- coefs[4]
  factor <- (sum(normal * point) + D) / sum(normal^2)
  result <- as.vector(point - 2 * factor * normal)
  
  datos <- rbind(matrix(point, nrow = 1), matrix(result, nrow = 1))
  s3d <- scatterplot3d(datos, pch=16, main="3D Symmetry", angle=55)
  
  s3d$points3d(point[1], point[2], point[3], col="blue")
  s3d$points3d(result[1], result[2], result[3], col="orange")
  
  p1 <- s3d$xyz.convert(point[1], point[2], point[3])
  p2 <- s3d$xyz.convert(result[1], result[2], result[3])
  segments(p1$x, p1$y, p2$x, p2$y, lty=2)
  
  return(result)
}


homothetyPoint2D <- function(point, center, k) {
  matrix_h <- diag(k, 2, 2)
  res <- (point - center) %*% matrix_h + center
  result <- as.vector(res)
  
  xlim <- range(c(point[1], result[1], center[1])) + c(-1, 1)
  ylim <- range(c(point[2], result[2], center[2])) + c(-1, 1)
  
  plot(rbind(point, result, center), type="n", asp=1, xlim=xlim, ylim=ylim,
       main=paste("2D Homothety (k =", k, ")"), xlab="X", ylab="Y")
  grid(); abline(h=0, v=0, col="gray")
  
  points(point[1], point[2], col="blue", pch=16)
  points(result[1], result[2], col="purple", pch=16)
  points(center[1], center[2], col="red", pch=4)
  
  segments(center[1], center[2], result[1], result[2], lty=2)
  
  text(point[1], point[2], "P", pos=3, col="blue")
  text(result[1], result[2], "P'", pos=3, col="purple")
  text(center[1], center[2], "C", pos=3, col="red")
  
  return(result)
}

homothetyPoint3D <- function(point, center, k) {
  matrix_h <- diag(k, 3, 3)
  res <- (point - center) %*% matrix_h + center
  result <- as.vector(res)
  
  datos <- rbind(matrix(point, nrow=1), matrix(result, nrow=1), matrix(center, nrow=1))
  s3d <- scatterplot3d(datos, type="n", main=paste("3D Homothety (k =", k, ")"), angle=55)
  
  s3d$points3d(point[1], point[2], point[3], col="blue", pch=16)
  s3d$points3d(result[1], result[2], result[3], col="purple", pch=16)
  
  cent <- s3d$xyz.convert(center[1], center[2], center[3])
  points(cent$x, cent$y, col="red", pch=4, cex=1.5)
  
  p1 <- s3d$xyz.convert(point[1], point[2], point[3])
  p2 <- s3d$xyz.convert(result[1], result[2], result[3])
  
  segments(cent$x, cent$y, p2$x, p2$y, lty=2)
  
  return(result)
}



par(mfrow=c(2, 2))

P2 <- c(2, -1)
res_trans_2d <- translatePoint2D(P2, c(-1, 4))
res_rot_2d   <- rotatePointIn2d(P2, center = c(0, 0), angle = 45)
res_sim_2d   <- simetricPoint2D(P2, coefs = c(2, -1, -1))
res_hom_2d   <- homothetyPoint2D(P2, center = c(2, -2), k = 5)

par(mfrow=c(2, 2))

P3 <- c(2, -1, 3)
res_trans_3d <- translatePoint3D(P3, vector = c(2, -1, 3))
res_rot_3d   <- rotatePointIn3d(P3, center = c(0, 0, 0), angle = 60, axis = "z")
res_sim_3d   <- simetricPoint3D(P3, coefs = c(0, 0, 1, 0))
res_hom_3d   <- homothetyPoint3D(P3, center = c(1, 1, 1), k = 2)

par(mfrow=c(1,1))