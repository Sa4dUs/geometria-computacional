
library(scatterplot3d)

translatePoint2D <-function(point,vector ){
    #if translation vector has more dimension we could translate also
    if (length(point) != length(vector)){
        stop("The vector length must be equal to the point length")
    }
    # plot only works in 2D i think
    result = point+vector
    plot(point, result, main="Traslation of a 2D point")
    points(point[1],point[2],col="blue",pch = 16)
    points(result[1],result[2],col="red",pch = 16)
    abline(h = 0, v = 0, col = "gray")
    arrows(point[1],point[2],result[1], result[2])
    return (result)
}


rotatePointIn2d <- function(point, center, angle) {
  #Angle enters in degrees
  theta <- angle * (pi / 180)
  
  #Rotation matrix 2x2
  matrix <- matrix(c(cos(theta),  sin(theta), 
                    -sin(theta), cos(theta)), nrow = 2)
  res <- (point - center) %*% matrix + center
  result <- as.vector(res)
  x_vals <- c(point[1], result[1], center[1])
  y_vals <- c(point[2], result[2], center[2])
  
  plot(x_vals, y_vals,
       main = "Rotation of a 2D point",
       type = "n",
       xlab = "X", ylab = "Y",
       asp = 1)
  
  abline(h = 0, v = 0, col = "gray")

  points(point[1], point[2], col = "blue", pch = 16)
  points(result[1], result[2], col = "green", pch = 16)
  points(center[1], center[2], col = "red", pch = 16)
  
  segments(center[1], center[2], point[1], point[2], lty = 2)
  segments(center[1], center[2], result[1], result[2], lty = 2)
  
  arrows(point[1], point[2], result[1], result[2], lwd = 2)
  
  r <- sqrt(sum((point - center)^2)) * 0.5
  
  t_seq <- seq(0, theta, length.out = 100)
  
  arc_x <- center[1] + r * cos(t_seq + atan2(point[2]-center[2], point[1]-center[1]))
  arc_y <- center[2] + r * sin(t_seq + atan2(point[2]-center[2], point[1]-center[1]))
  
  lines(arc_x, arc_y, col = "purple", lwd = 2)
  
  text(arc_x[length(arc_x)], arc_y[length(arc_y)],
       paste0(angle, "°"), col = "purple", pos = 4)
  
  text(point[1], point[2], "P", pos = 3, col = "blue")
  text(result[1], result[2], "P'", pos = 3, col = "green")
  text(center[1], center[2], "C", pos = 3, col = "red")


  
  
  return(result)
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

#For entering the hiperplane the inputs are the coefficients
simetricPointRn <- function(point, coefs) {
  
  n <- length(point)
  
  if (length(coefs) != n + 1) {
    stop("Los coeficientes deben tener longitud n + 1")
  }
  
  normal <- coefs[1:n]
  D <- coefs[n + 1]
  
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

v_trans <- c(-1, 4)
c_rot <- c(0, 0)
ang_rot <- 45
coefs_sim <- c(2, -1, -1)
c_hom <- c(2, -2)
k_hom <- 5

res_trans_2d <- t(apply(P2, 1, translatePoint2D, vector = v_trans))
res_rot_2d   <- t(apply(P2, 1, rotatePointIn2d, center = c_rot, angle = ang_rot))
res_sim_2d   <- t(apply(P2, 1, simetricPoint2d, coefs = coefs_sim))
res_hom_2d   <- t(apply(P2, 1, homothetyPoint, center = c_hom, k = k_hom))



par(mfrow=c(1,1)) 

# --- 2D Translation ---
plot(rbind(P2, res_trans_2d), type="n", main="2D Translation", xlab="X", ylab="Y", asp=1)
grid()
abline(h = 0, v = 0, col = "gray")
points(P2, col="blue", pch=16, cex=1.5)
points(res_trans_2d, col="red", pch=16, cex=1.5)
arrows(P2[,1], P2[,2], res_trans_2d[,1], res_trans_2d[,2], length=0.1, col="black")
text(P2[,1], P2[,2], labels=paste0("P", 1:3), pos=1, col="blue", cex=0.8)
text(res_trans_2d[,1], res_trans_2d[,2], labels=paste0("P'", 1:3), pos=3, col="red", cex=0.8)

# --- 2D Rotation ---
plot(rbind(P2, res_rot_2d, c_rot), type="n", main="2D Rotation", xlab="X", ylab="Y", asp=1)
grid()
abline(h = 0, v = 0, col = "gray")
points(P2, col="blue", pch=16, cex=1.5)
points(res_rot_2d, col="green", pch=16, cex=1.5)
points(c_rot[1], c_rot[2], col="red", pch=4, cex=2, lwd=2)
segments(c_rot[1], c_rot[2], P2[,1], P2[,2], col="blue", lty=2)
segments(c_rot[1], c_rot[2], res_rot_2d[,1], res_rot_2d[,2], col="green", lty=2)
arrows(P2[,1], P2[,2], res_rot_2d[,1], res_rot_2d[,2], length=0.1, col="black")
text(P2[,1], P2[,2], labels=paste0("P", 1:3), pos=1, col="blue", cex=0.8)
text(res_rot_2d[,1], res_rot_2d[,2], labels=paste0("P'", 1:3), pos=3, col="green", cex=0.8)

# --- 2D Simetry---
plot(rbind(P2, res_sim_2d), type="n", main="2D Simetry", xlab="X", ylab="Y", asp=1)
grid()
abline(h = 0, v = 0, col = "gray")

intercept_sim <- -coefs_sim[3]/coefs_sim[2]
slope_sim <- -coefs_sim[1]/coefs_sim[2]
abline(a=intercept_sim, b=slope_sim, col="black", lwd=2) 
points(P2, col="blue", pch=16, cex=1.5)
points(res_sim_2d, col="orange", pch=16, cex=1.5)
segments(P2[,1], P2[,2], res_sim_2d[,1], res_sim_2d[,2], lty=2, col="gray")
text(P2[,1], P2[,2], labels=paste0("P", 1:3), pos=2, col="blue", cex=0.8)
text(res_sim_2d[,1], res_sim_2d[,2], labels=paste0("P'", 1:3), pos=4, col="orange", cex=0.8)

# --- 2D Homotethy ---
plot(rbind(P2, res_hom_2d, c_hom), type="n", main="2D Homotethy", xlab="X", ylab="Y", asp=1)
grid()
abline(h = 0, v = 0, col = "gray")
points(P2, col="blue", pch=16, cex=1.5)
points(res_hom_2d, col="purple", pch=16, cex=1.5)
points(c_hom[1], c_hom[2], col="red", pch=4, cex=2, lwd=2)
segments(c_hom[1], c_hom[2], res_hom_2d[,1], res_hom_2d[,2], lty=2, col="gray")
text(P2[,1], P2[,2], labels=paste0("P", 1:3), pos=4, col="blue", cex=0.8)
text(res_hom_2d[,1], res_hom_2d[,2], labels=paste0("P'", 1:3), pos=2, col="purple", cex=0.8)


P3 <- matrix(c(1, 2, 1, 
               3, 4, 2, 
               0, 0, 5), ncol = 3, byrow = TRUE)

v_trans_3d <- c(2, -1, 3)
c_rot_3d   <- c(0, 0, 0)
ang_rot_3d <- 60
coefs_plano_sim <- c(0, 0, 1, 0)
c_hom_3d   <- c(1, 1, 1)
k_hom_3d   <- 2


res_trans_3d <- t(apply(P3, 1, function(p) p + v_trans_3d))
res_rot_3d <- t(apply(P3, 1, rotatePointIn3d, center = c_rot_3d, angle = ang_rot_3d, axis = "z"))
res_sim_3d <- t(apply(P3, 1, simetricPoint3d, coefs = coefs_plano_sim))
res_hom_3d <- t(apply(P3, 1, homothetyPoint, center = c_hom_3d, k = k_hom_3d))


# --- 3D Translation ---
s3d <- scatterplot3d(rbind(P3, res_trans_3d), pch = 16, color = "lightgray",
                     main = "3D Translation", xlab="X", ylab="Y", zlab="Z")
s3d$points3d(P3, col = "blue", pch = 16, cex = 1.5)
s3d$points3d(res_trans_3d, col = "red", pch = 16, cex = 1.5)
p1 <- s3d$xyz.convert(P3)
p2 <- s3d$xyz.convert(res_trans_3d)
arrows(p1$x, p1$y, p2$x, p2$y, length = 0.1, col = "black")
legend("topleft", legend = c("Original", "Translated"), col = c("blue", "red"), pch = 16)

# --- 3D Rotation ---
s3d <- scatterplot3d(rbind(P3, res_rot_3d), pch = 16, color = "lightgray",
                     main = "3D Rotation", asp=1)
s3d$points3d(P3, col = "blue", pch = 16, cex = 1.5)
s3d$points3d(res_rot_3d, col = "green", pch = 16, cex = 1.5)
p_centro <- s3d$xyz.convert(c_rot_3d[1], c_rot_3d[2], c_rot_3d[3])
points(p_centro$x, p_centro$y, col="red", pch=4, lwd=2)
legend("topleft", legend = c("Original", "Rotated"), col = c("blue", "green"), pch = 16)

# --- 3D Simetry ---
s3d <- scatterplot3d(rbind(P3, res_sim_3d), pch = 16, color = "white",
                     main = "3D Simetry")

s3d$points3d(P3, col = "blue", pch = 16, cex = 1.5)
s3d$points3d(res_sim_3d, col = "orange", pch = 16, cex = 1.5)

for(i in 1:nrow(P3)){
  orig <- s3d$xyz.convert(P3[i,1], P3[i,2], P3[i,3])
  refl <- s3d$xyz.convert(res_sim_3d[i,1], res_sim_3d[i,2], res_sim_3d[i,3])
  segments(orig$x, orig$y, refl$x, refl$y, lty = 2, col = "gray")
}
legend("topleft", legend = c("Original", "Reflected"), col = c("blue", "orange"), pch = 16)

# --- 3D Homotethy ---
s3d <- scatterplot3d(rbind(P3, res_hom_3d), pch = 16, color = "white",
                     main = "3D Homotethy")
s3d$points3d(P3, col = "blue", pch = 16, cex = 1.5)
s3d$points3d(res_hom_3d, col = "purple", pch = 16, cex = 1.5)
p_centro_h <- s3d$xyz.convert(c_hom_3d[1], c_hom_3d[2], c_hom_3d[3])
points(p_centro_h$x, p_centro_h$y, col="red", pch=4, lwd=2)
for(i in 1:nrow(P3)){
  orig <- s3d$xyz.convert(c_hom_3d[1], c_hom_3d[2], c_hom_3d[3])
  dest <- s3d$xyz.convert(res_hom_3d[i,1], res_hom_3d[i,2], res_hom_3d[i,3])
  segments(orig$x, orig$y, dest$x, dest$y, lty = 3, col = "gray")
}
legend("topleft", legend = c("Original", "Scaled"), col = c("blue", "purple"), pch = 16)