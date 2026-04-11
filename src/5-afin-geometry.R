library(plot3D)

drawRegion <- function(points, col) {
  ch <- chull(points)
  polygon(points[ch, ], col=col, border="black")
}

translatePoints2D <- function(points, vector) {
  result <- sweep(points, 2, vector, "+")
  
  plot(0,0, type="n", asp=1,
       xlim=range(c(points[,1], result[,1])) + c(-1,1),
       ylim=range(c(points[,2], result[,2])) + c(-1,1),
       main="2D Translation")
  
  grid(); abline(h=0, v=0, col="gray")
  
  drawRegion(points, rgb(0,0,1,0.4))
  drawRegion(result, rgb(1,0,0,0.4))
  
  return(result)
}

rotatePointsIn2d <- function(points, center, angle) {
  theta <- angle * pi / 180
  
  R <- matrix(c(cos(theta), -sin(theta),
                sin(theta),  cos(theta)), 2, 2, byrow=TRUE)
  
  shifted <- sweep(points, 2, center, "-")
  result <- sweep(shifted %*% t(R), 2, center, "+")
  
  plot(0,0, type="n", asp=1,
       xlim=range(c(points[,1], result[,1])) + c(-1,1),
       ylim=range(c(points[,2], result[,2])) + c(-1,1),
       main=paste("2D Rotation", angle))
  
  grid(); abline(h=0, v=0, col="gray")
  
  drawRegion(points, rgb(0,0,1,0.4))
  drawRegion(result, rgb(0,1,0,0.4))
  
  points(center[1], center[2], col="black", pch=4, lwd=2)
  
  return(result)
}

simetricPoints2D <- function(points, coefs) {
  normal <- coefs[1:2]
  d <- coefs[3]
  
  factor <- (points %*% normal + d) / sum(normal^2)
  result <- points - 2 * (factor %*% t(normal))
  
  plot(0,0, type="n", asp=1,
       xlim=range(c(points[,1], result[,1])) + c(-1,1),
       ylim=range(c(points[,2], result[,2])) + c(-1,1),
       main="2D Symmetry")
  
  grid(); abline(h=0, v=0, col="gray")
  
  drawRegion(points, rgb(0,0,1,0.4))
  drawRegion(result, rgb(1,0.5,0,0.4))
  
  if(coefs[2] != 0){
    abline(a=-d/coefs[2], b=-coefs[1]/coefs[2], lwd=2)
  } else {
    abline(v=-d/coefs[1], lwd=2)
  }
  
  return(result)
}

homothetyPoints2D <- function(points, center, k) {
  shifted <- sweep(points, 2, center, "-")
  result <- sweep(shifted * k, 2, center, "+")
  
  plot(0,0, type="n", asp=1,
       xlim=range(c(points[,1], result[,1])) + c(-1,1),
       ylim=range(c(points[,2], result[,2])) + c(-1,1),
       main=paste("2D Homothety k=", k))
  
  grid(); abline(h=0, v=0, col="gray")
  
  drawRegion(points, rgb(0,0,1,0.4))
  drawRegion(result, rgb(0.6,0,0.8,0.4))
  
  points(center[1], center[2], col="black", pch=4, lwd=2)
  
  return(result)
}

translateSurface3D <- function(x, y, z, vector) {
  xr <- range(c(x, x + vector[1]))
  yr <- range(c(y, y + vector[2]))
  zr <- range(c(z, z + vector[3]))
  
  surf3D(x, y, z,
         col="lightblue", border="black", colkey=FALSE,
         xlim=xr, ylim=yr, zlim=zr,
         main="3D Translation")
  
  surf3D(x+vector[1], y+vector[2], z+vector[3],
         add=TRUE, col="red", border="black")
}

rotateSurface3D <- function(x, y, z, angle, axis="z") {
  theta <- angle*pi/180
  c <- cos(theta); s <- sin(theta)
  
  pts <- cbind(as.vector(x), as.vector(y), as.vector(z))
  
  R <- switch(axis,
              "x" = matrix(c(1,0,0,0,c,s,0,-s,c),3,3,byrow=TRUE),
              "y" = matrix(c(c,0,-s,0,1,0,s,0,c),3,3,byrow=TRUE),
              "z" = matrix(c(c,s,0,-s,c,0,0,0,1),3,3,byrow=TRUE))
  
  rot <- pts %*% t(R)
  
  x2 <- matrix(rot[,1], nrow=nrow(x))
  y2 <- matrix(rot[,2], nrow=nrow(y))
  z2 <- matrix(rot[,3], nrow=nrow(z))
  
  xr <- range(c(x, x2))
  yr <- range(c(y, y2))
  zr <- range(c(z, z2))
  
  surf3D(x, y, z,
         col="lightblue", border="black", colkey=FALSE,
         xlim=xr, ylim=yr, zlim=zr,
         main=paste("3D Rotation", angle, axis))
  
  surf3D(x2, y2, z2, add=TRUE, col="green", border="black")
}

simetricSurface3D <- function(x, y, z, coefs) {
  pts <- cbind(as.vector(x), as.vector(y), as.vector(z))
  
  normal <- coefs[1:3]
  d <- coefs[4]
  
  factor <- (pts %*% normal + d)/sum(normal^2)
  ref <- pts - 2*(factor %*% t(normal))
  
  x2 <- matrix(ref[,1], nrow=nrow(x))
  y2 <- matrix(ref[,2], nrow=nrow(y))
  z2 <- matrix(ref[,3], nrow=nrow(z))
  
  xr <- range(c(x, x2))
  yr <- range(c(y, y2))
  zr <- range(c(z, z2))
  
  surf3D(x, y, z,
         col="lightblue", border="black", colkey=FALSE,
         xlim=xr, ylim=yr, zlim=zr,
         main="3D Symmetry")
  
  surf3D(x2, y2, z2, add=TRUE, col="orange", border="black")
}

homothetySurface3D <- function(x, y, z, center, k) {
  pts <- cbind(as.vector(x), as.vector(y), as.vector(z))
  
  shifted <- sweep(pts,2,center,"-")
  res <- sweep(shifted*k,2,center,"+")
  
  x2 <- matrix(res[,1], nrow=nrow(x))
  y2 <- matrix(res[,2], nrow=nrow(y))
  z2 <- matrix(res[,3], nrow=nrow(z))
  
  xr <- range(c(x, x2))
  yr <- range(c(y, y2))
  zr <- range(c(z, z2))
  
  surf3D(x, y, z,
         col="lightblue", border="black", colkey=FALSE,
         xlim=xr, ylim=yr, zlim=zr,
         main=paste("3D Homothety k=", k))
  
  surf3D(x2, y2, z2, add=TRUE, col="purple", border="black")
}

grid2d <- expand.grid(x=seq(-2,2,length=100),
                      y=seq(-2,2,length=100))
points2d <- as.matrix(grid2d[grid2d$x + grid2d$y <= 4, ])

M <- mesh(seq(-2,2,length=50),
          seq(-2,2,length=50))

x <- M$x
y <- M$y
z <- x^2 + y^2

par(mfrow=c(2,2), mar=c(2,2,2,2))

translatePoints2D(points2d, c(3,4))
rotatePointsIn2d(points2d, c(0,0), 45)
simetricPoints2D(points2d, c(1,-3,0))
homothetyPoints2D(points2d, c(0,0), 1.5)

par(mfrow=c(2,2), mar=c(2,2,2,2))

translateSurface3D(x,y,z,c(0,0,5))
rotateSurface3D(x,y,z,45,"y")
simetricSurface3D(x,y,z,c(0,0,1,-10))
homothetySurface3D(x,y,z,c(0,0,0),0.5)

par(mfrow=c(1,1))