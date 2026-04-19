library(plot3D)

X       <- seq(0, pi, length.out = 50)
Y       <- seq(0, 2*pi, length.out = 50)
M       <- mesh(X, Y)
phi     <- M$x
theta   <- M$y

r <- sin(4*phi)^3 + cos(2*phi)^3 + sin(6*theta)^2 + cos(6*theta)^4
x <- r * sin(phi) * cos(theta)
y <- r * cos(phi)
z <- r * sin(phi) * sin(theta)

surf3D(x, y, z, colvar = y, colkey = FALSE, shade = 0.5,
       box = FALSE, theta = 60)