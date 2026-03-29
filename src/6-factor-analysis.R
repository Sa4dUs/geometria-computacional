# ============================================================
# File: 6-factor-analysis.R
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-03-05
# Last modified: 2026-03-29
# License: MIT
# ============================================================

library(readxl)
library(psych)
library(EFAtools)
library(GPArotation)

data_excel <- read_excel("assets/Estudiantes.xlsx")
M <- as.data.frame(data_excel[, -1])

cor_matrix <- cor(M)
print(cor_matrix)

BARTLETT(M)
KMO(M)

ev <- eigen(cor_matrix)
plot(ev$values, type="b", pch=19, col="blue", 
     xlab="Number of Factors", ylab="Eigenvalues")
abline(h = 1, col="red", lty=2)

n_factors <- 3
model <- principal(M, nfactors = n_factors, rotate = "varimax", scores = TRUE)

print(model$loadings, cutoff = 0.4, digits = 3)

print(model$Vaccounted)
print(model$communality)

student_scores <- model$scores
head(student_scores)

biplot(model)
