## Testing Independence (Slide 5)
set.seed(29)
x <- rnorm(100)
z <- 3*x + rnorm(100)
y <- 2*z + rnorm(100)
ex <- data.frame(x,y,z)
# Chain: x dep z, z dep y, x indep y given z

summary(lm(y ~ x + z, data = ex))
# tests:
# coef of x: y indep x given z -> yes, p-value >= 0.05
# coef of z: y indep z given x -> no, p-value < 0.05

summary(lm(x ~ y + z, data = ex))
# tests:
# coef of y: x indep y given z -> no
# coef of z: x indep z given y -> no

summary(lm(z ~ y + x, data = ex))
# tests:
# coef of y: z indep y given x -> no
# coef of x: z indep x given y -> no



# Example data generation
set.seed(239)
N <- 500000
X <- rnorm(N)
Z <- rnorm(N)
W <- 5 * X + 10 * Z + rnorm(N)
Y <- 12 * W + rnorm(N)
V <- 8 * W + rnorm(N)
dat <- cbind(X, Z, W, Y, V)

library(pcalg)
suffStat <- list(C = cor(dat), n = nrow(dat))
gaussCItest(x = "X", y = "Z", S = NULL, suffStat)
# Test for X and Z independent? Yes, p-value > 0.05



library(pcalg)
suffStat <- list(C = cor(dat), n = nrow(dat))
pc.fit <- pc(suffStat,
             indepTest = gaussCItest,
             alpha = 0.01,
             labels = colnames(dat),
             verbose = TRUE)
