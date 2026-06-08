#############
# Exercise 2
#############

set.seed(253)
library(gRbase)
library(igraph)
grest <- dag(
  c("Study", "Teacher", "Prior", "Attendance"),
  c("Attendance", "Prior", "Teacher"),
  c("Exam", "Study", "Prior")
)
E(grest)$weight <- c(-5, -3, -3, -2, 4, 8, 3)
plot(grest, edge.label = E(grest)$weight)

# c)
set.seed(253)
n <- 1000000  # Sample size

# Simulate data from the SCM (using the DAG's coefficients)
prior      <- rnorm(n)
teacher    <- rnorm(n)
attendance <- -2 * prior + 4 * teacher + rnorm(n)
study      <- -3 * prior - 5 * teacher + -3 * attendance + rnorm(n)
exam       <- 3 * prior + 8 * study + rnorm(n)

# Estimate total causal effect of attendance on exam
fit <- lm(exam ~ attendance + prior + teacher)
coef(fit)["attendance"]  # Returns the estimated effect (≈ 24 for large n)
