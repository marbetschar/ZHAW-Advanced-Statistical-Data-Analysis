############
# Exercise 3
############

library(gRbase)
library(gRain)
load("data/exam.rda")

# a)
g <- dag(
  c("prior"),
  c("study", "prior"),
  c("score", "prior", "study")
)
plot(g)

# b)
gn <- grain(g, data = exam)
querygrain(gn, nodes = "score", type = "marginal")

# c)
# P(score = high | study = much) =
# ∑ P(prior) * P(study = much | prior) * P(score = high | study = much, prior)
querygrain(gn, nodes = c("score", "study"), type = "conditional")
# P(score = high | study = much) = 0.86
# P(score = high | study = little) = 0.31

# d)
# P(score = high | do(study = much)) =
# ∑ P(prior) * P(score = high | prior, study = little) =
querygrain(gn, nodes = c("prior"), type = "marginal")
querygrain(gn, nodes = c("score", "study", "prior"), type = "conditional")
p_score_high_do_study_much = 0.7511046 * 0.8547486 + 0.2488954 * 0.96226415
p_score_high_do_study_much

# e)
p_score_high_do_study_little = 0.7511046 * 0.1315789 + 0.2488954 * 0.5431034
p_score_high_do_study_little

# f)
ace = p_score_high_do_study_much - p_score_high_do_study_little
ace