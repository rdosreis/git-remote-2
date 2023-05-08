install.packages("usethis")

library(usethis)
use_git_config(user.name = "Lucas Helal", user.email = "lucas@lucashelal.me")
install.packages("pkgdown")
library(pkgdown)
usethis::create_github_token()

gitcreds::gitcreds_set()

usethis::create_github_token()

gitcreds::gitcreds_set()
credentials::set_github_pat()

gitcreds::gitcreds_set()


D <- DAG.empty()
D <- D + 
  node("A", distr = "rnorm", mean = 50, sd = 5) +
  node("X", distr = "rnorm", mean = 33 + A*0.1, sd = 5 ) +
  node("Y", distr = "rnorm", mean = 14+ A*0.9*0.4 + X*0.9*0.2, sd = 6 )

# Generate data from the DAG
Dset1 <- set.DAG(D)

plotDAG(Dset1, xjitter = 0.2, yjitter = 0.85, 
        edge_attrs = list(width = 0.5, arrow.width = 0.4, arrow.size = 0.8), 
        vertex_attrs = list(size = 15, label.cex = 1.4))




dat_test <- sim(DAG = Dset1, n = 100, rndseed = 123)

summary(dat_test)




