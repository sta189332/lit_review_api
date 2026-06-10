library(plumber)
pr <- plumber::pl_create("plumber.R")
pr$pr_run(host = "0.0.0.0", port = 8000)
