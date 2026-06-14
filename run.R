library(plumber)

# Use the classic 'plumb' function instead of 'pl_create'
pr <- plumber::plumb("plumber.R")

# Launch the server cleanly on port 8000
pr$run(host = "127.0.0.1", port = 8000, swagger = TRUE)
