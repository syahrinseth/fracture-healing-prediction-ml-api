library(plumber)
r <- plumb("plumbertest.R")
r$run(port = 8000)
