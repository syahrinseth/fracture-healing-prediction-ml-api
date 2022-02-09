library(plumber)
r <- plumb(file="/Users/muhammadnorsyahrinseth/Documents/projects/fracture-healing-prediction-ml-api/plumber.R")
r$run(port = 8000)
