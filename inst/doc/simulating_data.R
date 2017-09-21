## ----echo=FALSE,results='asis'-------------------------------------------
knitr::opts_chunk$set(collapse = T)

## ---- results = "hide", message = F, warning = F-------------------------
library(rstansim)

small_simulation <- simulate_data(
  file = "basic_sim.stan", 
  param_values = list("scale" = 2),
  vars = c("x", "y"),
  data_name = "small_sim_example",
  nsim = 10,
  path = "sim_data/"
  )

## ------------------------------------------------------------------------
# is the data saved?
dir("sim_data")

# is the data of the correct format?
example_data <- readRDS("sim_data/small_sim_example_1.rds")
str(example_data)

library(ggplot2)
ggplot(as.data.frame(example_data), aes(x = x, y = y)) + 
  geom_point()


## ---- echo=FALSE, results='asis'-----------------------------------------
## clean up all created files from vignetes/
# remove stan model
if(file.exists("basic_sim.stan"))
  unlink("basic_sim.stan")

# if simulated data dir has been created, delete it
if(dir.exists("sim_data"))
  unlink("sim_data", recursive = T)

