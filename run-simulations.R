library(here)
library(dplyr)
source(here("functions/simulation-functions.R"))


# set up the number of unique simulations: each simulation corresponds to a unique landscape 
n_simulations <- 2000

# set up the number of chains per simulation
n_chains <- 20

# set up number of iterations for each chain
n_iterations <- 100

# set grid size of simulation landscape
grid_size <- 10

# set up parameters that are varying in each simulation
sim_parameters <- sampleSimParameters(
  cor_landscape = c(0.1,6),
  var_landscape = c(0.1,3),
  max_step_sizes = c(5,5),
  rejection_temp = c(0, 0),
  n_simulations = n_simulations
)

# run simulation 
simulated_data <- runSearchSimulation(
  sim_parameters = sim_parameters,
  grid_size = grid_size,
  n_iterations = n_iterations,
  n_chains = n_chains
)

# add consensus information
simulated_data <- simulated_data %>%
  group_by(sim_number, iteration) %>%
  mutate(
    n_other_agents_per_iter = sapply(
      seq_along(x), 
      function(i) sum(x[i] == x[-i] & y[i] == y[-i] & chain_number[i] != chain_number[-i])
    ))


# save 
save(simulated_data, file = here(
  paste0(
    "output/simdata-norm-",
    n_simulations,
    "-simulations-",
    n_iterations,
    "-iterations-",
    n_chains,
    "-chains-",
    grid_size,
    "-grid-size-temp-0-step-5fixed-rank-highcor.Rdata"
  )
))



