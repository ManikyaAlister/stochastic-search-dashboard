library(here)
source(here("functions/simulation-functions.R"))


# set up the number of unique simulations: each simulation corresponds to a unique landscape 
n_simulations <- 1000

# set up the number of chains per simulation
n_chains <- 20

# set up number of iterations for each chain
n_iterations <- 100

# set grid size of simulation landscape
grid_size <- 15

# set up parameters that are varying in each simulation
sim_parameters <- sampleSimParameters(
  cor_landscape = c(0.1,10),
  var_landscape = c(0.1, 20),
  max_step_sizes = c(1,7),
  rejection_temp = c(0.1, 10),
  n_simulations = n_simulations
)

# run simulation 
simulated_data <- runSearchSimulation(
  sim_parameters = sim_parameters,
  grid_size = grid_size,
  n_iterations = n_iterations,
  n_chains = n_chains
)

# save 
save(simulated_data, file = here(
  paste0(
    "output/simdata-",
    n_simulations,
    "-simulations-",
    n_iterations,
    "-iterations-",
    n_chains,
    "-chains-",
    grid_size,
    "-grid-size.Rdata"
  )
))



