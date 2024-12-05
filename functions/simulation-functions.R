source(here("functions/search-functions.R"))
source(here("functions/landscape-functions.R"))

#' Samples simulation parameters for a batch of simulations
#'
#' @param cor_landscape Sample range for the landscape spatial correlation parameter
#' @param var_landscape Sample range for the landscape variance parameter 
#' @param max_step_sizes Sample range for the maximum step size of search agents.
#' @param rejection_temp Sample range for the rejection temperature of search agents. 
#' @param n_simulations The total number of simulations to run in a batch. 
#'
#' @return A data frame with each simulation parameter (columns) for each simulation run (rows)
#' @export
#'
#' @examples
sampleSimParameters = function(cor_landscape = c(.1,7),
                               var_landscape = c(1,20),
                               max_step_sizes = c(1,5),
                               rejection_temp = c(0.1,10),
                               n_simulations = 100
) {
  # define parameter ranges for simulations ([1] = lower, [2] = upper)
  sim_parameter_ranges <- rbind(
      # landscape parameters
      cor_landscape = cor_landscape, # spatially correlations of hypotheses in landscape
      var_landscape = var_landscape, # variance in hypothesis quality in landscape
      # search parameters,
      max_step_sizes = max_step_sizes, # maximum step size for search agents (whole number)
      rejection_temp = rejection_temp # likelihood of accepting bad hypotheses
  )
  
  parameter_names <- rownames(sim_parameter_ranges)
  
  colnames(sim_parameter_ranges) <- c("Lower", "Upper")
  
  library(lhs)
  
  sim_parameters <- randomLHS(n_simulations, k = length(parameter_names))
  colnames(sim_parameters) <- parameter_names
  
  # initiate Latin hypercube sampling with defined ranges for each parameter
  for (useParam in parameter_names) {
    sim_parameters[, useParam] = sim_parameter_ranges[useParam, "Lower"] +
      sim_parameters[, useParam] * (sim_parameter_ranges[useParam, "Upper"] - sim_parameter_ranges[useParam, "Lower"])
  }
  sim_parameters[,"max_step_sizes"] <- round(sim_parameters[,"max_step_sizes"]) # step size should be whole number 
  sim_parameters
}

#' Function that runs multiple search simulations based on sampled parameters
#'
#' @param sim_parameters A data frame of rows containing the parameter values for each simulation, where each row pertains to a different simulation. 
#' @param grid_size The size of the search landscape (grid size^2)
#' @param n_iterations The number of iterations the search agents will run for. 
#' @param n_chains The number of chains searching each landscape.
#'
#' @return A list containing the simulation output for each run and chain. 
#' @export
#'
#' @examples
runSearchSimulation = function(sim_parameters, grid_size = 10, n_iterations, n_chains){
  
  # Make an empty list to store simulation information
  search_plot_list = list()
  simulation_runs_df = NULL
  
  # simulate search across different scenarios
  for (i in 1:nrow(sim_parameters)) {
    # reset parameters
    cor_landscape = NULL
    var_landscape = NULL
    max_step_sizes = NULL
    rejection_temp = NULL

    # Extract the simulation parameters for this run 
    cor_landscape = sim_parameters[i,"cor_landscape"]
    var_landscape = sim_parameters[i,"var_landscape"]
    max_step_size = sim_parameters[i, "max_step_sizes"]
    rejection_temp = sim_parameters[i,"rejection_temp"]
    
    
    # generate landscape
    landscape_i <-
      generataeLandscape(grid_size, cor_landscape, var_landscape)
  

       # search landscape
       search_j <-
         searchMH(
           n_iterations = n_iterations,
           landscape = landscape_i,
           n_chains = n_chains,
           max_step = max_step_size,
           rejection_temp = rejection_temp
         )
       
       # extract the position of the ground truth
       ground_truth_position_i <-
         as.data.frame(search_j[[1]]$ground_truth_position)

       
       # Empty object to store chain data
       all_chain_data <- NULL 
       
       
       # combine multiple chains into a single data frame
       for (chain in 1:n_chains){
         chain_data <- search_j[[chain]]
         df_chain <- cbind.data.frame(
           x = chain_data$x,
           y = chain_data$y,
           value = chain_data$search_values,
           #index = names(search_i),
           iteration = chain_data$iteration,
           cor_landscape = cor_landscape, 
           var_landscape = var_landscape, 
           max_step_size = max_step_size, 
           rejection_temp = rejection_temp,
           ground_truth_x = chain_data$ground_truth_position$x,
           ground_truth_y = chain_data$ground_truth_position$y,
           ground_truth_value = chain_data$ground_truth_value,
           # calculate distance of search from ground truth at each iteration
           distance_from_truth = sqrt((chain_data$x - chain_data$ground_truth_position$x) ^
                                        2 + (chain_data$y - chain_data$ground_truth_position$y) ^ 2
           ),
           sim_number = i,
           chain_number = chain
         )
         all_chain_data <- rbind(all_chain_data, df_chain)
       }
       
    # combine chain data with rest of the simulation
    simulation_runs_df <- rbind(simulation_runs_df, all_chain_data)
    #   search_plot_list[[i]] <- search_plot_i
    print(paste0("Simulation ", i, " done"))
  }
  # combine plots and data frame
  simulation_runs_df
}

