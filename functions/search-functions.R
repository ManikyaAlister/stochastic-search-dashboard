
# simple metropolis-Hastings style search algorithm
searchMH <- function(n_iterations,
                     landscape_df,
                     rejection_temp = 1,
                     max_step = 3,
                     n_chains = 1) {
  
  # Ensure input is in the expected format
  stopifnot(all(c("x", "y", "likelihoods") %in% colnames(landscape_df)))
  
  # Identify the ground truth
  ground_truth_value <- max(landscape_df$likelihoods)
  ground_truth_position <- 
    landscape_df[landscape_df$likelihoods == ground_truth_value, c("x", "y")]
  
  # set up empty list to store data from all chains
  all_chain_data <- vector("list", n_chains)
  
  # loop through each chains -- this is pretty inefficient but simple and should be okay since simulations aren't massive
  for (chain in 1:n_chains) {
    # initialize empty list for chain data in this particular chain
    chain_data <- list()
    
    # Initialize storage vectors
    search_values <- numeric(n_iterations) # for values of accepted samples
    search_positions_x <- numeric(n_iterations) # for x positions of accepted samples
    search_positions_y <- numeric(n_iterations) # for y positions of accepted samples
    ranks <- numeric(n_iterations)
    # For plotting, I also want to store all proposal samples, even those that weren't sampled. 
    all_proposal_values <- c()
    all_proposal_x <- c()
    all_proposal_y <- c()
    all_proposal_decision <- c() # whether the proposal was rejected or accepted
    
    # Random starting points
    starting_position <- landscape_df[sample(1:nrow(landscape_df), size = 1), c("x", "y")]
    current_position <- as.numeric(starting_position)
    
    # Set first value and position
    current_value <- landscape_df[
      landscape_df$x == current_position[1] & 
        landscape_df$y == current_position[2], "likelihoods"]
    
    ranks[1] <- landscape_df[landscape_df$x == current_position[1] & 
                                 landscape_df$y == current_position[2], "rank"]
    
    search_values[1] <- current_value
    search_positions_x[1] <- current_position[1]
    search_positions_y[1] <- current_position[2]
    all_proposal_values[1] <- current_value
    all_proposal_x[1] <- current_position[1]
    all_proposal_y[1] <- current_position[2]
    all_proposal_decision[1] <- "accept"
    
    # # Rejection rule function with a scaling factor
    # rejection_rule <- function(current_value, proposal_value, temp = 1) {
    #   
    #   # normalize values to ensure they are not negative
    #   proposal_value <- proposal_value - min(proposal_value, current_value)
    #   current_value <- current_value - min(proposal_value, current_value)
    #   print(proposal_value)
    #   print(current_value)
    #   
    #   log_proposal <- log(proposal_value)
    #   log_current <- log(current_value)
    #   # Calculate acceptance probability in log-space
    #   log_acceptance_prob <- (log_proposal - log_current) / temp
    #   acceptance_prob <- min(1, exp(log_acceptance_prob)) 
    #   print(acceptance_prob)
    #   runif(1) < acceptance_prob
    # }

    
    rejection_rule <- function(current_log_value, proposal_log_value, temp = 1) {
      # Calculate acceptance probability using log values
      acceptance_prob <- min(1, exp((proposal_log_value - current_log_value) / temp))
      runif(1) < acceptance_prob
    }

    
    
    # Begin sampling process
    for (i in 1:(n_iterations - 1)) {
      proposal_position <- current_position # set as the same for now for while loop 
      # make sure samples is not 1) out of the space and 2) the same as the current position
      while (!all(proposal_position %in% 1:max(landscape_df$x)) || all(current_position == proposal_position)) {
        
        get_new_position <- function(current_position, max_step) {
          # Generate a random step within the uniform range [-max_step, max_step]
          step <- sample(-max_step:max_step, 1)
          current_position + step
        }
        
        
        # Generate new proposal positions for each dimension
        proposal_position <- c(
          get_new_position(current_position[1], max_step),
          get_new_position(current_position[2], max_step)
        )
      }
      # get the value of the proposal position
      proposal_value <- landscape_df[
        landscape_df$x == proposal_position[1] & 
          landscape_df$y == proposal_position[2], "likelihoods"]
      
      # Acceptance/rejection
      if (proposal_value > current_value) {
        # Automatically accept if the proposal improves the likelihood
        current_position <- proposal_position
        current_value <- proposal_value
        proposal_decision <- "accept"
      } else if (rejection_rule(current_value, proposal_value, temp = rejection_temp)) {
        # Accept based on the rejection rule with scaling
        current_position <- proposal_position
        current_value <- proposal_value
        proposal_decision <- "accept"
      } else {
        # Reject otherwise
        proposal_decision <- "reject"
      }
      
      
      # Record current position and value
      search_positions_x[i + 1] <- current_position[1]
      search_positions_y[i + 1] <- current_position[2]
      search_values[i + 1] <- current_value
      ranks[i+1] <- landscape_df[landscape_df$x == current_position[1] & 
        landscape_df$y == current_position[2], "rank"]
      
      # if proposal was accepted, record that sample 
      if (proposal_decision == "accept"){
        all_proposal_values <- c(all_proposal_values, current_value)
        all_proposal_x <- c(all_proposal_x, current_position[1])
        all_proposal_y <- c(all_proposal_y, current_position[2])
        all_proposal_decision <- c(all_proposal_decision,"accept")
        # if proposal was rejected, record both the rejected sample and the original sample
      } else if (proposal_decision == "reject"){
        all_proposal_values <- c(all_proposal_values, proposal_value, current_value)
        all_proposal_x <- c(all_proposal_x, proposal_position[1], current_position[1])
        all_proposal_y <- c(all_proposal_y, proposal_position[2], current_position[2])
        all_proposal_decision <- c(all_proposal_decision, "reject", "accept")
      }
    }
    
    # Return results
    chain_data <- list(
      search_values = search_values,
      rank = ranks,
      x = search_positions_x,
      y = search_positions_y,
      all_proposal_values = all_proposal_values,
      all_proposal_x = all_proposal_x,
      all_proposal_y = all_proposal_y,
      all_proposal_decision = all_proposal_decision,
      ground_truth_position = ground_truth_position,
      ground_truth_value = ground_truth_value,
      iteration = 1:n_iterations,
      starting_points = starting_position,
      chain = chain
      )
    
    all_chain_data[[chain]] <- chain_data
  }
  all_chain_data
}

