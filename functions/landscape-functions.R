library(MASS)
# Define the squared exponential kernel function
squaredExpKernel <- function(x1, x2, length_scale = 0.001, sigma_f = 100) {
  # x1 and x2 are the locations of two arms
  # length_scale controls the "smoothness" of the correlation
  # sigma_f controls the overall variance of the rewards
  dist_sq <- sum((x1 - x2)^2)
  sigma_f^2 * exp(-0.5 * dist_sq / length_scale^2)
}

# function to generate the covariance matrix from the kernel function
generateCovMatrix <- function(grid_size, length_scale, sigma_f) {
  # Set up a 2D grid of arms (e.g., 10x10)
  arms <- expand.grid(x = seq(1, grid_size), y = seq(1, grid_size))
  
  # Define the number of arms
  n_arms <- nrow(arms)
  
  n_arms <- nrow(arms)
  cov_matrix <- matrix(0, n_arms, n_arms)
  for (i in 1:n_arms) {
    for (j in 1:n_arms) {
      cov_matrix[i, j] <-
        squaredExpKernel(as.numeric(arms[i, ]),
                         as.numeric(arms[j, ]),
                         length_scale,
                         sigma_f)
    }
  }
  cov_matrix
}

# function that generates the rewards from a multivariate normal distribution
generateRewards <- function(cov_matrix) {    
  set.seed(100)
  n_arms <- nrow(cov_matrix)
  # Sample rewards from a multivariate normal distribution
  mean_rewards <- rep(20, n_arms)  # Mean rewards centered however because they'll be normalized. Higher is better to avoid negative values. 
  rewards <- mvrnorm(1, mu = mean_rewards, Sigma = cov_matrix)
  rewards
  
  # normalize rewards
  #norm_rewards <- rewards/sum(rewards)
  # # Normalize rewards using min-max scaling <- not sure if this is best approach, since it ensures a constant difference between the best and woest hypothesis
  # min_reward <- min(rewards)
  # max_reward <- max(rewards)
  # norm_rewards <- (rewards - min_reward) / (max_reward - min_reward)
  #norm_rewards
}


# function to generate likelihoods instead of rewards
generateLikelihoods <- function(cov_matrix) {
  set.seed(100)
  n_arms <- nrow(cov_matrix)

  # Sample from a multivariate normal distribution
  mean_values <- rep(20, n_arms)  # Mean values for the Gaussian process
  values <- mvrnorm(1, mu = mean_values, Sigma = cov_matrix)

  # Transform into likelihoods using exponentiation
  likelihoods <- exp(values)  # Exponentiate to ensure positivity

  # Normalize to sum to 1
  likelihoods <- likelihoods / sum(likelihoods)

  likelihoods
}

# generateLikelihoods <- function(cov_matrix) {    
#   set.seed(100)
#   n_arms <- nrow(cov_matrix)
#   
#   # Mean rewards with reduced centering
#   mean_rewards <- rep(10, n_arms) 
#   
#   # Sample rewards
#   rewards <- mvrnorm(1, mu = mean_rewards, Sigma = cov_matrix)
#   
#   # Apply softmax normalization to get likelihoods
#   softmax <- function(x) {
#     exp_x <- exp(x - max(x)) # Numerical stability
#     exp_x / sum(exp_x)
#   }
#   likelihoods <- softmax(rewards)
#   
#   likelihoods
# }


# Function to generate the landscape with likelihoods
generateLandscape <- function(grid_size, length_scale, sigma_f) {
  # Set up a 2D grid of arms/hypotheses (e.g., 10x10)
  arms <- expand.grid(x = seq(1, grid_size), y = seq(1, grid_size))
  
  # Generate covariance matrix
  cov_matrix <- generateCovMatrix(grid_size, length_scale, sigma_f)
  
  # Generate likelihoods
  arms <- arms %>% 
    mutate(likelihoods = generateLikelihoods(cov_matrix),
           index = 1:nrow(arms),
           rank = (max(index)+1)-rank(likelihoods, ties.method = "average") # rearrange so highest likelihood is ranked 1
    )

  # Return
  arms
}

t = generateLandscape(10,.1,.1)
