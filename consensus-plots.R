library(here)
library(tidyverse)
load(here("output/simdata-100-simulations-100-iterations-20-chains-15-grid-size.Rdata"))

# Add a column to count how many other chains are at the same position
df <- simulated_data %>%
  group_by(sim_number, iteration) %>%
  mutate(
    n_other_agents_per_iter = sapply(
      seq_along(x), 
      function(i) sum(x[i] == x[-i] & y[i] == y[-i] & chain_number[i] != chain_number[-i])
    ))
  # ) %>%
  # ungroup() %>%
  # group_by(sim_number) %>%
  # mutate(n_other_agents_total = sapply(
  #   seq_along(x), 
  #   function(i) sum(x[i] == x[-i] & y[i] == y[-i] & chain_number[i] != chain_number[-i])
  # )) %>%
  # ungroup()

summ_cons = df %>% 
  group_by(sim_number, x, y) %>%
  summarise(total_consensus = sum(n_other_agents_per_iter), value = mean(value)) %>%
  group_by(total_consensus) %>%
  summarise(value = mean(value))

df %>%
  group_by(sim_number, x) %>%
  summarise(value = sum(value))

cons_quality <- df %>% 
  #filter(rejection_temp > 5) %>%
  group_by(n_other_agents_per_iter) %>%
  summarise(mean_quality = mean(value), n = n()) %>%
  mutate(percent = n_other_agents_per_iter/max(df$chain_number))

plot(cons_quality$n_other_agents_per_iter, cons_quality$mean_quality)

plot(cons_quality$percent, cons_quality$mean_quality)


summ_nsubj <- df %>%
  #filter(rejection_temp > 7) %>%
  group_by(sim_number,x,y) %>%
  summarise(count = n(), quality = mean(value), n_subj = length(unique(chain_number)))  %>%
  group_by(n_subj) %>%
  summarise(quality = mean(quality), n_subj = max(n_subj))
  
plot(summ_nsubj$n_subj, summ_nsubj$quality)

summ_count <- df %>%
  filter(rejection_temp > 7) %>%
  group_by(sim_number,x,y) %>%
  summarise(count = n(), quality = mean(value), n_subj = length(unique(chain_number)))  %>%
  group_by(count) %>%
  summarise(quality = mean(quality), n_subj = max(n_subj))

plot(summ_count$count, summ_count$quality)



unique(df$chain_number)
