library(tidyverse)
plotHypQuality = function(parameter,
                          data,
                          stat = "median",
                          xlab = parameter,
                          ylab = "Likelihood of Testimony",
                          colour_direction = 1,
                          lm = TRUE,
                          title = NULL, 
                          ylim = NA,
                          xlim = NA,
                          smooth_method = "loess") {
  data = as.data.frame(data)
  data$plot_param <- data[, parameter]
  data$is_ground_truth <- ifelse(data$value == data$ground_truth_value, TRUE, FALSE)
  data$scaled_col = (data$value - min(data$value)) / (max(data$value) - min(data$value)) * 1

  d_sum <-  data %>%
    group_by(plot_param) %>%
    # create a normalised value where each hypothesis quality is proportionate to the best hypothesis in a landscape. 
    mutate(norm_value = log(ground_truth_value)/log(value)) %>% # log so that when value == truth = 1 and when value < truth = <1  
    summarise(
      n = n(),
      prob_ground_truth = sum(is_ground_truth) / n,
      median = median(value),
      median_norm = median(norm_value),
      median_rank = median(rank)
    )%>%
    mutate(p_hyp_scaled = (median - min(median)) / (max(median) - min(median)) * 1,
           norm_median = median / sum(median)) 
  
  d_sum %>%
    ggplot(aes(x = plot_param, y = get(stat))) +
    geom_point(alpha = .2, aes(colour = get(stat))) +
    geom_smooth(linewidth = 2.8, method = smooth_method, colour = "white") + # white outline
    geom_smooth(linewidth = 2.4, method = smooth_method, colour = "black") + 
    scale_colour_viridis_c(option = "plasma", direction = colour_direction, limits = c(0,ylim))+
    
    ylim(0, ylim) +
    xlim(min(data$plot_param), xlim) +
    labs(x = xlab, y = ylab, title = title) +
    theme_bw() +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 24, hjust = 0.5),
          legend.position = "none")
  
}


plotConsensusQuality = function(
    data,
    variable = "Knowability",
    stat = "median",
    #xlab = parameter,
    ylab = "Likelihood of Testimony",
    title = NULL,
    lm = TRUE,
    ylim = NA,
    xlim = NA,
    smooth_method = "loess"){
  
  cons_quality <- data %>% 
    #filter(rejection_temp > 5) %>%
    group_by(n_other_agents_per_iter, !!sym(variable)) %>%
    summarise(median = median(value), median_rank = median(rank), n = n()) %>%
    mutate(percent_of_agents_per_iter = n_other_agents_per_iter/max(data$chain_number))
  
  p <- cons_quality %>%
    ggplot(aes(x = percent_of_agents_per_iter, y = !!sym(stat), colour = !!sym(variable), fill = !!sym(variable))) +
    geom_point(alpha = .6)+
    geom_smooth(method = smooth_method, linewidth = 2.5, alpha = .4)+
    scale_color_brewer(palette = "Set2")+
    scale_fill_brewer(palette = "Set2")+
    labs(y = ylab,x = "Consensus Proportion", title = title)+
    theme_bw()+
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 24, hjust = 0.5),
    )
  
  p
}
