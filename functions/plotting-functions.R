library(tidyverse)
plotHypQuality = function(parameter,
                          data,
                          xlab = parameter,
                          ylab = "Mean Hypothesis Quality",
                          lm = TRUE,
                          ylim = NA,
                          xlim = NA,
                          smooth_method = NULL) {
  data$plot_param <- data[, parameter]
  data$is_ground_truth <- ifelse(data$value == data$ground_truth_value, TRUE, FALSE)
  data$scaled_col = (data$value - min(data$value)) / (max(data$value) - min(data$value)) * 1
  
  d_sum <-  data %>%
    group_by(plot_param) %>%
    summarise(
      n = n(),
      prob_ground_truth = sum(is_ground_truth) / n,
      mean = mean(value)
    )
  
  d_sum %>%
    mutate(p_hyp_scaled = (mean - min(mean)) / (max(mean) - min(mean)) * 1,
           norm_mean = mean / sum(mean)) %>%
    ggplot(aes(x = plot_param, y = mean)) +
    geom_smooth(linewidth = 2.5, method = smooth_method) +
    geom_point(alpha = .7) +
    ylim(0, ylim) +
    xlim(min(data$plot_param), xlim) +
    labs(x = xlab, y = ylab) +
    theme_bw() +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 25))
  
}
