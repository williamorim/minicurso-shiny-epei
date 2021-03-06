gam_plot <- function(fit, var) {
  
  termos <- map(
    fit$smooth,
    ~ .x$term
  ) %>% 
    flatten_chr()
  
  index <- 1:length(termos)
  index <- index[termos == var]
  
  smooth <- fit$smooth[[index]]
  
  xlab <- var
  
  x <- fit$model[,smooth$term]
  
  if (smooth$by == "NA") {
    by.level = "NA"
  } else {
    by.level = smooth$by.level
  }
  
  range = tibble(x = x, by = by.level)
  names(range) = c(smooth$term, smooth$by)
  
  par <- smooth$first.para:smooth$last.para
  mat <- PredictMat(smooth, range)
  y <- (mat %*% fit$coefficients[par]) %>% as.numeric
  
  se <- ((mat %*% fit$Vp[par, par, drop = FALSE]) * mat) %>%
    rowSums %>% 
    sqrt
  
  df <- tibble(
    label = smooth$label,
    x.var = smooth$term,
    x.val = x,
    value = y,
    se = se
  )
  
  ggplot(df, aes(x.val, value)) +
    geom_ribbon(aes(ymin = value - 2*se, ymax = value + 2*se), 
                fill = "grey80") +
    geom_line(color = 'blue', size = 1) +
    #geom_point(aes(x = x.val, y = min(value-2*se)-sd(value))) +
    labs(y = 's(.)', x = xlab) +
    theme_bw()
  
}
