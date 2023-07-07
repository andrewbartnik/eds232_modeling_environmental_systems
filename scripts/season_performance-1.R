#' summer performance metrics
#'
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param wy water year
#' @param cor_weight weight for correlation performance metric
#' @param rmse_weight weight for rmse performance metric
#' @return cor, rmse, combined

library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)

calculate_season_performance <- function(m, o, wy, month, cor_weight = 0.5, rmse_weight = 0.5){

  sager_flow <- cbind.data.frame(m,o, wy, month)
  # filter the dataframe for the specified summer months
  sager_summer <- sager_flow |>
    filter(month %in% c(6, 7, 8)) |>
    group_by(wy) |>
    summarize(model = sum(m), obs = sum(o)) |>
    ungroup()

  # calculate the correlation and RMSE between model and obs
  cor_value <- cor(sager_summer$model, sager_summer$obs)
  rmse_value <- sqrt(mean((sager_summer$model - sager_summer$obs)^2))

  # normalize the values to be on the same scale (0-1)
  cor_norm <- (cor_value + 1) / 2
  rmse_norm <- 1 - rmse_value / max(sager_summer$obs)

  # calculate the combined performance metric based on the specified weights
  perf_metric <- cor_weight * cor_norm + rmse_weight * rmse_norm

  return(list(cor=cor_norm, rmse=rmse_norm, combined=perf_metric))
}

#Correlation gives an indication of the strength and direction of the relationship
#between the predicted and observed values to indicate model performance.
#RMSE gives an indication of how well the model predictions match the
#observed values, and this metric is more sensitive to large errors, which
#helps us to identify significant model differences. We use these metrics
#together to provide a comprehensive assessment of model performance, because
#they account for both accuracy and precision of model predictions.

