#' Function predicting the interpolated values of fMRI
#'
#' @param data_comb Data frame for combined conditions
#' @param gam_comb_cond Model which includes condition as a factor
#' @param gam_comb_no_cond Model which does not include condition as a factor
#' @param no_of_inter Number of interpolated values
#' @param CI Confidence interval for the plot drawing
#'
#' @returns A matrix with predictions for both models.
#' @export
#'
predict_gam <- \(data_comb, gam_comb_cond, gam_comb_no_cond, no_of_inter = 100, CI = 0.05) {

  predict_matrix <- matrix(nrow = no_of_inter * nrow(data_comb), ncol = 2)
  intrapolation_seq <- seq(1,10,length = no_of_inter)

  df_interpolated_combined <- data.frame(
    Time_to_Repetition = rep(intrapolation_seq, nrow(data_comb)/10),
    base_values = rep(data_comb$base_values, each = no_of_inter),
    condition = c(rep(1, no_of_inter * nrow(data_comb)/2), rep(2, no_of_inter * nrow(data_comb)/2))
  )

  predict(gam_comb_cond, newdata = df_interpolated_combined) -> predict_matrix[,1]
  predict(gam_comb_no_cond, newdata = df_interpolated_combined) -> predict_matrix[,2]

  Avg_pred_gams_combined_cond <- numeric(length = no_of_inter)
  Var_pred_gams_combined_cond <- numeric(length = no_of_inter)

  Avg_pred_gams_combined_no_cond <- numeric(length = no_of_inter)
  Var_pred_gams_combined_no_cond <- numeric(length = no_of_inter)

  for (i in 1:length(intrapolation_seq)) {
    temp_avg <- numeric(length = nrow(data_comb)/10)
    for (j in 1:(nrow(data_comb)/10)) {
      temp_avg[j] <- predict_matrix[i + (j-1) * no_of_inter, 1]
    }
    Var_pred_gams_combined_cond[i] <- var(temp_avg)
    Avg_pred_gams_combined_cond[i] <- mean(temp_avg)
  }

  for (i in 1:length(intrapolation_seq)) {
    temp_avg <- numeric(length = nrow(data_comb)/10)
    for (j in 1:(nrow(data_comb)/10)) {
      temp_avg[j] <- predict_matrix[i + (j-1) * no_of_inter, 2]
    }
    Var_pred_gams_combined_no_cond[i] <- var(temp_avg)
    Avg_pred_gams_combined_no_cond[i] <- mean(temp_avg)
  }

  SD_pred_gams_combined_cond <- sqrt(Var_pred_gams_combined_cond)
  SD_pred_gams_combined_no_cond <- sqrt(Var_pred_gams_combined_no_cond)

  SE_pred_gams_combined_cond <- SD_pred_gams_combined_cond/sqrt(length(SD_pred_gams_combined_cond))
  SE_pred_gams_combined_no_cond <- SD_pred_gams_combined_no_cond/sqrt(length(SD_pred_gams_combined_no_cond))

  CI_val <- qnorm(1 - CI/2)

  Upper_ci_combined_cond <- Avg_pred_gams_combined_cond + CI_val * SE_pred_gams_combined_cond
  Lower_ci_combined_cond <- Avg_pred_gams_combined_cond - CI_val * SE_pred_gams_combined_cond

  Upper_ci_combined_no_cond <- Avg_pred_gams_combined_no_cond + CI_val * SE_pred_gams_combined_no_cond
  Lower_ci_combined_no_cond <- Avg_pred_gams_combined_no_cond - CI_val * SE_pred_gams_combined_no_cond

  # Plotting predictions

  df_pred_plot <- data.frame(
    TR_vec = intrapolation_seq,
    fit_cond = Avg_pred_gams_combined_cond,
    fit_no_cond = Avg_pred_gams_combined_no_cond,
    upper_cond = Upper_ci_combined_cond,
    lower_cond = Lower_ci_combined_cond,
    upper_no_cond = Upper_ci_combined_no_cond,
    lower_no_cond = Lower_ci_combined_no_cond
  )

  pred_plot <- ggplot(df_pred_plot, aes(x = TR_vec)) +
    geom_line(aes(y = fit_cond), color = "skyblue2")  +
    geom_line(aes(y = upper_cond), color = "skyblue2", linetype = "dashed", alpha = 0.5, size = 1) +
    geom_line(aes(y = lower_cond), color = "skyblue2", linetype = "dashed", alpha = 0.5, size = 1) +
    geom_line(aes(y = fit_no_cond), color = "red3")  +
    geom_line(aes(y = upper_no_cond), color = "red3", linetype = "dashed", alpha = 0.5, size = 1) +
    geom_line(aes(y = lower_no_cond), color = "red3", linetype = "dashed", alpha = 0.5, size = 1) +
    labs(x = "Time to repetition (TR)", y = "BOLD signal", title = "GAM fit comparison") +
    theme_classic()

  return(list("prediction_matrix" = predict_matrix, "prediction_plot" = pred_plot))
}
