#' Function predicting the interpolated values of fMRI
#'
#'
#' @param data_con_1 Data frame for first conditions
#' @param data_con_2 Data frame for second conditions
#' @param data_comb Data frame for combined conditions
#' @param gam_con_1 Model which includes first condition only
#' @param gam_con_2 Model which includes second condition only
#' @param gam_comb_cond Model which includes condition as a factor
#' @param gam_comb_no_cond Model which does not include condition as a factor
#' @param no_of_inter Number of interpolated values
#' @param CI Confidence interval for the plot drawing

#'
#' @returns A matrix with predictions for both models.
#' @export
#'
predict_gam <- \(
  data_con_1,
  data_con_2,
  data_comb,
  gam_con_1,
  gam_con_2,
  gam_comb_cond,
  gam_comb_no_cond,
  no_of_inter = 100,
  CI = 0.05
  ) {

  num_TR_post <- max(data_comb$Time_to_Repetition)
  predict_matrix <- matrix(nrow = no_of_inter * nrow(data_comb), ncol = 2)
  intrapolation_seq <- seq(1, num_TR_post, length = no_of_inter)

  df_interpolated_combined <- data.frame(
    Time_to_Repetition = rep(intrapolation_seq, nrow(data_comb) / 10),
    base_values = rep(data_comb$base_values, each = no_of_inter),
    condition = c(rep(1, no_of_inter * nrow(data_comb) / 2), rep(2, no_of_inter * nrow(data_comb) / 2))
  )

  predict(gam_comb_cond, newdata = df_interpolated_combined) -> predict_matrix[, 1]
  predict(gam_comb_no_cond, newdata = df_interpolated_combined) -> predict_matrix[, 2]

  Avg_pred_gams_combined_cond <- numeric(length = no_of_inter)
  Var_pred_gams_combined_cond <- numeric(length = no_of_inter)

  Avg_pred_gams_combined_no_cond <- numeric(length = no_of_inter)
  Var_pred_gams_combined_no_cond <- numeric(length = no_of_inter)

  for (i in 1:length(intrapolation_seq)) {
    temp_avg <- numeric(length = nrow(data_comb) / 10)
    for (j in 1:(nrow(data_comb) / 10)) {
      temp_avg[j] <- predict_matrix[i + (j - 1) * no_of_inter, 1]
    }
    Var_pred_gams_combined_cond[i] <- var(temp_avg)
    Avg_pred_gams_combined_cond[i] <- mean(temp_avg)
  }

  for (i in 1:length(intrapolation_seq)) {
    temp_avg <- numeric(length = nrow(data_comb) / 10)
    for (j in 1:(nrow(data_comb) / 10)) {
      temp_avg[j] <- predict_matrix[i + (j - 1) * no_of_inter, 2]
    }
    Var_pred_gams_combined_no_cond[i] <- var(temp_avg)
    Avg_pred_gams_combined_no_cond[i] <- mean(temp_avg)
  }

  SD_pred_gams_combined_cond <- sqrt(Var_pred_gams_combined_cond)
  SD_pred_gams_combined_no_cond <- sqrt(Var_pred_gams_combined_no_cond)

  SE_pred_gams_combined_cond <- SD_pred_gams_combined_cond / sqrt(length(SD_pred_gams_combined_cond))
  SE_pred_gams_combined_no_cond <- SD_pred_gams_combined_no_cond / sqrt(length(SD_pred_gams_combined_no_cond))

  CI_val <- qnorm(1 - CI / 2)

  Upper_ci_combined_cond <- Avg_pred_gams_combined_cond + CI_val * SE_pred_gams_combined_cond
  Lower_ci_combined_cond <- Avg_pred_gams_combined_cond - CI_val * SE_pred_gams_combined_cond

  Upper_ci_combined_no_cond <- Avg_pred_gams_combined_no_cond + CI_val * SE_pred_gams_combined_no_cond
  Lower_ci_combined_no_cond <- Avg_pred_gams_combined_no_cond - CI_val * SE_pred_gams_combined_no_cond

  nonoverlap_index <- which(Lower_ci_combined_cond < Upper_ci_combined_no_cond |
                            Lower_ci_combined_no_cond < Upper_ci_combined_cond)
  # Plotting predictions

  df_pred_plot <- data.frame(
    TR_vec = intrapolation_seq,
    fit_cond = Avg_pred_gams_combined_cond,
    fit_no_cond = Avg_pred_gams_combined_no_cond,
    upper_cond = Upper_ci_combined_cond,
    lower_cond = Lower_ci_combined_cond,
    upper_no_cond = Upper_ci_combined_no_cond,
    lower_no_cond = Lower_ci_combined_no_cond,
    nonoverlap_TRs = intrapolation_seq[nonoverlap_index]
  )

  pred_plot <- ggplot(df_pred_plot, aes(x = TR_vec)) +
    geom_line(aes(y = fit_cond, color = "Condition"), size = 1.4)  +
    geom_line(
      aes(y = upper_cond, color = "Condition"),
      linetype = "dashed",
      alpha = 0.5,
      size = 1
    ) +
    geom_line(
      aes(y = lower_cond, color = "Condition"),
      linetype = "dashed",
      alpha = 0.5,
      size = 1
    ) +

    geom_line(aes(y = fit_no_cond, color = "No condition"), size = 1.4)  +
    geom_line(
      aes(y = upper_no_cond, color = "No condition"),
      linetype = "dashed",
      alpha = 0.5,
      size = 1
    ) +
    geom_line(
      aes(y = lower_no_cond, color = "No condition"),
      linetype = "dashed",
      alpha = 0.5,
      size = 1
    ) +

    scale_color_manual(
      name = "Model Type",
      values = c("Condition" = "skyblue2", "No condition" = "red3")
    ) +

    geom_rug(
      aes(x = nonoverlap_TRs),
      sides = "b",
      inherit.aes = TRUE,
      color = "black",
      alpha = 0.7,
      length = unit(0.05, "npc")
    ) +

    scale_x_continuous(breaks = seq(1, num_TR_post, by = 1)) +
    labs(x = "Time to repetition (TR)", y = "BOLD signal", title = "GAM fit comparison") +
    theme_bw() +
    theme(
      plot.title = element_text(size = 18, hjust = 0.5),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "bottom"
    )

  return(list(
    "prediction_matrix" = predict_matrix,
    "prediction_plot" = pred_plot
  ))
}
