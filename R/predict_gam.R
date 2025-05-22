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
  intrapolation_seq <- seq(1, num_TR_post, length = no_of_inter)
  predict_matrix_comb <- matrix(nrow = no_of_inter * nrow(data_comb), ncol = 2)
  predict_matrix_con <- matrix(nrow = no_of_inter * nrow(data_con_1), ncol = 2)

  df_interpolated_c1 <- data.frame(
    Time_to_Repetition = rep(intrapolation_seq, nrow(data_con_1) / 10),
    base_values = rep(data_con_1$base_values, each = no_of_inter)
  )

  df_interpolated_c2 <- data.frame(
    Time_to_Repetition = rep(intrapolation_seq, nrow(data_con_2) / 10),
    base_values = rep(data_con_2$base_values, each = no_of_inter)
  )

  df_interpolated_combined <- data.frame(
    Time_to_Repetition = rep(intrapolation_seq, nrow(data_comb) / 10),
    base_values = rep(data_comb$base_values, each = no_of_inter),
    condition = c(rep(1, no_of_inter * nrow(data_comb) / 2), rep(2, no_of_inter * nrow(data_comb) / 2))
  )


  predict(gam_con_1, newdata = df_interpolated_c1) -> predict_matrix_con[, 1]
  predict(gam_con_2, newdata = df_interpolated_c2) -> predict_matrix_con[, 2]
  predict(gam_comb_cond, newdata = df_interpolated_combined) -> predict_matrix_comb[, 1]
  predict(gam_comb_no_cond, newdata = df_interpolated_combined) -> predict_matrix_comb[, 2]

  Avg_pred_gams_con_1 <- numeric(length = no_of_inter)
  Var_pred_gams_con_1 <- numeric(length = no_of_inter)

  Avg_pred_gams_con_2 <- numeric(length = no_of_inter)
  Var_pred_gams_con_2 <- numeric(length = no_of_inter)

  Avg_pred_gams_combined_cond <- numeric(length = no_of_inter)
  Var_pred_gams_combined_cond <- numeric(length = no_of_inter)

  Avg_pred_gams_combined_no_cond <- numeric(length = no_of_inter)
  Var_pred_gams_combined_no_cond <- numeric(length = no_of_inter)

  for (i in 1:length(intrapolation_seq)) {
    temp_avg <- numeric(length = nrow(data_con_1) / 10)
    for (j in 1:(nrow(data_con_1) / 10)) {
      temp_avg[j] <- predict_matrix_con[i + (j - 1) * no_of_inter, 1]
    }
    Var_pred_gams_con_1[i] <- var(temp_avg)
    Avg_pred_gams_con_1[i] <- mean(temp_avg)
  }

  for (i in 1:length(intrapolation_seq)) {
    temp_avg <- numeric(length = nrow(data_con_2) / 10)
    for (j in 1:(nrow(data_con_2) / 10)) {
      temp_avg[j] <- predict_matrix_con[i + (j - 1) * no_of_inter, 2]
    }
    Var_pred_gams_con_2[i] <- var(temp_avg)
    Avg_pred_gams_con_2[i] <- mean(temp_avg)
  }

  for (i in 1:length(intrapolation_seq)) {
    temp_avg <- numeric(length = nrow(data_comb) / 10)
    for (j in 1:(nrow(data_comb) / 10)) {
      temp_avg[j] <- predict_matrix_comb[i + (j - 1) * no_of_inter, 1]
    }
    Var_pred_gams_combined_cond[i] <- var(temp_avg)
    Avg_pred_gams_combined_cond[i] <- mean(temp_avg)
  }

  for (i in 1:length(intrapolation_seq)) {
    temp_avg <- numeric(length = nrow(data_comb) / 10)
    for (j in 1:(nrow(data_comb) / 10)) {
      temp_avg[j] <- predict_matrix_comb[i + (j - 1) * no_of_inter, 2]
    }
    Var_pred_gams_combined_no_cond[i] <- var(temp_avg)
    Avg_pred_gams_combined_no_cond[i] <- mean(temp_avg)
  }

  SD_pred_gams_con_1 <- sqrt(Var_pred_gams_con_1)
  SD_pred_gams_con_2 <- sqrt(Var_pred_gams_con_2)

  SE_pred_gams_con_1 <- SD_pred_gams_con_1 / sqrt(length(SD_pred_gams_con_1))
  SE_pred_gams_con_2 <- SD_pred_gams_con_2 / sqrt(length(SD_pred_gams_con_2))

  SD_pred_gams_combined_cond <- sqrt(Var_pred_gams_combined_cond)
  SD_pred_gams_combined_no_cond <- sqrt(Var_pred_gams_combined_no_cond)

  SE_pred_gams_combined_cond <- SD_pred_gams_combined_cond / sqrt(length(SD_pred_gams_combined_cond))
  SE_pred_gams_combined_no_cond <- SD_pred_gams_combined_no_cond / sqrt(length(SD_pred_gams_combined_no_cond))

  CI_val <- qnorm(1 - CI / 2)

  Upper_ci_con_1 <- Avg_pred_gams_con_1 + CI_val * SE_pred_gams_con_1
  Lower_ci_con_1 <- Avg_pred_gams_con_1 - CI_val * SE_pred_gams_con_1

  Upper_ci_con_2 <- Avg_pred_gams_con_2 + CI_val * SE_pred_gams_con_2
  Lower_ci_con_2 <- Avg_pred_gams_con_2 - CI_val * SE_pred_gams_con_2

  Upper_ci_combined_cond <- Avg_pred_gams_combined_cond + CI_val * SE_pred_gams_combined_cond
  Lower_ci_combined_cond <- Avg_pred_gams_combined_cond - CI_val * SE_pred_gams_combined_cond

  Upper_ci_combined_no_cond <- Avg_pred_gams_combined_no_cond + CI_val * SE_pred_gams_combined_no_cond
  Lower_ci_combined_no_cond <- Avg_pred_gams_combined_no_cond - CI_val * SE_pred_gams_combined_no_cond

  nonoverlap_index_con <- which(Lower_ci_con_1 < Upper_ci_con_2 |
                                  Lower_ci_con_2 < Upper_ci_con_1)

  nonoverlap_index_comb <- which(Lower_ci_combined_cond < Upper_ci_combined_no_cond |
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
    nonoverlap_TRs_comb = intrapolation_seq[nonoverlap_index_comb]
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
      aes(x = nonoverlap_TRs_comb),
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
