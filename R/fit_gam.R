#' Fitting the GAMs
#'
#' @param data_con_1 Data frame for condition 1
#' @param data_con_2 Data frame for condition 2
#' @param data_comb Data frame for combined conditions
#' @param smoothing_basis Smooth term
#' @param number_of_knots Basis dimension choice for smooths
#' @param estimating_method The smoothing parameter estimation method
#'
#' @returns List of 3 models.
#' @export
#'
fit_gam <- \(data_con_1, data_con_2, data_comb, smoothing_basis = "tp", number_of_knots = 10, estimating_method = "REML"){

  gam_1 <- gam(fMRI_values ~ s(Time_to_Repetition, bs = smoothing_basis, k = number_of_knots) + s(base_values, bs = smoothing_basis, k = number_of_knots)
      + ti(Time_to_Repetition, base_values), data = data_con_1, method = estimating_method, select = TRUE)

  gam_2 <- gam(fMRI_values ~ s(Time_to_Repetition, bs = smoothing_basis, k = number_of_knots) + s(base_values, bs = smoothing_basis, k = number_of_knots)
      + ti(Time_to_Repetition, base_values), data = data_con_2, method = estimating_method, select = TRUE)

  gam_comb <- gam(fMRI_values ~ s(Time_to_Repetition, bs = smoothing_basis, k = number_of_knots, by = condition)
      + s(base_values, bs = smoothing_basis, k = number_of_knots, by = condition)
      + ti(Time_to_Repetition, base_values) + condition,
      data = data_comb, method = estimating_method, select = TRUE)

  return(list("condition 1" = gam_1, "condition 2" = gam_2, "combined conditions" = gam_comb))
}
