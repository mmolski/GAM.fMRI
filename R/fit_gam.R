#' Function fitting the GAMs to previously pre-processed data.
#'
#' @param data_con_1 Data frame for condition 1
#' @param data_con_2 Data frame for condition 2
#' @param data_comb Data frame for combined conditions
#' @param smoothing_basis Smooth term
#' @param number_of_knots Basis dimension choice for smooths
#' @param estimating_method The smoothing parameter estimation method
#'
#' @returns List of 4 GAM models. First was fit to data containing condition 1 only, second condition 2 only. Next two intake combined data set, once fitted considering conditions and once not.
#' @export
#'
fit_gam <- \(
  data_con_1,
  data_con_2,
  data_comb,
  smoothing_basis = "tp",
  number_of_knots = 10,
  estimating_method = "REML"
  ) {

  gam_con_1 <- gam(
    fMRI_values ~ s(Time_to_Repetition, bs = smoothing_basis, k = number_of_knots)
    + s(base_values, bs = smoothing_basis, k = number_of_knots)
    + ti(Time_to_Repetition, base_values),
    data = data_con_1,
    method = estimating_method,
    select = TRUE
  )

  gam_con_2 <- gam(
    fMRI_values ~ s(Time_to_Repetition, bs = smoothing_basis, k = number_of_knots)
    + s(base_values, bs = smoothing_basis, k = number_of_knots)
    + ti(Time_to_Repetition, base_values),
    data = data_con_2,
    method = estimating_method,
    select = TRUE
  )

  gam_comb_cond <- gam(
    fMRI_values ~ s(Time_to_Repetition, bs = smoothing_basis, k = number_of_knots, by = condition)
    + s(base_values, bs = smoothing_basis, k = number_of_knots, by = condition)
    + ti(Time_to_Repetition, base_values) + condition,
    data = data_comb,
    method = estimating_method,
    select = TRUE
  )

  gam_comb_no_cond <- gam(
    fMRI_values ~ s(Time_to_Repetition, bs = smoothing_basis, k = number_of_knots)
    + s(base_values, bs = smoothing_basis, k = number_of_knots)
    + ti(Time_to_Repetition, base_values),
    data = data_comb,
    method = estimating_method,
    select = TRUE
  )

  return(
    list(
      "condition_1" = gam_con_1,
      "condition_2" = gam_con_2,
      "combined_condition" = gam_comb_cond,
      "combined_no_condition" = gam_comb_no_cond
    )
  )
}
