#' Function fitting the GAMs to previously pre-processed data.
#'
#'
#' @param data_comb Data frame for combined conditions
#' @param smoothing_basis Smooth term
#' @param number_of_knots Basis dimension choice for smooths
#' @param estimating_method The smoothing parameter estimation method
#'
#' @returns List of 2 GAM models. One including condition as a factor and one not.
#' @export
#'
fit_gam <- \(
  data_comb,
  smoothing_basis = "tp",
  number_of_knots = 10,
  estimating_method = "REML"
) {

  gam_comb_no_cond <- gam(
    fMRI_values ~ s(Time_to_Repetition, bs = smoothing_basis, k = number_of_knots)
    + s(base_values, bs = smoothing_basis, k = number_of_knots)
    + ti(Time_to_Repetition, base_values),
    data = data_comb,
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

  return(
    list(
      "combined_condition" = gam_comb_cond,
      "combined_no_condition" = gam_comb_no_cond
    )
  )
}
