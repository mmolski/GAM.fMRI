#' Function for modelling fMRI data
#'
#' @param data_frame A processed data frame
#' @param smoothing_basis A smoothing function used for modelling
#' @param number_of_knots A number of knots used for local smoothing
#' @param method A method of estimation
#'
#' @returns A model
#' @export
fit_gam <- \(data_frame, smoothing_basis = "tp", number_of_knots = 10, estimating_method = "REML"){

  g <- gam(fMRI_values ~ s(Time_to_Repetition, bs = smoothing_basis, k = number_of_knots, by = condition)
      + s(base_values, bs = bs_options[i], k = number_of_knots, by = condition)
      + ti(Time_to_Repetition, base_values) + condition,
      data = data_frame, method = estimating_method, select = TRUE)
  return(g)
}
