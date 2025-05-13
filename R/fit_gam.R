# Function for modelling

fit_gam <- \(data_frame, smoothing_basis = "tp", number_of_knots = 10, method = "REML"){

  gam(fMRI_values ~ s(Time_to_Repetition, bs = bs_options[i], k = 10, by = condition)
      + s(base_values, bs = bs_options[i], k = 10, by = condition)
      + ti(Time_to_Repetition, base_values) + condition,
      data = long_data_combined, method = "REML", select = TRUE)

}
