#' Function that creates an array for SPM map
#'
#' @param data_gz Main data file (.gz extension) with BOLD signal values
#' @param cond_1_txt The file stating when stimuli 1 occurred
#' @param cond_2_txt The file stating when stimuli 2 occurred
#'
#' @returns The array of p-values that can be used to create a SPM map
#' @export
LRT_array <- \(data_gz, cond_1_txt, cond_2_txt) {

  xdim <- dim(data_gz)[1]
  ydim <- dim(data_gz)[2]
  zdim <- dim(data_gz)[3]
  lrt_array <- array(NA, dim = c(xdim, ydim, zdim))

  pb <- txtProgressBar(min = 0, max = xdim, style = 3)

  for (i in 1:xdim) {
    for (j in 1:ydim){
      for (k in 1:zdim) {

        temp_data <- prepare_data(data_gz, cond_1_txt, cond_2_txt, i, j, k)
        if (!(any(temp_data$fMRI_values == 0))) {
          temp_fit <- fit_gam(temp_data)
          temp_lrt <- LRT_comparison(temp_fit$combined_condition, temp_fit$combined_no_condition)
          lrt_array[i,j,k] <- temp_lrt
        }

      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(lrt_array)
}
