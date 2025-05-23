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
  map <- apply(data_gz$data, c(1, 2, 3), function(x) ! all(x == x[1], na.rm = T))

  mv <- as.numeric(table(map)[2])
  pc <- 1

  for (i in 1:xdim) {
    for (j in 1:ydim) {
      for (k in 1:zdim) {


        if(map[i,j,k] == TRUE) {
          cat(paste0(Sys.time()),': processing voxel',pc,'out of',mv,'@ (x,y,z)=',i,j,k,'\n')

          temp_data <- prepare_data(data_gz, cond_1_txt, cond_2_txt, i, j, k)
          temp_fit <- try(fit_gam(temp_data$con_1, temp_data$con_2, temp_data$con_comb), silent = T)
          if (class(temp_fit) != 'try-error') {
            temp_lrt <- LRT_comparison(temp_fit$combined_condition, temp_fit$combined_no_condition)
            lrt_array[i, j, k] <- temp_lrt
          }
          pc = pc + 1
        }
      }
    }
  }
  return(lrt_array)
}
