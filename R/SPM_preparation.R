#' Preparation for SPM map
#'
#' @param data_gz Main data file (.gz extension) with BOLD signal values.
#' @param lrt_array The array of p-values of LRT evaluated on model with condition included vs the one without.
#' @param dfs_model_comp The difference of degrees of freedom between models.
#'
#' @returns Two data overwritten objects. First with p-values and the other with z-values.
#' @export
#'
SPM_preparation <- \(data_gz, lrt_array, dfs_model_comp = 5) {

  # Setting the data to the array in nifiti file
  data_gz$setData(lrt_array)

  # Changing name
  data_gz$changeName('LRT_pvals')

  # Saving data in nifiti format
  writeNifti(data_gz)

  # Transforming to Z-values
  data_gz_val <- data_gz$clone()

  df <- dfs_model_comp
  qchisq(0.05, df = 5, lower.tail = FALSE)

  lrt_z <- lrt_array

  for (i in 1:dim(lrt_z)[1]) {
    for (j in 1:dim(lrt_z)[2]) {
      for (k in 1:dim(lrt_z)[3]) {
        lrt_z[i,j,k] <- qchisq(lrt_array[i,j,k], df = 5, lower.tail = FALSE)
          }
        }
      }

  data_gz_val$setData(lrt_z)
  data_gz_val$changeName('LRT_zvals')
  writeNifti(data_gz_val)

  return(list(
    "p-vals" = data_gz,
    "Z-vals" = data_gz_val
  ))
}

