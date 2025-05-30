#' Preparation for SPM map
#'
#' @param data_gz Main data file (.gz extension) with BOLD signal values.
#' @param lrt_array The array of p-values of LRT evaluated on model with condition included vs the one without.
#' @param dfs_model_comp The difference of degrees of freedom between models.
#' @param mult_test_method The method of p-values adjustment.
#'
#' @returns Two data overwritten objects. First with p-values and the other with z-values.
#' @export
#'
SPM_preparation <- \(data_gz, lrt_array, dfs_model_comp = 5, mult_test_method = "hochberg") {

  # Setting the data to the array in nifiti file
  data_gz$setData(lrt_array)

  # Changing name
  data_gz$changeName('LRT_pvals')

  # Saving data in nifiti format
  writeNifti(data_gz)

  # Transforming to Z-values
  data_gz_val <- data_gz$clone()

  org_dims <- dim(lrt_array)
  df <- dfs_model_comp
  lrt_z <- p.adjust(lrt_array, method = mult_test_method)
  lrt_z_adj <- array(lrt_z, dim = org_dims)

  # browser()

  for (i in 1:dim(lrt_z_adj)[1]) {
    for (j in 1:dim(lrt_z_adj)[2]) {
      for (k in 1:dim(lrt_z_adj)[3]) {
        lrt_z_adj[i,j,k] <- qchisq(lrt_z_adj[i,j,k], df = 5, lower.tail = FALSE)
          }
        }
      }

  data_gz_val$setData(lrt_z_adj)
  data_gz_val$changeName('LRT_zvals_adj')
  writeNifti(data_gz_val)

  # Binarizing
  data_gz_bin <- data_gz$clone()

  data_gz_bin$setData(as.numeric(lrt_z_adj < qchisq(0.05, df = 5, lower.tail = FALSE)))
  data_gz_bin$changeName('LRT_binarized')
  writeNifti(data_gz_bin)

  return(list(
    "p-vals" = data_gz,
    "Z-vals" = data_gz_val,
    "Bin-vals" = data_gz_bin
  ))
}

