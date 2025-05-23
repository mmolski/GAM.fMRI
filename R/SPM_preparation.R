SPM_preparation <- \(data_gz, lrt_array) {

  # Setting the data to the array in nifiti file
  data_gz$setData(lrt_array)

  # Changing name
  data_gz$changeName('LRT_pvals')

  # Saving data in nifiti format
  writeNifti(data_gz)

  # Transforming to Z-values
  data_gz_val <- data_gz$clone()

  df <- 5 # degrees of freedom

  lrt_z

  data_gz_val$setData(lrt_z)
  data_gz_val$changeName('LRT_zvals')
  writeNifti(data_gz_val)

  return(list(
    "p-vals" = data_gz,
    "Z-vals" = data_gz_val
  ))
}

