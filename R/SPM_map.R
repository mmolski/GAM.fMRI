SPM_map <- \(data_gz, lrt_array, x, y, z) {

  # Setting the data to the array in nifiti file
  data_gz$setData(lrt_array)

  # Changing name
  data_gz$changeName('LRT_sig_SPM')

  # Saving data in nifiti format
  writeNifti(data_gz)

  xdim = dim(data_gz)[1]
  ydim = dim(data_gz)[2]
  zdim = dim(data_gz)[3]

  layout(matrix(1:4, nrow=2, byrow=T))
  image(1:ydim, 1:zdim, data_gz[x,,])
  points(y, z, pch = 16, col = 'black')
  image(1:xdim, 1:zdim, data_gz[,y,])
  points(x, z, pch = 16, col = 'black')
  image(1:xdim, 1:ydim, data_gz[,,z])
  points(x, y, pch = 16, col = 'black')


}
