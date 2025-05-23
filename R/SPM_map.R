SPM_map <- \(data_gz_val,  x, y, z) {



  xdim = dim(data_gz_val$data)[1]
  ydim = dim(data_gz_val$data)[2]
  zdim = dim(data_gz_val$data)[3]

  layout(matrix(1:4, nrow=2, byrow=T))
  image(1:ydim, 1:zdim, data_gz_val$data[x,,])
  points(y, z, pch = 16, col = 'black')
  image(1:xdim, 1:zdim, data_gz_val$data[,y,])
  points(x, z, pch = 16, col = 'black')
  image(1:xdim, 1:ydim, data_gz_val$data[,,z])
  points(x, y, pch = 16, col = 'black')

  # Try

  library(ggplot2)

  library(reshape2)

  # Convert matrix to long format

  df <- melt(mat)

  names(df) <- c("Y", "X", "value")  # match coordinate names

  # Flip Y axis so it looks like base R image()

  ggplot(df, aes(x = X, y = Y, fill = value)) +

    geom_raster() +

    scale_y_reverse() +  # to mimic base R's image() orientation

    scale_fill_gradientn(colors = terrain.colors(10)) +

    theme_minimal()

  mat <- matrix(1:100, nrow = 10)

}
