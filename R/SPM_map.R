#' Function which produces Statistical Parametric Maps
#'
#' @param data_gz_val Main data file (.gz extension) with BOLD signal values.
#' @param x x dimension of a map
#' @param y y dimension of a map
#' @param z z dimension of a map
#' @param prediction_plot_con The prediction plot of condition1 vs condition2 for a given voxel.
#' @param not_show_legend Option for legend of the whole plot
#' @param map_colour Colour of the map
#' @param legend_name Name of statistic values you use
#'
#' @returns An ggplot object (SPM)
#' @export
#'
SPM_map <- \(data_gz_val,  x, y, z, prediction_plot_con = NULL, not_show_legend = NULL, map_colour = "plasma", legend_name = "Z-value") {

  # Extracting dimensions

  xdim = dim(data_gz_val$data)[1]
  ydim = dim(data_gz_val$data)[2]
  zdim = dim(data_gz_val$data)[3]

  layout(matrix(1:4, nrow=2, byrow=T))

  # Reshaping the 3D array into a long format, so that ggplot2 likes it

  xy_slice_data <- expand.grid(x = 1:xdim, y = 1:ydim) %>%
    mutate(value = as.vector(data_gz_val$data[,,z]))

  xz_slice_data <- expand.grid(x = 1:xdim, z = 1:zdim) %>%
    mutate(value = as.vector(data_gz_val$data[,y,]))

  yz_slice_data <- expand.grid(y = 1:ydim, z = 1:zdim) %>%
    mutate(value = as.vector(data_gz_val$data[x,,]))

  # Data frame for the xyz point

  point_data <- data.frame(x = x, y = y, z = z)

  # Creating ggplot2 plots for each slice

  p_xy <- ggplot(xy_slice_data, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_viridis_c(option = map_colour, name = legend_name) +
    geom_point(data = point_data, aes(x = x, y = y),
               color = 'red2', shape = 4, size = 3, stroke = 1.5, inherit.aes = FALSE) +
    labs(title = paste0("Brain slice (Z = ", z, ")"), x = "X", y = "Y") +
    coord_fixed(ratio = 1) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          legend.position = "none")

  p_xz <- ggplot(xz_slice_data, aes(x = x, y = z, fill = value)) +
    geom_raster() +
    scale_fill_viridis_c(option = map_colour, name = "Z-value") +
    geom_point(data = point_data, aes(x = x, y = z),
               color = 'red2', shape = 4, size = 3, stroke = 1.5, inherit.aes = FALSE) +
    labs(title = paste0("Brain slice (Y = ", y, ")"), x = "X", y = "Z") +
    coord_fixed(ratio = 1) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          legend.position = not_show_legend)

  p_yz <- ggplot(yz_slice_data, aes(x = y, y = z, fill = value)) +
    geom_raster() +
    scale_fill_viridis_c(option = map_colour, name = "Z-value") +
    geom_point(data = point_data, aes(x = y, y = z),
               color = 'red2', shape = 4, size = 3, stroke = 1.5, inherit.aes = FALSE) +
    labs(title = paste0("Brain slice (X = ", x, ")"), x = "Y", y = "Z") +
    coord_fixed(ratio = 1) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          legend.position = "none")

  if (!is.null(prediction_plot_con)) {
    prediction_plot_con <- prediction_plot_con +
      theme(plot.title = element_text(size = 12, hjust = 0.5),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10))
  }


  # Combining plots

  combined_plot <- p_yz + p_xz + p_xy + prediction_plot_con + plot_layout(ncol = 2, byrow = TRUE)

  final_plot <- combined_plot +
    plot_annotation(
      title = paste0("Overall brain slices and GAM fit analysis for voxel ", "(", x , ",", y , ",", z , ")"),
      caption = "The threshold value for Z is 11.0705 for 95% significance",
      theme = theme(plot.title = element_text(size = 18, hjust = 0.5),
                    plot.caption = element_text(size = 8, hjust = 0.5))
    )

  return(final_plot)
}
