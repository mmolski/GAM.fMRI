#' Function preparing a data frame for the further analysis
#'
#' @param data_gz Main data file (.gz extension) with BOLD signal values
#' @param cond_1_txt The file stating when stimuli 1 occurred
#' @param cond_2_txt The file stating when stimuli 2 occurred
#' @param x First dimension
#' @param y Second dimension
#' @param z Third dimension
#'
#' @returns A polished data set prepared for modelling
#' @export
#'
prepare_data <- \(data_gz, cond_1_txt, cond_2_txt, x, y, z){

  # Setting up some objects

  TR <- data_gz$pixdim[5] # Time to repetition
  c1_time_in_TR <- cond_1_txt[,1]/TR
  c2_time_in_TR <- cond_2_txt[,1]/TR
  num_post <- 10 # Time between each stimulus of particular condition divided by the length of one TR
  c1_len <- length(c1_time_in_TR)
  c2_len <- length(c2_time_in_TR)
  c1_trials = matrix(NA, nrow = c1_len, ncol = num_post)
  c2_trials = matrix(NA, nrow = c2_len, ncol = num_post)
  TRs_vector <- paste(1:10)
  Atts_vec <- paste(1:20, "trial")
  colnames(c1_trials) <- colnames(c2_trials) <- TRs_vector
  rownames(c1_trials) <- rownames(c2_trials) <- Atts_vec

  for(i in 1:c1_len) {
    c1_trials[i,] = dat[x,y,z,c1_time_in_TR[i]:(c1_time_in_TR[i] + num_post - 1)]
  }

  for(i in 1:c2_len) {
    c2_trials[i,] = dat[x,y,z,c2_time_in_TR[i]:(c2_time_in_TR[i] + num_post - 1)]
  }

  # Wide to long format

  long_data_c1 <- pivot_longer(as.data.frame(c1_trials),
                               cols = all_of(TRs_vector),
                               names_to = "Time_to_Repetition",
                               values_to = "fMRI_values") %>% as.data.frame()


  long_data_c2 <- pivot_longer(as.data.frame(c2_trials),
                               cols = all_of(TRs_vector),
                               names_to = "Time_to_Repetition",
                               values_to = "fMRI_values") %>% as.data.frame()

  long_data_c1[,1] <- as.numeric(long_data_c1[,1])
  long_data_c2[,1] <- as.numeric(long_data_c2[,1])

  # Adding Base values

  base_values_c1 <- numeric(20)
  base_values_c2 <- numeric(20)
  seq(1,200, by = 10) -> my_seq

  for (i in 1:20) {
    long_data_c1[my_seq[i],2] -> base_values_c1[i]
    long_data_c2[my_seq[i],2] -> base_values_c2[i]
  }

  base_values_rep_c1 <- rep(base_values_c1, each = 10)
  base_values_rep_c2 <- rep(base_values_c2, each = 10)
  base_values_rep_combined <- c(base_values_rep_c1,base_values_rep_c2)

  long_data_c1_extra <- long_data_c1 %>% mutate(base_values = base_values_rep_c1, condition = rep(1, nrow(long_data_c1)))
  long_data_c2_extra <- long_data_c2 %>% mutate(base_values = base_values_rep_c2, condition = rep(2, nrow(long_data_c2)))

  long_data_combined <- bind_rows(long_data_c1_extra,long_data_c2_extra)
  long_data_combined$condition <- as.factor(long_data_combined$condition)

  return(list("cond1" = long_data_c1_extra, "cond2" = long_data_c2_extra, "comb" = long_data_combined))
}
