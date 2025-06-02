
# GAM.fMRI

<!-- badges: start -->
<!-- badges: end -->

The goal of GAM.fMRI package is to help researchers achieve better inference on fMRI data.
I would highly recommend trying this approach out, if you love seeking neurological truths.

"All models are approximations. **Essentially, all models are wrong, but some are useful.** However, the model must always be borne in mind."

<table align="center">
  <tr>
    <td align="center">
      George E. P. Box
    </td>
  </tr>
</table>


## Installation

You can install the development version of GAM.fMRI from [GitHub](https://github.com/mmolski/GAM.fMRI) with:

``` r
# install.packages("devtools")
devtools::install_github("mmolski/GAM.fMRI")
```

## Example

I have created a template for your data analysis below:

``` r
# Section for calling the packages

library(GAM.fMRI)

# Section for loading the data for the analysis

data_fmri = niftiR6::readNifti('your_data_file_name.gz')

# The files which include the timings of two stimuli 

condition_1 = read.table('your_timing_file_condition_1.txt')
condition_2 = read.table('your_timing_file_condition_2.txt')

# Setting up the dimension for the voxel of your interest.

x <- NULL
y <- NULL
z <- NULL

# Data preparation (wrangling)

dat_prep <- prepare_data(
  data_gz = data_fmri,
  cond_1_txt = condition_1,
  cond_2_txt = condition_2,
  x = x,
  y = y,
  z = z
)

# Fitting the GAM

gam_fit <- fit_gam(dat_prep$con_1, dat_prep$con_2, dat_prep$con_comb)

# Testing predictions

preds_and_plots <- predict_and_plot_gam(
  dat_prep$con_1,
  dat_prep$con_2,
  dat_prep$con_comb,
  gam_fit$condition_1,
  gam_fit$condition_2,
  gam_fit$combined_condition,
  gam_fit$combined_no_condition
)

print(preds_and_plots$prediction_plot_con)
print(preds_and_plots$prediction_plot_comb)

# Comparing different statistics for the models

comp_metr <- comparison_metrics(gam_fit$combined_condition, gam_fit$combined_no_condition)
print(comp_metr)

# LRT test

LRT_test <- LRT_comparison(gam_fit$combined_condition, gam_fit$combined_no_condition)
print(LRT_test)

### Testing LRT for the whole array

LRT_test_array <- LRT_array(data_gz = data_fmri, cond_1_txt = condition_1, cond_2_txt = condition_2)

# Making a Statistical Parametric Map object for GAM

SPM_prep <- SPM_preparation(data_fmri, LRT_test_array)

SPM_map <- SPM_map(SPM_prep$`Z-vals`, x = x, y = y, z = z, prediction_plot_con = preds_and_plots$prediction_plot_con)
print(SPM_map)

SPM_map_bin <- SPM_map(SPM_prep$`Bin-vals`, x = x, y = y, z = z, prediction_plot_con = preds_and_plots$prediction_plot_con, not_show_legend = "none")
print(SPM_map_bin)
```

