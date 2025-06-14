% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SPM_preparation.R
\name{SPM_preparation}
\alias{SPM_preparation}
\title{Preparation for SPM map}
\usage{
SPM_preparation(
  data_gz,
  lrt_array,
  dfs_model_comp = 5,
  mult_test_method = "hochberg"
)
}
\arguments{
\item{data_gz}{Main data file (.gz extension) with BOLD signal values.}

\item{lrt_array}{The array of p-values of LRT evaluated on model with condition included vs the one without.}

\item{dfs_model_comp}{The difference of degrees of freedom between models.}

\item{mult_test_method}{The method of p-values adjustment.}
}
\value{
Two data overwritten objects. First with p-values and the other with z-values.
}
\description{
Preparation for SPM map
}
