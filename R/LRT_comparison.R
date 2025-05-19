#' Function producing p-values that help with models assessment
#'
#' @param gam_comb_cond Model which includes condition as a factor
#' @param gam_comb_no_cond Model which does not include condition as a factor
#'
#' @returns A p-value corresponding to the Likelihood Ratio Test
#' @export
LRT_comparison <- \(gam_comb_cond, gam_comb_no_cond) {

  invisible(capture.output(result_LRT <- compareML(gam_comb_no_cond, gam_comb_cond)))
  LRT_result <- result_LRT$table[2,6] %>% as.numeric()
  if (is.na(LRT_result)) {
    LRT_result = 1
  }
  return(LRT_result)
}
