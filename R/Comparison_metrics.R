#' Function comparing GAMs by different metrics
#'
#' @param gam_comb_cond Model which includes condition as a factor
#' @param gam_comb_no_cond Model which does not include condition as a factor
#'
#' @returns A data frame with different metrics by which one can compare two different models
#' @export
#'
comparison_metrics <- \(gam_comb_cond, gam_comb_no_cond) {
  df_all_metrics <- data.frame(
    AIC_cond = AIC(model_cond),
    AIC_no_cond = AIC(model_no_cond),
    BIC_cond = BIC(model_cond),
    BIC_no_cond = BIC(model_no_cond),
    R2_adj_cond = summary(model_cond)$r.sq,
    R2_adj_no_cond = summary(model_no_cond)$r.sq,
    Dev_Explained_cond = summary(model_cond)$dev.expl,
    Dev_Explained_no_cond = summary(model_no_cond)$dev.expl,
    RMSE_cond = sqrt(mean(residuals.gam(model_cond)^2)),
    RMSE_no_cond = sqrt(mean(residuals.gam(model_no_cond)^2))
  )
  rownames(df_all_metrics) <- NULL
  return(df_all_metrics)
}
