#' @title est_fixedeffect
#'
#' @description Calculates estimate (fixed effects) of the c-statistic for multiple pooled prediction model studies
#'
#' @param vec_logitc Array of logit transformed c-statistics (AUC) of predictive model studies. Calculated in order using the logit function.
#' @param vec_SElogitc Array of standard errors associated with the logit transformed c-statistics (AUC) of each predictive model study. Calculated using the est_SElogitc function.
#'
#' @return Pooled estimate of the c-statistic (fixed effects), value from 0 to 1.
#' @export

est_fixedeffect <- function(vec_logitc, vec_SElogitc){

  weights <- 1/(vec_SElogitc^2)
  logit_pooled_effect_fixed <- sum(weights*vec_logitc)/sum(weights)

  pooled_effect_fixed <- invlogit(logit_pooled_effect_fixed)

  return(pooled_effect_fixed)
}
