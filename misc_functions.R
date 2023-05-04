package_loader <- function(package_list) {
  for (p in package_list) {
    if (
      !(p %in% installed.packages())
    ) {
      install.packages(p)
    }
    lapply(p, library, character.only = T)
  }
}


model_summary <- function(model) {
  summ <- summary(model)
  odds_ratio <- exp(fixef(model))
  mod_se <- sqrt(diag(vcov(model)))
  
  output <- cbind(
    summ$coefficients,
    odds_ratio,
    OR_ci_ll = fixef(model) - (1.96 * mod_se),
    OR_ci_ul = fixef(model) + (1.96 * mod_se)
  )
  
  return(output)
}
