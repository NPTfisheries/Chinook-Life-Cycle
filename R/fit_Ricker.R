# Fit a Ricker stock recruit curve.
# Ryan N. Kinzer
# Created: 9 April 2024

fit_Ricker <- function(stock, recruits, inits = NULL){
  mod <- nls(recruits ~ (alpha*stock*exp(-beta*stock)),
             start = list(alpha = inits[1], beta = inits[2]))
  return(mod)
}