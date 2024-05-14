# Fit a Beverton-Holt stock recruit curve.
# Ryan N. Kinzer
# Created: 9 April 2024


fit_BH <- function(stock, recruits, inits = NULL){
  mod <- nls(recruits ~ (alpha*stock)/(1+(beta*stock)),
             start = list(alpha = inits[1], beta = inits[2]))
  return(mod)
}