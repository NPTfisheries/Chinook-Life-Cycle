#' Title Weir management sliding scale.
#' Author Ryan Kinzer
#' Created May 15, 2024


weir_scale <- function(viable){
  # weir management
  critical <- viable * .3
  
  steps <- ceiling(c(
    .05 * critical,
    .5 * critical,
    critical,
    .5 * viable,
    viable,
    1.5 * viable,
    2 * viable,
    Inf
  ))
  
  weir_scale <- as.data.frame(cbind(
    'steps' = steps,
    'max_pNOB' = c(0, .5, .4, .4, .3, .3, .25, .25),
    'min_pNOB' = c(0, 0, .2, .25, .3, .4, .5, 1.0),
    #greater than 2 * viable = .25
    'max_pHOS' = c(1.0, 1.0, .7, .6, .5, .4, .25, .1) # greater than 2 * viable = .1
  ))
}

