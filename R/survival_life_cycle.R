#' Title Stochastic life-cycle model to predict returns.
#'
#' Author Ryan Kinzer
#' Created March 8, 2024
#' @return
#' @export
#'
#' @examples
#'
life_cycle <- function(spawners, sex_p, fecundity,
                       parr_s, smolt_s, hydro_s, ocean_s, adult_s, spawn_s){

  f = rbinom(1, spawners, sex_p) # female spawners
  eggs <- round(sum(rnorm(f, fecundity, sd = 250))) # deposited eggs
  parr <- rbinom(1, eggs, parr_s) # parr leaving the tributary
  smolt_lgr <- rbinom(1, parr, smolt_s) # smolt at LGR
  smolt_bon <- rbinom(1, smolt_lgr, hydro_s) # smolt at bon
  adults_bon <- rbinom(1, smolt_bon, ocean_s)
  adults_lgr <- rbinom(1, adults_bon, adult_s)
  adults_trib <- rbinom(1, adults_lgr, spawn_s)
  

  results <- list('spawners_f' = f,
                  'eggs' = eggs,
                  'parr' = parr,
                  'smolt_lgr' = smolt_lgr,
                  'smolt_bon' = smolt_bon,
                  'adults_bon' = adults_bon,
                  'adults_lgr' = adults_lgr,
                  'adults_trib' = adults_trib)
  
  return(results)
}