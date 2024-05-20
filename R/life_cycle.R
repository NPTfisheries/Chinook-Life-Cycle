#' Title Stochastic life-cycle model to predict returns.
#'
#' Author Ryan Kinzer
#' Created March 8, 2024

life_cycle <- function(

    N0 = 1000, # natural escapement at time = 0
    H0 = 1000, # hatchery escapement at time = 0
    viable = 1000, # MAT; in case of Lostine its 1000/2 b/c half the fish go into Lostine
    years = 10,
    
    harvest = FALSE,
    stochastic = FALSE,
    
    # natural fish metrics
    # stock recruit model object for spawners to smolt at LGD
    sr_model = NULL,
    sr_type = c('Beverton-Holt', 'Ricker'),
    # survival model object for smolt at LGD to brood year returns
    sar_model = NULL, #mixed effects model with origin and brood_year random effects
    # age model object
    age_model = NULL, # multinomial model with origin

    Npsp = 0, # prespawn mortality in nature
    Hpsp = 0, # hatchery pre-spawn mortality rate
    
    # hatchery program metrics    
    brood_take = 0, # total brood stock take
    sliding_scale = TRUE, # created inside the function (based off viable abundance and Lostine & Imnaha weir mgt. scales)
    pNOB = c('min', 'max'),
    max_pHOS = NULL,
    
    # hatchery brood stock spawner to smolt ratio at LGD survival
    BS_smolt_mu = NULL, #6.8,#1000, 
    BS_smolt_sd = NULL, #.44, #315,
    
    reproduction_age = 2 # reproduction occurs at 'reproduction_age' expressed as ocean age and greater

){
  
  source(here::here("R","predict_BH.R"))
  source(here::here("R","predict_Ricker.R"))
  source(here::here("R", "weir_scale.R"))
  source(here::here('R','calc_harvest.R'))
  
  # need positive integers, if fractional numbers are supplied integer will drop fractional portion
  N0 <- as.integer(N0)
  H0 <- as.integer(H0)
  viable <- as.integer(viable)
  bs <- as.integer(brood_take)
  years <- as.integer(years + 2)

  pNOB <- match.arg(pNOB)
  sr_type <- match.arg(sr_type)
  
  stopifnot(
    (N0 >= 0 && is.integer(N0)),
    (H0 >= 0 && is.integer(H0)),
    (bs >= 0 && is.integer(bs)),
    (viable > 0 && is.integer(viable)),
    (years > 0 && is.integer(years)),
    (is.numeric(Hpsp) && Hpsp <= 1 && Hpsp >= 0),
    (is.numeric(Npsp) && Npsp <= 1 && Npsp >= 0),
    (!is.null(BS_smolt_mu)),
    (!is.null(BS_smolt_sd)),
    (!is.null(sr_model)),
    (!is.null(sar_model)),
    (class(sar_model)[1] == 'glmerMod'),
    (!is.null(age_model)),
    (class(age_model)[1] == 'multinom')
  )
  
  # get fixed ages
  beta <- coefficients(age_model)
  nat_tmp <- exp(beta[,1])/(1+sum(exp(beta[,1])))
  Nage <- c(1-sum(nat_tmp), nat_tmp)
  hat_tmp <- exp(apply(beta,1,sum)) / (1 + sum(exp(apply(beta,1,sum))))
  Hage <- c(1 - sum(hat_tmp), hat_tmp)

  # create sliding scales
  
  # weir management
  
  weir_table <- weir_scale(viable)
  
  # critical <- viable * .3
  # 
  # steps <- ceiling(c(
  #   .05 * critical,
  #   .5 * critical,
  #   critical,
  #   .5 * viable,
  #   viable,
  #   1.5 * viable,
  #   2 * viable,
  #   Inf
  # ))
  # 
  # weir_scale <- as.data.frame(cbind(
  #   'steps' = steps,
  #   'max_pNOB' = c(0, .5, .4, .4, .3, .3, .25, .25),
  #   'min_pNOB' = c(0, 0, .2, .25, .3, .4, .5, 1.0),
  #   #greater than 2 * viable = .25
  #   'max_pHOS' = c(1.0, 1.0, .7, .6, .5, .4, .25, .1) # greater than 2 * viable = .1
  # ))
  
  # create object to store RY returns
  
  col_names <- c('nat_esc', 'adult_nat_esc', 'hat_esc', 'adult_hat_esc', 'adult_hat_removed', 'nat_by_rtn', 'hat_by_rtn', 'nob', 'hob', 'nob_spwn', 'hob_spwn', 'nos', 'hos', 'nat_smolt', 'hat_smolt', 'nat_harvest', 'hat_harvest')
  
  dat <- matrix(
    nrow = years,
    ncol = length(col_names),
    dimnames = list(1:years,
                    col_names)
  )
  
  # initialize escapement/age returns
  nat_esc_age <-
    matrix(rep(0L, (years + length(Nage))) * length(Nage),
           nrow = years + length(Nage),
           ncol = length(Nage))   #empty matrix for natural origin escapement split into age bins dimension (year+5 x 3)
  
  nat_esc_age[1:length(Nage), 1:length(Nage)] <- t(rmultinom(length(Nage), size = N0, prob = Nage))
  
  hat_esc_age <-
    matrix(rep(0L, (years + length(Hage))) * length(Hage),
           nrow = years + length(Hage),
           ncol = length(Hage))
  
  hat_esc_age[1:length(Hage), 1:length(Hage)] <- t(rmultinom(length(Hage), size = H0, prob = Hage))
  
  # Need to create RY returns.
  for(i in 1:years){

    # if(i == 14){
    #   browser()
    # }
    
    # tributary escapement with jacks
    nat_esc <- sum(nat_esc_age[i,])
    hat_esc <- sum(hat_esc_age[i,])
    
    # pre harvest adult only tributary escapement 
    pre_adult_nat_esc <- sum(nat_esc_age[i,reproduction_age:length(Nage)])
    pre_adult_hat_esc <- sum(hat_esc_age[i,reproduction_age:length(Hage)])

    # weir management proportions - based on preharvest adult only estimates

    if(sliding_scale){
      max_phos <- weir_table[min(which(weir_table$steps > pre_adult_nat_esc)),4]                  
    } else {
      if(max_pHOS == 1){
        max_phos <- max_pHOS - .001
      } else {
        max_phos <- max_pHOS
      }
    }
    
    if(pNOB == 'min'){
      pnob = weir_table[min(which(weir_table$steps > pre_adult_nat_esc)),3]
    } else if(pNOB == 'max'){
      pnob <- weir_table[min(which(weir_table$steps > pre_adult_nat_esc)),2]
    }
    
    tmp_nob <- floor(bs * pnob)

    # harvest limits
    if(harvest){
      harvest_ests <- calc_harvest(pre_adult_nat_esc, pre_adult_hat_esc, bs)
      nat_harvest <- harvest_ests[[1]]
      hat_harvest <- harvest_ests[[2]]
    } else {
      nat_harvest <- 0
      hat_harvest <- 0
    }
    
    # adults reaching the weir post harvest - assumes 100% exploitation
    adult_nat_esc <- pre_adult_nat_esc - nat_harvest
    adult_hat_esc <- pre_adult_hat_esc - hat_harvest
    
    
    if(tmp_nob <= adult_nat_esc){
      nob <- tmp_nob
    } else {
      nob <- adult_nat_esc
    }
    
    nob_spwn <- rbinom(1, nob, 1-Hpsp)  # actual natural-origin brood surviving to spawn in the hatchery
    
    tmp_hob = bs - nob       # if nob = 0, 100% of brood stock take is hatchery origin
    
    if(tmp_hob <= adult_hat_esc){
      hob <- tmp_hob
    } else {
      hob <- adult_hat_esc
    }
    
    hob_spwn <- rbinom(1, hob, 1-Hpsp) # actual hatchery-origin brood surviving to spawn in the hatchery
    
    BS <- nob_spwn + hob_spwn
    
    # Naturally spawning adults
    
    # 'tmp_' released above weir
    tmp_nos <- adult_nat_esc - nob # what returned minus what was removed
    
    if (tmp_nos < 0) {
      tmp_nos <- 0
    }
    
    # what actually spawned
    nos <- rbinom(1, tmp_nos, 1-Npsp) # actual spawning adults in the wild
    
    tmp_hos <-adult_hat_esc - hob
    
    if (tmp_hos < 0) {
      tmp_hos <- 0
    }
    
    tmp_phos <- tmp_hos/(tmp_hos + tmp_nos)

    if(tmp_phos < max_phos|is.nan(tmp_phos)){
      tmp_hos <- tmp_hos
    } else {
      # solve for tmp_hos based on max_pHOS
      tmp_hos <- floor(-(max_phos * tmp_nos)/(max_phos - 1))
    }
    
    if (tmp_hos <= 0) {
      tmp_hos <- 0
    }
    
    adult_hat_removed <- adult_hat_esc - hob - tmp_hos
        
    hos <- rbinom(1, tmp_hos, (1-Npsp))
    
    spawners <- (nos + hos)      # Total fish spawning in the stream
    
    if(spawners <= 0){
      spawners <- 0
    }       
    
    # juvenile productivity: spawners to smolt at LGD
    if (!stochastic) {
      params <- coef(sr_model)
      BSsmolt <- exp(BS_smolt_mu)
    } else {
      params <- coef(sr_model) + MASS::mvrnorm(1, mu = numeric(length(coef(sr_model))), Sigma = vcov(sr_model))
      BSsmolt <- rlnorm(1, BS_smolt_mu, BS_smolt_sd) #need distribution for hat smolt survival; rlnorm(1, tau_mulog, tau_sdlog)
    }
    
    if (sr_type == "Ricker") {
      nat_smolt <- as.numeric(predict_Ricker(stock = spawners, alpha = params[1], beta = params[2]))
    } else if (sr_type == "Beverton-Holt")  {
      nat_smolt <- as.numeric(predict_BH(stock = spawners, alpha = params[1], beta = params[2]))
    } 
    
    nat_smolt <- floor(nat_smolt)
    hat_smolt <- floor(BS * BSsmolt)
    
    # smolts to total tributary escapement including jacks
    if(!stochastic){
      beta <- lme4::fixef(sar_model)
      yr_ef <- c(0,0)
    } else {
      beta <- lme4::fixef(sar_model) + MASS::mvrnorm(1, mu = numeric(length(lme4::fixef(sar_model))), Sigma = vcov(sar_model))
      yr_ef <- c(rnorm(1, 0, lme4::VarCorr(sar_model)$brood_year[1,1]),0)
    }
    
    sars <- plogis(matrix(c(1,1,0,1), nrow = 2, dimnames = list(c('nat', 'hat'))) %*% (beta + yr_ef))
    nat_sar <- sars[1,1]
    hat_sar <- sars[2,1]
    
    nat_by_rtn <- floor(nat_smolt * nat_sar)
    hat_by_rtn <- floor(hat_smolt * hat_sar)
    
    if (!stochastic){
      tmp <- floor(Nage * nat_by_rtn)
      tmp_diff <- nat_by_rtn - sum(tmp)
      nat_by_rtn_age <- tmp + rmultinom(1, size = tmp_diff, prob = Nage) 
      
      tmp <- floor(Hage * hat_by_rtn)
      tmp_diff <- hat_by_rtn - sum(tmp)
      hat_by_rtn_age <- tmp + rmultinom(1, size = tmp_diff, prob = Hage) 
    } else {
      mu <- as.vector(t(coef(age_model)))
      sigma <- vcov(age_mod)
      
      beta <- matrix(MASS::mvrnorm(1, mu = mu, Sigma = sigma), ncol = 2, byrow = TRUE)
      nat_tmp <- exp(beta[,1])/(1+sum(exp(beta[,1])))
      nat_tmp <- c(1-sum(nat_tmp), nat_tmp)
      nat_by_rtn_age <- rmultinom(1, nat_by_rtn, nat_tmp)

      hat_tmp <- exp(apply(beta,1,sum)) / (1 + sum(exp(apply(beta,1,sum))))
      hat_tmp <- c(1 - sum(hat_tmp), hat_tmp)
      hat_by_rtn_age <- rmultinom(1,hat_by_rtn, hat_tmp)
    }
    

    for(j in 1:length(Nage)){
      nat_esc_age[i+j,j] <- nat_by_rtn_age[j,1]
    }
    
    for(j in 1:length(Hage)){
      hat_esc_age[i+j,j] <- hat_by_rtn_age[j,1]
    }
    
    dat_by <- c(nat_esc, adult_nat_esc, hat_esc, adult_hat_esc, adult_hat_removed, nat_by_rtn, hat_by_rtn, nob, hob, nob_spwn, hob_spwn, nos, hos, nat_smolt, hat_smolt, nat_harvest, hat_harvest)
    dat[i,] <- dat_by
    
  }
  
  return(as.data.frame(dat))
  #return(as.data.frame(dat[-(1:2),]))
  
}


# library(tidyverse)
# #source(here::here("R","predict_BH.R"))
# #source(here::here("R","predict_Ricker.R"))
# 
# sr_mod <- readRDS(file = './data/lostine_mods/lostine_smolt_mod.rds')
# sar_mod <- readRDS(file = './data/lostine_mods/lostine_sar_mod.rds')
# age_mod <- readRDS(file = './data/lostine_mods/lostine_age_mod.rds')
# 
# 
# N0 <- 1000
# H0 <- 1000
# 
# dat <- life_cycle(
#   
#   N0 = N0, # natural escapement at time = 0
#   H0 = H0, # hatchery escapement at time = 0
#   viable = 1000, # MAT; in case of Lostine its 1000/2 b/c half the fish go into Lostine
#   years = 10,
#   harvest = TRUE,
#   stochastic = TRUE,
# 
#   sr_model = sr_mod,
#   sr_type = 'Beverton-Holt',
#   sar_model = sar_mod,
#   age_model = age_mod,
#   
#   Npsp = 0, # prespawn mortality in nature
#   Hpsp = 0, # hatchery pre-spawn mortality rate
#   
#   # hatchery program metrics    
#   brood_take = 100, # total brood stock take
#   sliding_scale = TRUE,
#   pNOB = 'min',
#   max_pHOS = 1,
#   BS_smolt_mu = 6.8, 
#   BS_smolt_sd = .44,
#   reproduction_age = 2
#   )
# 
# 
# # check returns
# plot(1:dim(dat)[1], dat$nat_esc + dat$hat_esc, col = 'black', type = "l", lwd = 2, ylim = c(0, N0 + H0))
# lines(1:dim(dat)[1], dat$nat_esc, col = 'blue', lty = 2, lwd = 2)
# lines(1:dim(dat)[1], dat$hat_esc, col = 'green')
# 
# # check stock-recruit fit
# params <- coef(sr_mod)
# k <- params[1]/params[2]
# 
# dat$spawners <- dat$nos + dat$hos
# 
# plot(dat$spawners, dat$nat_smolt, ylim = c(0, k), xlim = c(0,2000))
# lines(0:max(dat$spawners), predict_BH(stock = 0:max(dat$spawners), alpha = params[1], beta = params[2]), lwd = 2)
# lines(0:max(dat$spawners), predict_Ricker(stock = 0:max(dat$spawners), alpha = params[1], capacity = k), lty = 2, lwd = 2)
# abline(h = k, lwd = 2, lty = 3)

