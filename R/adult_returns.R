source(here::here("R","predict_BH.R"))
source(here::here("R","predict_Ricker.R"))

adult_returns <- function(
    N0 = 1000,
    RScurve = c('Beverton-Holt', 'Ricker'),
    k = 1000,
    mu_lambda = 1.5,
    sd_lambda = 2.5,
    mu_tau = 4,
    sd_tau = 1.5,
    brood = 0,
    pNOB = 0, #NOB take number
    Hpsp = 0, # hatchery pre-spawn mortality rate
    Npsp = 0,
    omega1 = 10, # less than number for 0 NOB take
    omega2 = 50, # less than number for half the NOB take goal, above omega2 then meet NOB take goal
    Nage = c(0, 0, .2,.6,.2), # vector of age proportions for brood year returns starting with age-1 (sp/sm Chinook are typically c(0, 0, .3, .6, .1))
    Sage = c(0, 0, .3,.6,.1),
    reproduction_age = 4, # reproduction occurs at 'reproduction_age' and above
    years = 10,
    #iterations,
    Harvest = FALSE,
    MAT = 1000,
    Prodfixed = TRUE,
    Agefixed = TRUE
){
  
  # need positive integers, if fractional numbers are supplied integer will drop fractional portion
  N0 <- as.integer(N0) 
  k <- as.integer(k)
  brood <- as.integer(brood)
  omega1 <- as.integer(omega1)
  omega2 <- as.integer(omega2)
  years <- as.integer(years)
  MAT <- as.integer(MAT)
  
  mulog_lam = log(mu_lambda) # creates log-normal location and scale parameters
  sdlog_lam = log(sd_lambda)
  mulog_tau = log(mu_tau)
  sdlog_tau = log(sd_tau)
  
  RScurve <- match.arg(RScurve)

  stopifnot(
    (N0 > 0 && is.integer(N0)),
    (k > 0 && is.integer(k)),
    (brood >= 0 && is.integer(brood)),
    (omega1 > 0 && is.integer(omega1)),
    (omega2 > 0 && is.integer(omega2)),
    (MAT > 0 && is.integer(MAT)),
    (years > 0 && is.integer(years)),
    sum(Nage) == 1,
    sum(Sage) == 1,
    length(Nage) == length(Sage),
    (is.numeric(pNOB) && pNOB <= 1 && pNOB >= 0),
    (is.numeric(Hpsp) && Hpsp <= 1 && Hpsp >= 0),
    (is.numeric(Npsp) && Npsp <= 1 && Npsp >= 0)
  )
  
  #MAT <- 750
  #Total_esc <- 254
  pMAT_upper <- c(.3,.5,.75,1.08)
  NPT_harvest <- c(.01,.02,.03,.044,.175)
  harvest_thres <- MAT*pMAT_upper

  # RY returns
  
  col_names <- c('nat_esc', 'hat_esc', 'nat_by_rtn', 'hat_by_rtn', 'nob', 'hob', 'nob_spwn', 'hob_spwn', 'nos', 'hos', 'harvest', 'p', 'tau')
  
  dat <- matrix(
    nrow = years,
    ncol = length(col_names),
    dimnames = list(1:years,
                    col_names)
  )
  
  # initialize escapement/age returns
  nat_esc_age<-matrix(rep(0L, (years+length(Nage)))*length(Nage), nrow=years+length(Nage),ncol=length(Nage))   #empty matrix for natural origin escapement split into age bins dimension (year+5 x 3)
  tmp <- rmultinom(length(Nage), size = N0, prob = Nage)
  nat_esc_age[1:length(Nage),1:length(Nage)] <- t(tmp)
  
  hat_esc_age<-matrix(rep(0L, (years+length(Sage)))*length(Sage), nrow=years+length(Sage),ncol=length(Sage))

  # Need to create RY returns.
  for(i in 1:years){

    nat_esc <- sum(nat_esc_age[i,])
    hat_esc <- sum(hat_esc_age[i,])
    #Tot_esc[t,i]<- Nat_esc[t,i] + Sup_esc[t]
    #hfrac[t,i] = Sup_esc[t]/(Nat_esc[t,i] + Sup_esc[t])       
    #NoSup_esc[t,i]<-NoSup_esc_age[t,1]+NoSup_esc_age[t,2]+NoSup_esc_age[t,3]
    #Jack_esc[t,i] <- Nat_esc_age[t,1] + Sup_esc_age[t,1]
    
    # Hatchery spawning adults
    
    # the following commented out section is only if you want to model varying pNOB operations over time.....
    
    # if(t>=25){pnob = pNOB[2]}else# if you wanted to turn brood stock collection off after so many years 
    # {pnob = pNOB[1]}       
    # 
    # if(t>=25){bs = brood[2]}else# if you wanted to turn brood stock collection off after so many years 
    # {bs = brood[1]}
    
    pnob <- pNOB
    bs <- brood
    
    if(nat_esc < omega1){       # Sliding scale for brood stock collection, would change for other populations
      tmp_nob <-0
      } else if(nat_esc < omega2){
        tmp_nob<-round((bs*pnob)/2)
      } else {
          tmp_nob <- round(bs*pnob)
          }
    
    if(tmp_nob <= sum(nat_esc_age[i,reproduction_age:length(Nage)])){
      nob <- tmp_nob
    } else {
        nob <- sum(nat_esc_age[i,reproduction_age:length(Nage)])
    }
    
    nob_spwn <- rbinom(1, nob, 1-Hpsp)  # actual natural-origin brood surviving to spawn
    
    tmp_hob = bs - nob       # if nob = 0, 100% of brood stock take is hatchery origin
    
    if(tmp_hob <= sum(hat_esc_age[i,reproduction_age:length(Sage)])){
      hob <- tmp_hob
    } else {
        hob <- sum(hat_esc_age[i,reproduction_age:length(Sage)])
    }
    
    hob_spwn <- rbinom(1, hob, 1-Hpsp) # actual hatchery-origin brood surviving to spawn
    
    BS <- nob_spwn + hob_spwn
    
    # Naturally spawning adults
    
    tmp_nos <-sum(nat_esc_age[i,reproduction_age:length(Nage)]) - nob # what returned minus what was removed

    if (tmp_nos <= 0) {
      tmp_nos <- 0
    }
    
    nos <- rbinom(1, tmp_nos, 1-Npsp)
    
    tmp_hos <-sum(hat_esc_age[i,reproduction_age:length(Sage)]) - hob
    
    if (tmp_hos <= 0) {
      tmp_hos <- 0
    }
    
    hos <- rbinom(1, tmp_hos, (1-Npsp))

    
    # phos <= (sum(Sup_esc_age[t,reproduction_age:length(Sage)]) - hob) / ((sum(Sup_esc_age[t,reproduction_age:length(Sage)]) - hob) + (sum(Nat_esc_age[t,reproduction_age:length(Nage)]) - nob))
    # pnob = nat_bs_spwn/BS
    # pni = pnob / (phos + pnob)
    
    #Harvest - adult only and comes out after brood stock is collected
    if (Harvest) {
      if ((sum(nat_esc_age[i,reproduction_age:length(Nage)]) + sum(hat_esc_age[i,reproduction_age:length(Sage)])) <= harvest_thres[1]) {
        harvest <- 0
      } else {
        id <- max(which(harvest_thres < (sum(nat_esc_age[i,reproduction_age:length(Nage)]) + sum(hat_esc_age[i,reproduction_age:length(Sage)]))))
        harvest <- floor((harvest_thres[id] * NPT_harvest[id]) + ((sum(nat_esc_age[i,reproduction_age:length(Nage)]) + sum(hat_esc_age[i,reproduction_age:length(Sage)])) - harvest_thres[id]) * NPT_harvest[id + 1]) # harvest large portion of fish at lower rate, harvest excess at higher rate
        }
      } else {
        harvest <- 0
      }
    
    spawners <- (nos + hos) - harvest      # Total fish spawning in the stream
    
    if(spawners <= 0){
      spawners <- 0
      }       
    
    if (Prodfixed) {
      p <- mu_lambda  # fixes progeny returns mu_p
      tau <- mu_tau
    } else {
      p <- rlnorm(1, mulog_lam, sdlog_lam) # natural productivity subject to random variation 
      tau <- rlnorm(1, mulog_tau, sdlog_tau) # RRS subject to random variation
    }
    
    if (RScurve == "Ricker") {
      nat_by_rtn <- predict_Ricker(stock = spawners, alpha = p, capacity = k)
      #nat_by_rtn <- p * spawners * exp(-p * (spawners/(k*exp(1)))) #Ricker Prediciton for Nat progeny return * productivity R.V. following estimated gamma dist.
    } else if (RScurve == "Beverton-Holt")  {
      nat_by_rtn <- predict_BH(stock = spawners, alpha = p, capacity = k)
      #nat_by_rtn <- (p * spawners) / (1 + (p * (spawners/ k))) # Beverton Holt
    } 

    if(BS<=0){            #if brood stock collection was zero then no fish could return
      hat_by_rtn <- 0
      } else {
        hat_by_rtn <- (BS * p * tau)}  #progeny returns from brood stock are subjected to same environmental noise as of natural fish returns.
    
    if (Agefixed){
      tmp <- floor(matrix(nat_by_rtn * Nage, nrow=length(Nage)))
      tmp_diff <- nat_by_rtn - sum(tmp)
      nat_by_rtn_age = tmp + rmultinom(1, size = tmp_diff, prob = Nage)
    } else {
        nat_by_rtn_age <- rmultinom(1, size = nat_by_rtn, prob = Nage) # the age of Nat progeny returns follows a multinomial
    }
    
    if (Agefixed){
      tmp <- floor(matrix(hat_by_rtn * Sage, nrow=length(Sage)))
      tmp_diff <- hat_by_rtn - sum(tmp)
      hat_by_rtn_age = tmp + rmultinom(1, size = tmp_diff, prob = Sage)
      } else {
        hat_by_rtn_age <- rmultinom(1, size = hat_by_rtn, prob = Sage)  #the age of Sup progeny returns follows a multinomial
        }

    for(j in 1:length(Nage)){
      nat_esc_age[i+j,j] <- nat_by_rtn_age[j,1]
    }
    
    for(j in 1:length(Sage)){
      hat_esc_age[i+j,j] <- hat_by_rtn_age[j,1]
    }
    
    dat_by <- c(nat_esc, hat_esc, nat_by_rtn, hat_by_rtn, nob, hob, nob_spwn, hob_spwn, nos, hos, harvest, p, tau)
    dat[i,] <- dat_by
    
  }
  
  return(as.data.frame(dat))
  
}


k = 1000
a = 1.5

dat <- adult_returns(
  N0 = 1000,
  RScurve = 'Ricker',
  k = k,
  mu_lambda = a,
  sd_lambda = 2.5,
  mu_tau = 4,
  sd_tau = 1.5,
  brood = 0,
  pNOB = 0,
  #NOB take number
  Hpsp = 0,
  Npsp = 0,
  # hatchery pre-spawn mortality rate
  omega1 = 10,
  # less than number for 0 NOB take
  omega2 = 50,
  # less than number for half the NOB take goal, above omega2 then meet NOB take goal
  Nage = c(0, 0, .2, .6, .2),
  # vector of age proportions for brood year returns starting with age-1 (sp/sm Chinook are typically c(0, 0, .3, .6, .1))
  Sage = c(0, 0, .3, .6, .1),
  reproduction_age = 3,
  # reproduction occurs at 'reproduction_age' and above
  years = 100,
  #iterations,
  Harvest = FALSE,
  MAT = 1000,
  Prodfixed = TRUE,
  Agefixed = TRUE
)

# check returns
plot(1:dim(dat)[1], dat$nat_esc + dat$hat_esc, col = 'black', type = "l", lwd = 2, ylim = c(0,k))
lines(1:dim(dat)[1], dat$nat_esc, col = 'blue', lty = 2, lwd = 2)
lines(1:dim(dat)[1], dat$hat_esc, col = 'green')
abline(h = k, lwd = 2, lty = 2)

# check stock-recruit fit
plot(dat$nos, dat$nat_by_rtn)
lines(dat$nos, predict_BH(stock = dat$nos, a, capacity = k), lwd = 2)
lines(dat$nos, predict_Ricker(stock = dat$nos, a, capacity = k), lty = 2, lwd = 2)


# Play with parameters

S <- 0:10000
a = 1.5 # primary prod, slope when stock = 0, raising value means we reach capacity quicker or with fewer stock
#b = .0025
#k = a/b
k = 1000 # adult capacity

b_bh <- a/k
b_r <- a/(k*exp(1))

source(here::here("R","equilibrium_BH.R"))
pnts <- equilibrium_BH(a, b_bh)

# stock-recruit functions
plot(S, predict_BH(S, a, beta = b_bh), type = 'l', lwd = 2, col = 'black', ylim = c(0, k),
     main = 'Beverton-Holt and Ricker Stock-recruit Functions')
lines(S, predict_Ricker(S, a, beta = b_r), lwd = 2, col = 'black')
segments(1/b_r, 0, 1/b_r, k, lwd = 2, lty = 2, col = 'black')
abline(h = k, col = 'black', lwd = 2, lty = 2)
abline(a = 0, b = 1, col = 'red')
points(x = pts[2], y = pts[2], pch = 19, cex = 2)
# check capacity parameterization
#lines(S, predict_BH(S, a, capacity = k), lty = 2, lwd = 2, col = 'white')
#lines(S, predict_Ricker(S, a, capacity = k), lty = 2, lwd = 2, col = 'white')


# Plot stock-recruit curves----
# Beverton-Holt
#a = p = slope of the model at S = 0
# a/b = Rp = k = peak recruitment
b_bh <- a/k

bev_mod <- nls(nat_by_rtn ~ (a*nos)/(1 + (b *nos)), start=list(a=a, b=k),
               data = dat,
               control = nls.control(maxiter = 1000))

b = k/1.5

bev_mod <- nls(nat_by_rtn ~ ((a*nos)/(1 + (b*nos))), start = list(a = 1.5, b = b),
               data = dat)
coef(bev_mod)

plot(dat$nos, dat$nat_by_rtn)

# Ricker
ricker_mod<-nls(nat_by_rtn ~ p * nos * exp(-p * (nos/(k*exp(1)))), start=list(p=1.3, k=500),
                data = dat)
summary(ricker_mod)
coef(ricker_mod)

ricker_fit <- predict(ricker_mod, list(nos = dat$nos))
plot(dat$nos, ricker_fit)

plot(dat$nos, dat$nat_by_rtn, main = paste0('P = ', round(coef(ricker_mod)[1],2),
                                            '; k = ', round(coef(ricker_mod)[2],2)))
lines(dat$nos, ricker_fit, col = 'red')


ricker_fit <- predict(ricker_mod, list(nos = 0:20000))
plot(0:20000, ricker_fit)