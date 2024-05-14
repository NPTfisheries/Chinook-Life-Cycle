#------------------------------------------------------------------------------
# Supplementation Simulation Function
# Set population parameters based on observed means and levels of variation
# to help understand a population response.  Population response can be
# evaluated with different management scenarios, i.e., broodstock take, pNOB,
# harvest......
# Ryan N. Kinzer
# Created: ?-?-2013?   Modified: 5-2-2016
#------------------------------------------------------------------------------
# Initial Population Parameters
#------------------------------------------------------------------------------
# Initital State - 
# N0 - population size at time zero (start of supp. program)
# RScurve - Ricker or Beverton
# k - capacity of system ; logmean and logsd
#------------------------------------------------------------------------------
# Population Growth Potential - Productivity - logmean & logsd
#------------------------------------------------------------------------------
# mu_lambda - mean productiviy (PP - includes age 3, 4, 5)
# sd_lambda - standard deviation of productivity
# mu_tau - tau of supplementation fish, benefit of hatchery rearing (RRS)
# sd_tau - sd of RRS
# rho - correlation between Npp and RSS
#------------------------------------------------------------------------------
# Broodstock Collection and Weir Management
#------------------------------------------------------------------------------
# brood - broodstock removed as proportion of total capacity k
# pNOB - proportion of natural origin brood stock
# Hpsp <- pre-spawn mortality of broodstock removed prior to spawning
# omega1 - broodstock collection sliding scale - zero fish removed
# omega2 - half of targeted natural origin brood is nat. origin, 
#            remainder is filled with Sup.
#------------------------------------------------------------------------------
# Life History Characteristics
#------------------------------------------------------------------------------
# Nage - vector containing natural origin age probabilities
# Sage - vector containing supplementation origin age probabilities
#------------------------------------------------------------------------------
# Harvest - True/False, harvest is assumed equal for all fish (origin, age)
# MAT - minimum abundance threshold, similar to viable threshold but includes
#          hatchery and natural origin fish
#------------------------------------------------------------------------------
# Simulation Values - Stochasticity and Length
#------------------------------------------------------------------------------
# Prodfixed - True/False, if true, uses either Ricker or Beverton-Holt curve
#              with no variability, if false, curve multiplied by gamma r.v.
# Agefixed - True/False, if true, uses age vectors, if false uses 
#             multinomial with N = brood year return and p = age vectors 
# years - length of each time series
# iterations - number of times series simulated
#------------------------------------------------------------------------------
# Equations used:
# pni = pnob/(pnob + phos)
# add a harvest type control
# use a high, medium, low capacity system
# use high, medium, low productivities - MLE of gamma using all reference pops.
# Ricker - 1/b = number of spawners that gives maximum recruits
# Intersection with 1:1 line - S = (log(1/a))/-b
#------------------------------------------------------------------------------
# Function to make SupplementationEffectSim run. 
#------------------------------------------------------------------------------
sim_adult_returns<-function(N0,RScurve,k,mu_lambda,sd_lambda,mu_tau,sd_tau,
                            brood,pNOB,Hpsp,omega1,omega2,Harvest,MAT,Nage,
                            Sage,years,iterations,Prodfixed,Agefixed){
  #MAT <- 750
  #Total_esc <- 254
  pMAT_upper <- c(.3,.5,.75,1.08)
  NPT_harvest <- c(.01,.02,.03,.044,.175)
  harvest_thres <- MAT*pMAT_upper
  
  iloop<-iterations     # number of time series to produce
  tloop<-years          # number of years to forecast
  
  Nat_esc<-matrix(rep(0,iloop*tloop),nrow=tloop,ncol=iloop)  #empty matrix with dimensions (year x series) for natural origin fish that returned during run year
  Tot_esc<-matrix(rep(0,iloop*tloop),nrow=tloop,ncol=iloop)  #empty matrix with dimensions (year x series) for total fish that returned during run year
  Jack_esc <-matrix(rep(0,iloop*tloop),nrow=tloop,ncol=iloop)
  Spawn<-matrix(rep(0,iloop*tloop),nrow=tloop,ncol=iloop)    #Natural origin adults (4,5) and Supp. origin adults (4,5) that spawn in stream
  NoSup_esc <- matrix(rep(0,iloop*tloop),nrow=tloop,ncol=iloop) #empty matrix to store outcomes if no sup occurred    
  
  harvest<-matrix(rep(0,iloop*tloop),nrow=tloop,ncol=iloop) # empty matrix to record harvest numbers
  hfrac<-matrix(rep(0,iloop*tloop),nrow=tloop,ncol=iloop) # empty matrix to record hatchery fraction on spawning grounds before harvest
  pHOS<-matrix(rep(0,iloop*tloop),nrow=tloop,ncol=iloop)  # prior harvest
  PNI<-matrix(rep(0,iloop*tloop),nrow=tloop,ncol=iloop)  # 
  A_pNOB<-matrix(rep(0,iloop*tloop),nrow=tloop,ncol=iloop)  # 
  nob<-matrix(rep(0,iloop*tloop),nrow=tloop,ncol=iloop)  # 
  hob<-matrix(rep(0,iloop*tloop),nrow=tloop,ncol=iloop)  # 
  
  p <- matrix(rep(0,iloop*tloop),nrow=tloop,ncol=iloop) #empty matrix to monitor productivity values
  tau <- matrix(rep(0,iloop&tloop),nrow=tloop,ncol=iloop) # empty matrix to monitor RRS
  
  mulog_lam = log(mu_lambda) # creates log-normal location and scale parameters
  sdlog_lam = log(sd_lambda)
  mulog_tau = log(mu_tau)
  sdlog_tau = log(sd_tau)
  
  for(i in 1:iloop){
    Nat_esc_age<-matrix(rep(0,((tloop*3)+(3*5))),nrow=tloop+5,ncol=3)   #empty matrix for natural origin escapement split into age bins dimension (year+5 x 3)
    
    tmp<-rmultinom(5, size = N0, prob = Nage)
    # Brood year returns
    Nat_esc_age[1:3,1]<-tmp[1,1:3]      # initializes the age 3 natural ecapement column
    Nat_esc_age[1:4,2]<-tmp[2,1:4]      # initializes the age 4 natural ecapement column
    Nat_esc_age[1:5,3]<-tmp[3,1:5]      # initializes the age 5 natural ecapement column
    
    Sup_esc_age<-matrix(rep(0,((tloop*3)+(3*5))),nrow=tloop+5,ncol=3)   #empty matrix for supplementation origin escapement split into age bins dimension (year+5 x 3)
    Sup_esc<-rep(0,tloop)   #empty vector to record Supp. escapement
    
    NoSup_esc_age<-matrix(rep(0,((tloop*3)+(3*5))),nrow=tloop+5,ncol=3)
    NoSup_esc_age[1:3,1]<-tmp[1,1:3]    # initializes the age 3 natural ecapement column
    NoSup_esc_age[1:4,2]<-tmp[2,1:4]    # initializes the age 4 natural ecapement column
    NoSup_esc_age[1:5,3]<-tmp[3,1:5]    # initializes the age 5 natural ecapement column
    
    for(t in 1:tloop){
      # Escapement
      Nat_esc[t,i]<-Nat_esc_age[t,1]+Nat_esc_age[t,2]+Nat_esc_age[t,3]    # Total Nat escapement = Esc Nat age 3 + Esc Nat age 4 + Esc Nat age 5
      Sup_esc[t]<-Sup_esc_age[t,1]+Sup_esc_age[t,2]+Sup_esc_age[t,3]     # Total Sup escapement = Esc Sup age 3 + Esc Sup age 4 + Esc Sup age 5
      Tot_esc[t,i]<- Nat_esc[t,i] + Sup_esc[t]
      hfrac[t,i] = Sup_esc[t]/(Nat_esc[t,i] + Sup_esc[t])       
      NoSup_esc[t,i]<-NoSup_esc_age[t,1]+NoSup_esc_age[t,2]+NoSup_esc_age[t,3]
      
      Jack_esc[t,i] <- Nat_esc_age[t,1] + Sup_esc_age[t,1]
      
      # Broodstock
      if(t>=25){pnob = pNOB[2]}else# if you wanted to turn brood stock collection off after so many years 
      {pnob = pNOB[1]}       
      
      if(t>=25){bs = brood[2]}else# if you wanted to turn brood stock collection off after so many years 
      {bs = brood[1]}
      
      if(Nat_esc[t,i] < omega1){       # Sliding scale for brood stock collection, would change for other populations
        tmp_nob<-0} else
          if(Nat_esc[t,i] < omega2){
            tmp_nob<-round((bs*pnob)/2)} else
            {tmp_nob<-round(bs*pnob)}
      
      if(tmp_nob <= (Nat_esc_age[t,2] + Nat_esc_age[t,3])){nob[t,i] <- tmp_nob } else
      {nob[t,i] <- Nat_esc_age[t,2] + Nat_esc_age[t,3]}
      
      tmp_hob = bs - nob[t,i]       # if nob = 0, 100% of brood stock take is hatchery origin
      
      if(tmp_hob <= (Sup_esc_age[t,2] + Sup_esc_age[t,3])) {hob[t,i] = tmp_hob} else
      {hob[t,i] = Sup_esc_age[t,2] + Sup_esc_age[t,3]}
      
      BS = (nob[t,i] + hob[t,i])*(1-Hpsp)           # Total brood stock spawned
      
      
      # Spawners  
      N_spawn<-(Nat_esc_age[t,2]+Nat_esc_age[t,3])-nob[t,i]   # Natural spawners = (Esc Nat age 4 + Esc Nat age 5) - (Brood stock collection)
      if(N_spawn <= 0){     # makes sure Natural spawners >= 0
        N_spawn <- 0}
      
      S_spawn<-Sup_esc_age[t,2]+Sup_esc_age[t,3]-hob[t,i]     # Supp spawners = Esc Sup age 4 + Esc Sup age 5
      if(S_spawn <= 0){
        S_spawn <- 0}
      
      pHOS[t,i] = (S_spawn + Sup_esc_age[t,1]) / ((S_spawn + Sup_esc_age[t,1]) + (N_spawn + Nat_esc_age[t,1]))
      A_pNOB[t,i] = (nob[t,i]*(1-Hpsp))/BS
      PNI[t,i] = A_pNOB[t,i] / (pHOS[t,i] + A_pNOB[t,i])
      
      NoSup_spawn<-NoSup_esc_age[t,2]+NoSup_esc_age[t,3]     
      
      #Harvest
      if(Harvest) {
        if(Tot_esc[t,i] <= harvest_thres[1]) {harvest[t,i] <- 0} else
        {id <- max(which(harvest_thres < Tot_esc[t,i]))
        harvest[t,i] <- round((harvest_thres[id]*NPT_harvest[id]) + (Tot_esc[t,i]-harvest_thres[id])* NPT_harvest[id+1],0)}
      } else
        harvest[t,i] <- 0
      
      Spawn[t,i]<-N_spawn+S_spawn - harvest[t,i]      # Total fish spawning in the stream
      if(Spawn[t,i] <= 0){
        Spawn[t,i] <- 0}       
      
      if(Prodfixed) {p[t,i]<-mu_lambda  # fixes progeny returns mu_p
      tau[t,i]<-mu_tau}  else {
        
        p[t,i]<-rlnorm(1,mulog_lam,sdlog_lam)
        tau[t,i]<-rlnorm(1,mulog_tau,sdlog_tau)}
      
      if(RScurve=="Ricker") N_BY_rtn<-(p[t,i]*Spawn[t,i])*exp((-p[t,i]*Spawn[t,i])/k) else   #Ricker Prediciton for Nat progeny return * productivity R.V. following estimated gamma dist.
        N_BY_rtn<-(p[t,i]*Spawn[t,i])/(1+(p[t,i]*Spawn[t,i])/k)        # Beverton Holt
      
      if(RScurve=="Ricker") NoSup_BY_rtn<-(p[t,i]*NoSup_spawn)*exp((-p[t,i]*NoSup_spawn)/k) else
        NoSup_BY_rtn<-(p[t,i]*NoSup_spawn)/(1+(p[t,i]*NoSup_spawn)/k)
      
      if(BS<=0){            #if brood stock collection was zero then no fish could return
        S_BY_rtn<-0} else
        {S_BY_rtn<-(BS*p[t,i]*tau[t,i])}  #progeny returns from brood stock are subjected to same environmental noise as of natural fish returns.
      
      if(Agefixed) N_BY_rtn_age<-matrix(N_BY_rtn*Nage,nrow=3) else   # fixes age composition
        N_BY_rtn_age<-rmultinom(1, size = N_BY_rtn, prob = Nage)       # the age of Nat progeny returns follows a multinomial
      if(Agefixed) S_BY_rtn_age<-matrix(S_BY_rtn*Sage,nrow=3) else
        S_BY_rtn_age<-rmultinom(1, size = S_BY_rtn, prob = Sage)        #the age of Sup progeny returns follows a multinomial
      if(Agefixed) NoSup_BY_rtn_age<-matrix(NoSup_BY_rtn*Nage,nrow=3) else
        NoSup_BY_rtn_age<-rmultinom(1,size = NoSup_BY_rtn, prob=Nage)
      
      
      Nat_esc_age[t+3,1]<-N_BY_rtn_age[1,1]    #places Nat progeny returns into the correct escapement year based on age
      Nat_esc_age[t+4,2]<-N_BY_rtn_age[2,1]
      Nat_esc_age[t+5,3]<-N_BY_rtn_age[3,1]
      
      Sup_esc_age[t+3,1]<-S_BY_rtn_age[1,1]    #places Sup progeny returns into the correct escapement year based on age
      Sup_esc_age[t+4,2]<-S_BY_rtn_age[2,1]
      Sup_esc_age[t+5,3]<-S_BY_rtn_age[3,1]
      
      NoSup_esc_age[t+3,1]<-NoSup_BY_rtn_age[1,1]
      NoSup_esc_age[t+4,2]<-NoSup_BY_rtn_age[2,1]
      NoSup_esc_age[t+5,3]<-NoSup_BY_rtn_age[3,1]
      
    } # close t loop
  } #close j loop
  output <- list(Tot_esc,Nat_esc,Spawn,NoSup_esc,harvest,hfrac,A_pNOB,pHOS,PNI,p,tau,nob,hob,Jack_esc)
  names(output) <- c("Tot_esc","Nat_esc","Spawn","No_Sup","Harvest","Hfraction","A_pNOB","pHOS","PNI","N_lambda","RRS","NOB","HOB","Jack_Esc")
  return<- output
} # Close Function