#' Function returns the allowable harvest based on NOR and HOR returns and brood stock needs.
#' Calculations were originally copied from Kyle Bratcher's R script and Jack Yearout's
#' spreadsheet.
#'
#' Author Ryan Kinzer
#' Created May 10, 2024
#' 
#'


#nor_esc = 326
#hor_esc = 576
#broodstock = 163

calc_harvest <- function(nor_esc, hor_esc, broodstock){
  # ADULT BROODSTOCK NEEDS FROM AOP ---- 
  
  # *****UPDATE AS NECESSARY*****
  lst_bs_need <- broodstock #Lostine adult broodstock needs
  
  # Forecasts ----
  
  # Forecast natural origin (NO) adults for Lostine River (LST)
  
    lst_no <- nor_esc
  
  # Forecast hatchery origin (HO) adults for Lostine River (LST)
  
    lst_ho <- hor_esc
  
  # Wallowa/Lostine (WAL_LST) expansion natural origin (NO) adults
  
  # Forecast total NO WAL/LST ****"1.4" is the expansion used based on redd counts within the basin. 
  # ****This may require updating in the future.****
  
  wal_lst_no<-round(lst_no*1.4,0)
  
  # Allowed Natural Origin (NO) Impacts ----
  
  # Allowed NO Sportfish Impacts Wallowa/Lostine (WAL_LST) ----
  # References harvest scale
  if(wal_lst_no<300){
    wal_lst_sprt_impct<-0
  }else{
    if(wal_lst_no<1000) {
      wal_lst_sprt_impct<-round((wal_lst_no-300)*.03,0)
    }else{
      if(wal_lst_no<1500){
        wal_lst_sprt_impct<-round(21+(1499-999)*.06,0)
      }else{
        if(wal_lst_no<2000){
          wal_lst_sprt_impct<-round(51+(1999-1500)*.06,0)
        }else{
          if(wal_lst_no>1999){
            wal_lst_sprt_impct<-round(81+(wal_lst_no-1500)*.12,0)}
        }
      }}}
  
  # wal_lst_sprt_impct = the number of allowed NOR take from incidental mortality
  
  # Sport Handle Rate - Wallowa/Lostine (WAL_LST) - assumes a 10% mortality on all caught NOR fish
  wal_lst_no_hndl<-wal_lst_sprt_impct*10
  
  # allowed wild impact from FMEP - from Jack and it doesn't match Kyle's above
  # if(((wal_lst_no - 300)*.03)<0){
  #   wild_impact <- 0
  # } else {
  #   wild_impact <- round(((wal_lst_no - 300)*.03),0)
  # }
  # 
  # impact_rate <- wild_impact/wal_lst_no
  # wild_hnd <- wild_impact*10   # same as wal_lst_no_hndl
  
  # Sport Harvest ----
  # from Kyle
  
  HOsprt_hvst<-round((wal_lst_no_hndl/wal_lst_no)*lst_ho) # what does this mean?????? if we catch the same proportion of HOR as we can catch for NOR and stay under the take limit, this is the number of HOR fish we can catch/keep, doesn't account for brood needs
  
  # Allowed Tribal natural origin (NO) Impacts (WAL_LST) ----
  
  #References harvest scale - does not match Jack's
  if(wal_lst_no<225){
    lst_trb_impct<-round(wal_lst_no*.01)
  }else{
    if(wal_lst_no<750){
      lst_trb_impct<-round(4.5+(lst_no-225)*.08,0)
    }else{
      if(wal_lst_no<1125){
        lst_trb_impct<-round(47.25+(lst_no-750)*.16,0)
      }else{
        if(wal_lst_no<1500){
          lst_trb_impct<-round(114.75+(lst_no-1125)*.19,0)
        }else{
          if(wal_lst_no>1499){
            lst_trb_impct<-round(114.75+(lst_no-1500)*.28,0)}
        }
      }}}
  
  # used in JCAPE analysis but they don't match Jack's spreadsheet
  # pMAT_upper <- c(.3, .5, .75, 1.08)
  # NPT_harvest <- c(.01, .02, .03, .044, .175)
  # harvest_thres <- viable * pMAT_upper
  
  # tribal harvest sliding scale
  if(lst_ho < 300){
    treaty_hor_harvest <- lst_ho * .02
  } else {
    if(wal_lst_no < 1000){
      treaty_hor_harvest <- 6 + ((lst_ho - 300) * .09)
    } else {
      if(wal_lst_no < 1500){
        treaty_hor_harvest <- 69 + ((lst_ho - 1000) * .18)
      } else {
        if(wal_lst_no < 2000){
          treaty_hor_harvest <- 159 + ((lst_ho - 1500) * .21)
        } else {
          if(wal_lst_no >= 2000){
            treaty_hor_harvest <- 264 + ((lst_ho - 1999) * .4)
          } else {
            treaty_hor_harvest <- 0
          }
        }
      }
    }
  }
  
  treaty_hor_harvest <- round(treaty_hor_harvest, 0)
  
  if(wal_lst_no < 300){
    treaty_nor_harvest <- wal_lst_no * .01
  } else {
    if(wal_lst_no < 1000){
      treaty_nor_harvest <- 3 + ((wal_lst_no - 300) * .08)
    } else {
      if(wal_lst_no < 1500){
        treaty_nor_harvest <- 59 + ((wal_lst_no - 1000) * .16)
      } else {
        if(wal_lst_no < 2000){
          treaty_nor_harvest <- 139 + ((wal_lst_no - 1500) * .19)
        } else {
          if(wal_lst_no >= 2000){
            treaty_nor_harvest <- 234 + ((wal_lst_no - 1999) * .28)
          } else {
            treaty_nor_harvest <- 0
          }
        }
      }
    }
  }
  
  treaty_nor_harvest <- round(treaty_nor_harvest, 0)
  #treaty_hor_harvest <- (wild_impact + HOsprt_hvst) - treaty_harvest

  # Hatchery Origin (HO) Harvest Shares ---- 
  # !!!!THESE ARE PRELIMINARY ESTIMATES. DO NOT USE AS FINAL HARVEST SHARES!!!!----
  
  
  # lst_ho - lst_bs_need
  # HOsprt_hvst + treaty_hor_harvest
  
  nor_harvest = wal_lst_sprt_impct + treaty_nor_harvest #should include the NOR incidental mortality impact of sport harvest and the number of tribal harvest
  hor_harvest = HOsprt_hvst + treaty_hor_harvest #is this only a 50% share, should it be doubled to include the sport harvest share plus the tribal harvest share

    return(list('nor_harvest' = nor_harvest, 'hor_harvest' = hor_harvest))
}
