#' Function returns the allowable harvest based on NOR and HOR returns and brood stock needs.
#'
#' Author Ryan Kinzer
#' Created May 10, 2024
#' 
#'

calc_harvest <- function(nor_esc, hor_esc, broodstock){
  # ADULT BROODSTOCK NEEDS FROM AOP ---- 
  
  # *****UPDATE AS NECESSARY*****
  lst_bs_need<- broodstock #Lostine adult broodstock needs
  
  # Forecasts ----
  
  # Forecast natural origin (NO) adults for Lostine River (LST)
  
  
    lst_no<-nor_esc
  
  # Forecast hatchery origin (HO) adults for Lostine River (LST)
  
  
    lst_ho<-hor_esc
  
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
  
  # Sport Handle Rate - Wallowa/Lostine (WAL_LST)
  wal_lst_no_hndl<-wal_lst_sprt_impct*10
  
  # Allowed Tribal natural origin (NO) Impacts (WAL_LST) ----
  
  #References harvest scale
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
  
  # Hatchery Origin (HO) Harvest Shares ---- 
  # !!!!THESE ARE PRELIMINARY ESTIMATES. DO NOT USE AS FINAL HARVEST SHARES!!!!----
  
  # Sport Harvest ----
  
  HOsprt_hvst<-round((wal_lst_no_hndl/wal_lst_no)*lst_ho) 
  
  nor_harvest = lst_trb_impct
  hor_harvest = HOsprt_hvst
  return(list('nor_harvest' = nor_harvest, 'hor_harvest' = hor_harvest))
}
