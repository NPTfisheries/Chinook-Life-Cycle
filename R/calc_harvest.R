#' Function returns the allowable harvest based on NOR and HOR returns and brood stock needs.
#' Calculations were originally copied from Kyle Bratcher's R script and Jack Yearout's
#' spreadsheet.
#'
#' Authors Ryan Kinzer & Brian Simmons
#' Created May 10, 2024
#' 
#'


nor_esc = 326 # 2024 pre season estimate
hor_esc = 576 # 2024 pre-season estimate
broodstock = 164

# calc_harvest(nor_esc = nor_esc, hor_esc = hor_esc, broodstock = broodstock)

calc_harvest <- function(nor_esc, hor_esc, broodstock){
  # ADULT BROODSTOCK NEEDS FROM AOP ---- 
  
  # *****UPDATE AS NECESSARY*****
  lst_bs_need <- broodstock #Lostine adult broodstock needs
  
  # Forecasts ----
  
  # Forecast natural origin (NO) adults for Lostine River (LST)
  
    lst_NO <- nor_esc
  
  # Forecast hatchery origin (HO) adults for Lostine River (LST)
  
    lst_HO <- hor_esc
  
  # Wallowa/Lostine (WAL_LST) expansion natural origin (NO) adults
  
  # Forecast total NO WAL/LST ****"1.4" is the expansion used based on redd counts within the basin. 
  # ****This may require updating in the future.****
  
  wal_lst_NO<-round(lst_NO * 1.4,0)
  
  # Allowed Natural Origin (NO) Impacts ----
  
# Allowed NO Sportfish Impacts Wallowa/Lostine (WAL_LST) ----
  # References harvest scale
    
    if(wal_lst_NO<300){
      sprt_NO_impact<-0
    }else{
      if(wal_lst_NO<1000) {
        sprt_NO_impact<-round((wal_lst_NO-300)*.03,0)
      }else{
        if(wal_lst_NO<1500){
          sprt_NO_impact<-round(21+(1499-999)*.06,0)
        }else{
          if(wal_lst_NO<2000){
            sprt_NO_impact<-round(51+(1999-1500)*.06,0)
          }else{
            if(wal_lst_NO>1999){
              sprt_NO_impact<-round(81+(wal_lst_NO-1500)*.12,0)}
          }
        }}}
    
    # sprt_NO_impact = the number of allowed NOR take from incidental mortality
    
    # Sport Handle Rate - Wallowa/Lostine (WAL_LST) - assumes a 10% mortality on all caught NOR fish
    
    wal_lst_NO_hndl<-sprt_NO_impact*10 # This might not be necessary for our purposes.
    
    # allowed wild impact from FMEP - from Jack and it doesn't match Kyle's above
    # if(((wal_lst_NO - 300)*.03)<0){
    #   wild_impact <- 0
    # } else {
    #   wild_impact <- round(((wal_lst_NO - 300)*.03),0)
    # }
    # 
    # impact_rate <- wild_impact/wal_lst_NO
    # wild_hnd <- wild_impact*10   # same as wal_lst_NO_hndl
  
# Sport Hatchery Origin Harvest ----
  
    sprt_HO_hvst<-round((wal_lst_NO_hndl/wal_lst_NO)*lst_HO) 
  
    # ODFW assumes that the catch rate is equal for Natural Origin and Hatchery Origin Fish
    # ODFW sets sport harvest as the proportion of NO handled /  NO return * the Hatchery Origin Return
    # This theoretically sets the maximum HO harvest limit and stays at or under NO impact
  
  

# Tribal Natural Origin Impact ----
  
    # This Calculation Matches Jack's Spreadsheet
  
        if(wal_lst_NO < 300){
          treaty_NO_impact <- wal_lst_NO * .01
        } else {
          if(wal_lst_NO < 1000){
            treaty_NO_impact <- 3 + ((wal_lst_NO - 300) * .08)
          } else {
            if(wal_lst_NO < 1500){
              treaty_NO_impact <- 59 + ((wal_lst_NO - 1000) * .16)
            } else {
              if(wal_lst_NO < 2000){
                treaty_NO_impact <- 139 + ((wal_lst_NO - 1500) * .19)
              } else {
                if(wal_lst_NO >= 2000){
                  treaty_NO_impact <- 234 + ((wal_lst_NO - 1999) * .28)
                } else {
                  treaty_NO_impact <- 0
                }
              }
            }
          }
        }

        treaty_NO_impact <- round(treaty_NO_impact, 0)

    # This calculation matches ODFW the difference will impact TOTAL NO impact
    # 
        
        # if(wal_lst_NO < 225){
        #   treaty_NO_impact <- round(wal_lst_NO * .01)
        # }else{
        #   if(wal_lst_NO < 750){
        #     treaty_NO_impact <- round(4.5 + (lst_NO - 225) * .08, 0)
        #   }else{
        #     if(wal_lst_NO < 1125){
        #       treaty_NO_impact <- round(47.25 + (lst_NO - 750) * .16, 0)
        #     }else{
        #       if(wal_lst_NO < 1500){
        #         treaty_NO_impact <- round(114.75 + (lst_NO - 1125) * .19, 0)
        #       }else{
        #         if(wal_lst_NO > 1499){
        #           treaty_NO_impact <- round(114.75 + (lst_NO - 1500) * .28, 0)}
        #       }
        #     }
        #   }
        # }
        # 
        # treaty_NO_impact <- round(treaty_NO_impact, 0)
      
      
          
        
# Tribal Hatchery Origin Harvest ----
      
      #Assumes that ODFW correctly calculates TOTAL harvest share and NPT share is equivalent.
  
      treaty_HO_hvst <- (sprt_HO_hvst + sprt_NO_impact) - treaty_NO_impact
      
  
      # tribal harvest sliding scale
      # this matches NPT long-term Imnaha/Grande Ronde harvest plan
      # Does NOT match ODFW NPT harvest share agreement
      # 
      # if(lst_HO < 300){
      #   treaty_HO_hvst <- lst_HO * .02
      # } else {
      #   if(wal_lst_NO < 1000){
      #     treaty_HO_hvst <- 6 + ((lst_HO - 300) * .09)
      #   } else {
      #     if(wal_lst_NO < 1500){
      #       treaty_HO_hvst <- 69 + ((lst_HO - 1000) * .18)
      #     } else {
      #       if(wal_lst_NO < 2000){
      #         treaty_HO_hvst <- 159 + ((lst_HO - 1500) * .21)
      #       } else {
      #         if(wal_lst_NO >= 2000){
      #           treaty_HO_hvst <- 264 + ((lst_HO - 1999) * .4)
      #         } else {
      #           treaty_HO_hvst <- 0
      #         }
      #       }
      #     }
      #   }
      # }
      # 
      #   treaty_HO_hvst <- round(treaty_HO_hvst, 0)


# Total harvest by origin ----
  
  nor_harvest = sprt_NO_impact + treaty_NO_impact # includes the NOR incidental mortality impact of sport harvest and the number of tribal harvest
  hor_harvest = sprt_HO_hvst + treaty_HO_hvst 

    return(list('nor_harvest' = nor_harvest, 'hor_harvest' = hor_harvest))
}
