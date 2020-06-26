gen_search_grid_dep <- function(param_outbreak_tmp){
  tmp <- param_outbreak_tmp %>% slice(1:nrow(hcw_RR))
  
  tmp["IR1"] <- hcw_RR$od_hcw %>% 
    replace(., is.nan(.),1) %>% 
    replace(., is.infinite(.),1)
  tmp["IR2"] <- hcw_RR$rd_hcw %>% 
    replace(., is.nan(.),1) %>% 
    replace(., is.infinite(.),1)
  tmp["IR3"] <- hcw_RR$fc_hcw %>% 
    replace(., is.nan(.),1) %>% 
    replace(., is.infinite(.),1)
  
  n_test <- seq(0,100000,200)
  search_grid_dep_tmp <- vector("list", nrow(hcw_RR))
  names(search_grid_dep_tmp) <- 1:nrow(hcw_RR)
  pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent in :elapsed) eta: :eta",
    total = nrow(hcw_RR)-1,
    width = 80)
  
  for(t in 2:nrow(hcw_RR)){
    cbind(
      # Fever Clinic Patient
      # Numerator = Infectious and Symptomatic
      # Denominator = Acute Respiratory patients + Infectious and Symptomatic
      sapply(n_test, function(x) phyper(q = 0.1,
                                        m = (tmp$Io[t]*sen["chau_c_early"]),
                                        n = (tmp$n_denom_fc[t] - 
                                               tmp$Io[t]*sen["chau_c_early"]),
                                        k = x)),
      # Respiratory Department Patients
      # Numerator = Infectious and Symptomatic but have not yet visited fever clinic
      # Denominator = All respiratory patients + Infectious and Symptomatic + haven't visited fever clinic yet
      sapply(n_test, function(x) phyper(q = 0.1,
                                        m = ((tmp$Io[t] - 
                                                n_count$fc_patient*tmp$Io[t]/tmp$n_denom_fc[t])*sen["chau_c_early"]),
                                        n = (tmp$n_denom_rd[t] - 
                                               n_count$fc_patient -
                                               (tmp$Io[t] - n_count$fc_patient*tmp$Io[t]/tmp$n_denom_fc[t])*sen["chau_c_early"]),
                                        k = x)),
      # Other Hospital Department Patients
      # Numerator = Infectious and Pre-symptoamtic + Infectious and Subclinical + Recovered but positive
      # Denominator = All those who are not symptomatic
      sapply(n_test, function(x) phyper(q = 0.1,
                                        m = (tmp$Ip[t]*sen["chau_c_early"] +
                                               tmp$Iu[t]*sen["chau_sc_early"] +
                                               tmp$Ro_pos[t]*sen["chau_c_late"]+
                                               tmp$Ru_pos[t]*sen["chau_sc_late"]),
                                        n = (tmp$n_denom_baseline[t] - 
                                               (tmp$Io[t]) -
                                               (tmp$Ip[t]*sen["chau_c_early"] +
                                                  tmp$Iu[t]*sen["chau_sc_early"] +
                                                  tmp$Ro_pos[t]*sen["chau_c_late"] +
                                                  tmp$Ru_pos[t]*sen["chau_sc_late"])),
                                        k = x)),
      # HCW
      # Numerator = Infectious and pre-symptomatic + Infectious and subclinical + Previously infectious and subclinical
      # Denominator = All those who are not symptomatic (whether currently or previously)
      sapply(n_test, function(x) phyper(q = 0.1,
                                        m = ((tmp$Ip[t]*sen["chau_c_early"] +
                                                tmp$Iu[t]*sen["chau_sc_early"] +
                                                tmp$Ru_pos[t]*sen["chau_sc_late"])*tmp$IR3[t]),
                                        n = (tmp$n_denom_baseline[t] - 
                                               (tmp$Io[t] + 
                                                  tmp$Ro_pos[t]) -
                                               (tmp$Ip[t]*sen["chau_c_early"] +
                                                  tmp$Iu[t]*sen["chau_sc_early"] +
                                                  tmp$Ru_pos[t]*sen["chau_sc_late"])*tmp$IR3[t]),
                                        k = x)),
      sapply(n_test, function(x) phyper(q = 0.1,
                                        m = ((tmp$Ip[t]*sen["chau_c_early"] +
                                                tmp$Iu[t]*sen["chau_sc_early"] +
                                                tmp$Ru_pos[t]*sen["chau_sc_late"])*tmp$IR2[t]),
                                        n = (tmp$n_denom_baseline[t] - 
                                               (tmp$Io[t] + 
                                                  tmp$Ro_pos[t]) -
                                               (tmp$Ip[t]*sen["chau_c_early"] +
                                                  tmp$Iu[t]*sen["chau_sc_early"] +
                                                  tmp$Ru_pos[t]*sen["chau_sc_late"])*tmp$IR2[t]),
                                        k = x)),
      sapply(n_test, function(x) phyper(q = 0.1,
                                        m = ((tmp$Ip[t]*sen["chau_c_early"] +
                                                tmp$Iu[t]*sen["chau_sc_early"] +
                                                tmp$Ru_pos[t]*sen["chau_sc_late"])*tmp$IR1[t]),
                                        n = (tmp$n_denom_baseline[t] - 
                                               (tmp$Io[t] + 
                                                  tmp$Ro_pos[t]) -
                                               (tmp$Ip[t]*sen["chau_c_early"] +
                                                  tmp$Iu[t]*sen["chau_sc_early"] +
                                                  tmp$Ru_pos[t]*sen["chau_sc_late"])*tmp$IR1[t]),
                                        k = x)),
      # commuinty
      # everyone in that age group can be detected if they are previously or currently infectious
      sapply(n_test, function(x) phyper(q = 0.1,
                                        m = (tmp$n_COVID19_child_Ip[t]*sen["chau_c_early"] +
                                               tmp$n_COVID19_child_Io[t]*sen["chau_c_early"] +
                                               tmp$n_COVID19_child_Iu[t]*sen["chau_sc_early"] +
                                               tmp$n_COVID19_child_Ro_pos[t]*sen["chau_c_early"] +
                                               tmp$n_COVID19_child_Ru_pos[t]*sen["chau_sc_early"]),
                                        n = (tmp$n_denom_child[t] - 
                                               (tmp$n_COVID19_child_Ip[t]*sen["chau_c_early"] +
                                                  tmp$n_COVID19_child_Io[t]*sen["chau_c_early"] +
                                                  tmp$n_COVID19_child_Iu[t]*sen["chau_sc_early"] +
                                                  tmp$n_COVID19_child_Ro_pos[t]*sen["chau_c_early"] +
                                                  tmp$n_COVID19_child_Ru_pos[t]*sen["chau_sc_early"])),
                                        k = x)),
      sapply(n_test, function(x) phyper(q = 0.1,
                                        m = (tmp$n_COVID19_adult_Ip[t]*sen["chau_c_early"] +
                                               tmp$n_COVID19_adult_Io[t]*sen["chau_c_early"] +
                                               tmp$n_COVID19_adult_Iu[t]*sen["chau_sc_early"] +
                                               tmp$n_COVID19_adult_Ro_pos[t]*sen["chau_c_early"] +
                                               tmp$n_COVID19_adult_Ru_pos[t]*sen["chau_sc_early"]),
                                        n = (tmp$n_denom_adult[t] - 
                                               (tmp$n_COVID19_adult_Ip[t]*sen["chau_c_early"] +
                                                  tmp$n_COVID19_adult_Io[t]*sen["chau_c_early"] +
                                                  tmp$n_COVID19_adult_Iu[t]*sen["chau_sc_early"] +
                                                  tmp$n_COVID19_adult_Ro_pos[t]*sen["chau_c_early"] +
                                                  tmp$n_COVID19_adult_Ru_pos[t]*sen["chau_sc_early"])),
                                        k = x)),
      sapply(n_test, function(x) phyper(q = 0.1,
                                        m = (tmp$n_COVID19_elderly_Ip[t]*sen["chau_c_early"] +
                                               tmp$n_COVID19_elderly_Io[t]*sen["chau_c_early"] +
                                               tmp$n_COVID19_elderly_Iu[t]*sen["chau_sc_early"] +
                                               tmp$n_COVID19_elderly_Ro_pos[t]*sen["chau_c_early"] +
                                               tmp$n_COVID19_elderly_Ru_pos[t]*sen["chau_sc_early"]),
                                        n = (tmp$n_denom_elderly[t] - 
                                               (tmp$n_COVID19_elderly_Ip[t]*sen["chau_c_early"] +
                                                  tmp$n_COVID19_elderly_Io[t]*sen["chau_c_early"] +
                                                  tmp$n_COVID19_elderly_Iu[t]*sen["chau_sc_early"] +
                                                  tmp$n_COVID19_elderly_Ro_pos[t]*sen["chau_c_early"] +
                                                  tmp$n_COVID19_elderly_Ru_pos[t]*sen["chau_sc_early"])),
                                        k = x))) -> search_grid_dep_tmp[[t]]
    
    pb$tick()
  }
  return(search_grid_dep_tmp) 
}

