# @title: health_param.R
# @objective: 
# @author: Yang Liu, Sam Clifford

pop[pop$CNTY_CODE %in% c("110100", "310100"),] %>%
  mutate(r_hcw = c(9.1, 6.3)/1000,
         n_p = round(tot*(1.35/0.1)/1000),
         n_hcw = round(r_hcw*tot*5/7),
         pphcw = n_p/n_hcw) %>% 
  filter(CNTY_CODE == cnty) %>% 
  pull(pphcw) -> pphcw
# n_settings <- c("house", "other", "work")
# n_community <- c("adult", "hcw")

t_max <- 60
p_acute = 0.03
p_chronic = 0.07

gen_outbreak_param(p_acute_tmp = p_acute,
                   p_chronic_tmp = p_chronic,
                   prevalence_tmp = prevalence %>% filter(Rt == 2),
                   prevalence_age_tmp = prevalence_age %>% filter(Rt == 2))  -> param_outbreak

lapply(1:length(prevalence_sensitivity), function(i){
  gen_outbreak_param(p_acute_tmp = p_acute,
                     p_chronic_tmp = p_chronic,
                     prevalence_tmp = prevalence_sensitivity[[i]],
                     prevalence_age_tmp = prevalence_age_sensitivity[[i]])}) -> param_outbreak_sensitivity

n_count <- data.frame(fc_patient = ceiling((0.07*pop_tmp$tot/1000)/200)*200,
                      rd_patient = ceiling((1.35*pop_tmp$tot/1000)/200)*200) %>% #0.81, 1.35, 1.89
  mutate(fc_hcw = round(fc_patient/pphcw),
         rd_hcw = round(rd_patient/pphcw),
         od_patient = round((rd_patient/(p_acute+p_chronic))*(1-(p_acute+p_chronic)))) %>%
  mutate(od_hcw = round(od_patient/pphcw),
         child = unique(prevalence_age[prevalence_age$AG == "Children",]$pop),
         adult = unique(prevalence_age[prevalence_age$AG == "Younger Adults",]$pop),
         elderly = unique(prevalence_age[prevalence_age$AG == "Older Adults",]$pop))

n_count <- dplyr::select(n_count, c("fc_patient",
                                    "rd_patient",
                                    "od_patient",
                                    "fc_hcw",
                                    "rd_hcw",
                                    "od_hcw",
                                    "child",
                                    "adult",
                                    "elderly"))

