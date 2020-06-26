gen_outbreak_param  <- function(p_acute_tmp,
                                p_chronic_tmp,
                                prevalence_tmp,
                                prevalence_age_tmp){
  
  p_respiratory_tmp      <- p_acute_tmp + p_chronic_tmp
  n_acute_tmp            <- round(pop_tmp$tot*p_acute_tmp)
  n_respiratory_tmp      <- round(pop_tmp$tot*p_chronic_tmp)
  param_outbreak_tmp     <- prevalence_tmp 

  prevalence_age_tmp %>%
    mutate(group = case_when(grepl(pattern = "Children", x = AG) ~ "child",
                             grepl(pattern = "Younger",  x = AG) ~ "adult",
                             grepl(pattern = "Older",    x = AG) ~ "elderly",
                             TRUE                                ~ NA_character_),
           group = paste("n","COVID19", group, sep="_")) %>%
    ungroup %>%
    dplyr::select(Rt, Ip, Io, Iu, Ro_pos, Ru_pos, group, t) %>%
    gather(key, value, -group, -Rt, -t) %>%
    unite(col = "key", group, key) %>%
    spread(key, value) -> prevalence_age_tmp_
  
  prevalence_age_pops_tmp <-
    prevalence_age_tmp %>%
    ungroup %>%
    distinct(pop, AG) %>%
    mutate(group = case_when(grepl(pattern = "Children", x = AG) ~ "child",
                             grepl(pattern = "Younger",  x = AG) ~ "adult",
                             grepl(pattern = "Older",    x = AG) ~ "elderly",
                             TRUE                                ~ NA_character_)) %>%
    dplyr::select(-AG) %>%
    spread(group, pop)
  
  param_outbreak_tmp %<>%
    ungroup %>% 
    inner_join(prevalence_age_tmp_, by = c("t", "Rt")) %>%
    mutate(n_denom_fc = n_acute_tmp + Io,
           n_denom_rd = n_respiratory_tmp + Io,
           n_denom_baseline = pop_tmp$tot) %>%
    crossing(rename_all(prevalence_age_pops_tmp, .funs = function(x){paste("n","denom",x, sep="_")}))
  
  
  return(param_outbreak_tmp)
}