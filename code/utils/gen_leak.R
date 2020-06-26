gen_leak <- function(grid_tmp,
                     inc_tmp){
  grid_tmp %>% 
    map(as_tibble) %>% 
    map(setNames, colnames(n_count)) %>% 
    map(rownames_to_column, var = "r") %>% 
    bind_rows(.id = "t") %>% 
    mutate(t = as.numeric(t),
           r = as.numeric(r)*200) %>% 
    left_join(inc_tmp[,c("t","inc")], by = "t") %>% 
    pivot_longer(cols = colnames(n_count)) %>% 
    mutate(name = factor(name,
                         levels = colnames(n_count),
                         labels = c("Fever Clinics - Patients",
                                    "Respiratory Departments - Patients",
                                    "Other Departments - Patients",
                                    "Fever Clinics - HCW",
                                    "Respiratory Departments - HCW",
                                    "Other Departments - HCW",
                                    "Community - Children",
                                    "Community - Adults",
                                    "Community - Elderly"))) %>% 
    filter(name %in% c("Fever Clinics - Patients",
                       "Respiratory Departments - Patients")) %>%  
    pivot_wider(names_from = name,
                values_from = value) %>% 
    mutate(prod_fc = NA,
           prod_rd = NA) -> res
  return(res)
}