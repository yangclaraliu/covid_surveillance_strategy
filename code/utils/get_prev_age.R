get_prev_age <- function(tab){
  tab %>%
    mutate(AG = case_when(Age %in% 1:3 ~ "Children",
                          Age %in% 4:12 ~ "Younger Adults",
                          Age %in% 13:16 ~ "Older Adults")) %>% 
    group_by(comp, t, AG, Rt) %>% 
    summarise(counts = sum(value)) %>% 
    pivot_wider(names_from = comp,
                values_from = counts) %>% 
    mutate(I = Io + Iu + Ip,
           R = Ro_pos + Ru_pos) %>% 
    # pivot_longer(cols = c("Ip","Io","Iu","Ro_pos","Ru_pos","I"),
    #              names_to = "comp",
    #              values_to = "count") %>% 
    left_join(age_dist_tmp %>% 
                enframe %>% 
                # find defintions of different age groups 
                mutate(AG = case_when(name %in% 1:3 ~ "Children",
                                      name %in% 5:12 ~ "Younger Adults",
                                      name %in% 14:16 ~ "Older Adults")) %>% 
                group_by(AG) %>% 
                summarise(pop = sum(value)),by = "AG") -> tmp
  return(tmp)
}