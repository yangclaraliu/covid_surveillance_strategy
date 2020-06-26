# @title: get_sen.r
# @objective: Calculate duration of time spent in R plus stage
# @author: Yang Liu
# @created: 12/6/2020
# @updated: 12/6/2020
# Data used in this file is from
# (1) Chau et al. https://www.medrxiv.org/content/10.1101/2020.04.27.20082347v1.full.pdf
# (2) Long et al. https://www.sciencedirect.com/science/article/pii/S0720048X20301509
# (3) Ai et al. https://pubs.rsna.org/doi/10.1148/radiol.2020200642

paste0("data/PCR_sen/", list.files("data/PCR_sen/", pattern = "txt")) %>%
  {setNames(object =  as.list(.), nm = sub(pattern = "data/PCR_sen/(.*)\\.txt",
                                           x = ., replacement = "\\1"))} %>%
  map(read.table, sep=",", stringsAsFactors = FALSE) %>% 
  map(~setNames(., c("t","p")) %>%
        as_tibble %>% 
        mutate(t = round(t,2))) %>% 
  map(right_join, data.frame(t = seq(0,19,by = 0.01)), by = "t") %>% 
  map_df(mutate, p = imputeTS::na_interpolation(p),
         .id = "id") %>% 
  mutate(status = forcats::fct_recode(factor(id),
                                      SC = "Asymptomatic",
                                      C  = "Symptomatic")) %>%
  dplyr::select(-id) -> sen_imputed 

sen_imputed %>% 
  filter(t <= params$dinf) %>% 
  group_by(status) %>% 
  summarise(g = mean(p)) -> before

sen_imputed %>% 
  filter(t > params$dinf &
           t <= 10) %>% 
  group_by(status) %>% 
  summarise(g = mean(p)) -> after

sen <- c(sen_decreased = (after[after$status == "C",]$g/before[before$status == "C",]$g),
        sen_ai = 0.59,
        sen_long = 0.83,
        chau_c_early = before$g[1],
        chau_sc_early = before$g[2],
        chau_c_late = after$g[1],
        chau_sc_late = after$g[2])

# ggplot(., aes(x = t,
#               y = p,
#               color = status))+
# geom_point()
