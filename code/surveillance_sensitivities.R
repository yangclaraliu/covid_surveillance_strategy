# @title: surveillance_sensitivities.r
# @objective: estimate the representativeness of healthcare worker compare to baseline
# @author: Yang Liu, Sam Clifford

if (file.exists("code/results/search_grid.rdata")){
  load("code/results/search_grid.rdata")  
} else {
  gen_search_grid(param_outbreak) -> search_grid
  search_grid_sensitivity <- list()
  for(i in 1:length(param_outbreak_sensitivity)){
    print(sprintf("Sensitivity analysis: running %d of %d", i, length(param_outbreak_sensitivity)))
    gen_search_grid(param_outbreak_sensitivity[[i]]) -> search_grid_sensitivity[[i]]
  }
  save(search_grid, search_grid_sensitivity, file = "code/results/search_grid.rdata")
}

if (file.exists("code/results/search_grid_dep.rdata")){
  load("code/results/search_grid_dep.rdata")  
} else {
  gen_search_grid_dep(param_outbreak) -> search_grid_dep
  search_grid_sensitivity_dep <- list()
  for(i in 1:length(param_outbreak_sensitivity)){
    print(sprintf("Sensitivity analysis: running %d of %d", i, length(param_outbreak_sensitivity)))
    gen_search_grid_dep(param_outbreak_sensitivity[[i]]) -> search_grid_sensitivity_dep[[i]]
  }
  save(search_grid_dep, search_grid_sensitivity_dep, file = "code/results/search_grid_dep.rdata")
}

sensitivity_plot <- search_grid
sensitivity_plot %<>%
  map(as_tibble) %>%
  map(setNames, colnames(n_count)) %>%
  map(rownames_to_column, var = "r") %>%
  bind_rows(.id = "t") %>%
  mutate(t = as.numeric(t),
         r = as.numeric(r)*200) %>%
  left_join(incidence[incidence$Rt==2,c("t","inc")], by = "t") %>%
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
                                  "Community - Elderly")))


timings <- data.frame(with(incidence, approx(x = inc, y = t, xout = 10^seq(1,3)))) %>%
  rename(inc = x, t = y)

surveillance_sensitivity_v2 <-
  sensitivity_plot %>%
  filter(r <= 1e5,
         inc <= 1e4,
         t >= 5) %>%
  mutate(value = 1 - pmin(1, pmax(0, value)),
         value_d = cut(value, breaks = seq(0,1,by=0.25))) %>%
  ggplot(., aes(x = t,
                y = r))+
  geom_tile(aes(fill = value)) +
  #  geom_contour(aes(z = value), breaks = 0.5, color = "black", lty = 2) +
  #  geom_contour(aes(z = value), breaks = c(0.025, 0.975), color = "black", lty = 3) +
  theme_bw() +
  labs(x = "Time since first case (days)",
       y = "Number of Tests Conducted") +
  #ggsci::scale_fill_material("red")+
  scale_fill_viridis(name = "Surveillance Sensitivity of COVID-19 Transmission") +
  geom_vline(data = timings, aes(xintercept = t),
             color = "white",
             linetype = 2)+
  geom_text(data = timings,
            aes(x = t, y = 80000,
                label = sprintf("%i cases in community",inc)),
            color = "white",
            fontface = "italic",
            angle = 90,
            hjust = 1,
            vjust = 0,
            size  = 6,
            nudge_y = 0,
            nudge_x = -0.5) +
  
  facet_wrap(~name, dir="v")+
  scale_y_continuous(breaks = 1000*seq(0,100,by=20),
                     labels = label_K, expand = expansion(mult = c(0, 0.1))) +
  #scale_x_log10(breaks = c(10,100,1000,10000,100000))+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        #strip.background = element_rect(NA),
        panel.grid = element_blank(),
        legend.position = "bottom") +
  guides(fill=guide_colourbar(barwidth=20, label.position="bottom", title.vjust = 1))

surveillance_sensitivity_v2

ggsave(plot = color_strip(surveillance_sensitivity_v2),
       filename = "figs/surveillance_sensitivity_final.png",
       height = 15,
       width = 15)

all_scenarios <- param_outbreak_sensitivity %>%
  map(pull, lab) %>% 
  map(unique) %>% 
  unlist() 

calc_sur_sen <- function(resource = n_count$fc_patient,
                         scenario_tmp){
  
  search_grid_sensitivity[which(all_scenarios == scenario_tmp)] -> tmp
  
  incidence_sensitivity[[scenario_tmp]] -> inc_tmp
  
  tmp %>% 
    .[[1]] %>% 
    map(as_tibble) %>%
    map(setNames, colnames(n_count)) %>%
    map(rownames_to_column, var = "r") %>%
    bind_rows(.id = "t") %>%
    mutate(t = as.numeric(t),
           r = as.numeric(r)*200) %>%
    left_join(inc_tmp, by = "t") %>%
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
    filter(r == resource) %>% 
    filter(inc > 100) %>% 
    mutate(rk = rank(inc, ties.method = "average")) %>% 
    filter(rk == min(rk)) %>% 
    mutate(p = (1-value)*100) -> x
  return(x)
}

lapply(1:length(all_scenarios), function(s) calc_sur_sen(scenario_tmp = all_scenarios[s])) -> sur_sen

sensitivity_table %>% 
  rownames_to_column() %>% 
  filter(dincu == 5,
         dinf_u == 6,
         theta == 0.14)

sur_sen %>% 
  bind_rows() %>% 
  separate(lab, into = c("scenario", "Rt"), sep = "_") %>% 
  filter(scenario == "S5") %>% View()

sur_sen %>% 
  bind_rows() %>% 
  separate(lab, into = c("scenario", "Rt"), sep = "_") %>% 
  filter(scenario == "S5",
         Rt == 2) 

sur_sen %>% 
  bind_rows() %>% 
  separate(lab, into = c("scenario", "Rt"), sep = "_") %>% 
  filter(Rt == 2) %>% 
  dplyr::select(scenario, name, p) %>% 
  group_by(name) %>% 
  summarise(minimum = min(p),
            maximum = max(p)) 
  
