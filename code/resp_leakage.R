# @title: respiratory_leakage.r
# @objective: assessing the leakage due to not testing everyone at respiratory/ fever clinics
# @author: Yang Liu
# @created: 11/5/2020
# @created:1/6/2020

sensitivity_table[5,]
sensitivity_table %>% filter(dinf_u == 6, dincu == 5, theta == 0.14)
sensitivity_table[5,]
#baseline
which(all_scenarios == "S5_2.7")

pilot = 41
gen_leak(grid_tmp = search_grid_sensitivity[[pilot]],
         inc_tmp = incidence_sensitivity[[pilot]]) -> leak

leak_sensitivity <- list()
for(i in 1:length(search_grid_sensitivity)){
  leak_sensitivity[[i]] <- gen_leak(grid_tmp = search_grid_sensitivity[[i]],
                                    inc_tmp = incidence_sensitivity[[i]]) 
  print(paste0("Working on ", i, " of ", length(search_grid_sensitivity)," items."))
}
n_test <- seq(0,pop_tmp$tot*4.15/1000,200)

# leak$prod_fc <-
# leak$prod_rd <- 1-sapply(1:t_max, function(t) prod(leak$`Respiratory Departments - Patients`[1:t]

if(file.exists("code/results/resp_leakage_res.rdata")){
  load("code/results/resp_leakage_res.rdata")
} else {
  lapply(1:length(n_test), function(n){
    sapply(1:(nrow(hcw_RR)-1), function(t) {
      prod(leak[leak$r == n_test[n],]$`Fever Clinics - Patients`[1:t])
    })
  }) -> cum_fc
  
  cum_fc_sensitivity <- list()
  for(i in 1:length(search_grid_sensitivity)){
    lapply(1:length(n_test), function(n){
      sapply(1:(nrow(hcw_RR)-1), function(t) {
        prod(leak_sensitivity[[i]][leak_sensitivity[[i]]$r == n_test[n],]$`Fever Clinics - Patients`[1:t])
      })
    }) -> cum_fc_sensitivity[[i]]
    print(paste0("Working on ", i, " of ", length(search_grid_sensitivity)," items."))
  }
  
  lapply(1:length(n_test), function(n){
    sapply(1:(nrow(hcw_RR)-1), function(t) {
      prod(leak[leak$r == n_test[n],]$`Respiratory Departments - Patients`[1:t])
    })
  }) -> cum_rd                                    
  
  cum_rd_sensitivity <- list()
  for(i in 1:length(search_grid_sensitivity)){
    lapply(1:length(n_test), function(n){
      sapply(1:(nrow(hcw_RR)-1), function(t) {
        prod(leak_sensitivity[[i]][leak_sensitivity[[i]]$r == n_test[n],]$`Respiratory Departments - Patients`[1:t])
      })
    }) -> cum_rd_sensitivity[[i]]
    print(paste0("Working on ", i, " of ", length(search_grid_sensitivity)," items."))
  }
  save(cum_fc, cum_rd,
       cum_fc_sensitivity,cum_rd_sensitivity,
       file = "code/results/resp_leakage_res.rdata")
}

source("code/fig3_annex.R")

for(i in 1:length(cum_fc_sensitivity)){
  cum_fc_sensitivity[[i]] %<>% 
    do.call("rbind",.) %>% 
    as_tibble %>% 
    drop_na() %>% 
    rownames_to_column(var = "r") %>% 
    mutate(r = (as.numeric(r)-1)*200) 
}

for(i in 1:length(cum_rd_sensitivity)){
  cum_rd_sensitivity[[i]] %<>% 
    do.call("rbind",.) %>% 
    as_tibble %>% 
    drop_na() %>% 
    rownames_to_column(var = "r") %>% 
    mutate(r = (as.numeric(r)-1)*200) 
}

cum_fc %<>% 
  do.call("rbind",.) %>% 
  as_tibble %>% 
  drop_na() %>% 
  rownames_to_column(var = "r") %>% 
  mutate(r = (as.numeric(r)-1)*200) 

cum_rd %<>% 
  do.call("rbind",.) %>% 
  as_tibble %>% 
  drop_na() %>% 
  rownames_to_column(var = "r") %>% 
  mutate(r = (as.numeric(r)-1)*200) 

cum_fc %>% filter(r == n_count$fc_patient) -> fc_reference

sapply(2:ncol(cum_rd), function(x) {
  abs(cum_rd[x,2:ncol(cum_rd)] - fc_reference[2:ncol(cum_fc)]) %>% sum
}) %>% 
  which.min %>% 
  cum_rd[.,"r"] -> matched

cum_rd %>% 
  filter(r %in% c(ceiling((n_count$fc_patient*0.5)/200)*200, 
                  n_count$fc_patient, 
                  matched, 
                  n_count$rd_patient)) %>% 
  mutate(name = "rd") %>% 
  bind_rows(cum_fc %>% 
              filter(r %in% c(ceiling((n_count$fc_patient*0.5)/200)*200, 
                              n_count$fc_patient)) %>% 
              mutate(name = "fc")) %>% 
  # mutate(r = factor(r,
  #                      levels = c(1000, 2000, matched, 36400),
  #                      labels = c("1K", "2K", paste0(round(matched/1000,1), "K"), "36.4K"))) %>%
  pivot_longer(cols = starts_with("V"),
               names_to = "t") %>% 
  mutate(t = parse_number(t),
         name = factor(name,
                       levels = c("fc","rd"),
                       labels = c("Fever Clinic - Patients",
                                  "Respiratory Dept - Patients")),
         value = 1-value) %>% 
  left_join(incidence_sensitivity[[pilot]][,c("t","inc")], by = "t") -> ss_to_plot

r_groups <- data.frame(tags = r_levs) %>%
  rownames_to_column() %>%
  separate(., tags, into = c("group", "group2"), sep = "-", remove = F) %>%
  mutate(tags = factor(tags, levels = names(r_palette), ordered = T))

scarce_surveillance <- ss_to_plot %>%
  filter(inc <= 15000, inc >= 1) %>%
  ggplot(., aes(x = inc,
                y = value,
                color = name,
                group = r)) +
  geom_line(size = 2) +
  facet_wrap(~name)+
  labs(x = expression(Cumulative~incidence~`in`~community~on~day~t~(C[t])),
       y = expression(Probability~of~Detection~on~day~t~(pi[t]))) +
  annotation_logticks(sides = "b") +
  scale_color_manual(values = r_palette) +
  theme_bw()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        panel.grid = element_blank()) +
  scale_x_continuous(labels = label_K, trans = "log10") +
  geom_label(data = ss_to_plot %>%
               mutate(r = round((r/pop_tmp$tot)*1e3,2)) %>%
               group_by(r, name) %>%
               mutate(diff = abs(value - 0.5)) %>%
               filter(diff <= 0.2) %>%
               filter(diff == max(diff)),
             #filter(inc > 1000) %>%
             #filter(inc == min(inc)),
             aes(label = round(r,2)),
             size = 4,
             nudge_y = 0,
             nudge_x = 0.01)

color_strip <- function(plot){
  g <- ggplot_gtable(ggplot_build(plot))
  stripr <- which(grepl('strip-t', g$layout$name))
  k <- 1
  for (i in stripr) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    lab <- g$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$label %>%
      sub(x = ., pattern = "Departments", replacement = "Dept") %>%
      sub(x = ., pattern = "Fever Clinics", replacement = "Fever Clinic") %>%
      sub(x = ., pattern = "Adults", replacement = "Younger Adults") %>%
      sub(x = ., pattern = "Elderly", replacement = "Older Adults")
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <-
      r_palette[lab]
    g$grobs[[i]]$grobs[[1]]$children[[2]]$children[[1]]$gp$col <-
      ifelse(lab == "Community - Older Adults", "white", "grey10")
    k <- k+1
  }
  return(g)
  # return(grid::grid.draw(g))
}

ggarrange(color_strip(scarce_surveillance),
          color_strip(scarce_surveillance_part2),
          ncol = 1) -> p_tosave

# ggsave(filename = "figs/scarce_surveillance.pdf",
#        useDingbat = FALSE,
#        height = 10,
#        width = 16)

ggsave(filename = "figs/scarce_surveillance_v4.png",
       plot = p_tosave,
       height = 10,
       width = 10)

# which(cum_fc %>% 
#   filter(r == 1600) %>% 
#   dplyr::select(-r) %>% 
#   t<0.5) %>% min
# 
# which(cum_rd %>% 
#         filter(r == 1600) %>% 
#         dplyr::select(-r) %>% 
#         t<0.5) %>% min

lapply(1:length(cum_rd_sensitivity), function(x){
  cum_rd_sensitivity[[x]] %>%
    filter(r == n_count$rd_patient) %>%
    pivot_longer(cols = starts_with("V")) %>%
    mutate(t = parse_number(name)+1) %>%
    left_join(incidence_sensitivity[[x]][,c("t","inc")], by = "t") %>%
    mutate(v = 100*(1-value)) %>% 
    arrange(t) %>% 
    right_join(data.frame(inc = seq(0,500,1)), by = "inc") %>%
    mutate(value = imputeTS::na_interpolation(value),
           v = imputeTS::na_interpolation(v)) %>% 
    filter(inc %in% c(10,50,100))
}) %>% 
  bind_rows() %>% 
  group_by(inc) %>% 
  summarise(minimum = min(v),
            maximum = max(v))

cum_rd %>% 
  filter(r == n_count$rd_patient) %>% 
  pivot_longer(cols = starts_with("V")) %>%
  mutate(t = parse_number(name)+1) %>%
  left_join(incidence_sensitivity[[41]][,c("t","inc")], by = "t") %>% 
  mutate(v = 100*(1-value)) %>% 
  arrange(t) %>% 
  right_join(data.frame(inc = seq(0,500,1)), by = "inc") %>% 
  mutate(value = imputeTS::na_interpolation(value),
         v = imputeTS::na_interpolation(v)) %>%View()
  filter(inc %in% c(10,50,100))
