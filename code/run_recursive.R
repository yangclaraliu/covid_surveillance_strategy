# @title: run_recursive.r
# @objective: recursive algorithm that looks for the optimal strategy
# @author: Yang Liu, Sam Clifford

# define maximum capacity
n_count
n_cap <- (round(n_count/200) + 1) %>% 
  as_tibble 
n_cap
n_cap[5:6] <- ceiling(0.1*n_cap[5:6])
n_cap[4] <- 2
n_cap

pilot = 41 # baseline
# initial conditions
if (file.exists("code/results/recursive_dependent.rdata")){
  load("code/results/recursive_dependent.rdata")
} else {
  r <- vector("list", length = t_max)
  b <- vector("list", length = t_max)
  
  pb <- progress::progress_bar$new(
    format = "Search grids: [:bar] :current/:total (:percent in :elapsed) eta: :eta",
    total = t_max-1,
    width = 80)
  
  for(t in 2:t_max){
    s_0 <- rep(1,ncol(search_grid_sensitivity_dep[[pilot]][[t]]))
    s_0[which.min(search_grid_sensitivity_dep[[pilot]][[t]][2,])] <- 2
    r[[t]] <- list()
    b[[t]] <- list()
    # move
    s <- s_0
    repeat {
      f <- sweep(expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),
                             c(0,1),c(0,1),c(0,1),c(0,1),
                             c(0,1)) %>% 
                   mutate(add = rowSums(.)) %>% 
                   filter(add == 1) %>% # incremental
                   .[,1:9], 2, s, "+") %>% 
        mutate(all = rowSums(.)) %>% 
        filter(all <= round((4.15/1000)*pop_tmp$tot/200) + length(s_0))  # maximum testing capacity
      i <- f$all %>% unique - length(s_0)
      f <- f %>% 
        dplyr::select(-all) %>%
        as.matrix()
      m <- which(sweep(f, 2, n_cap, "<=") %>% rowSums() < ncol(f))
      if(length(m)>0) f <- f[-m,]
      if(nrow(f) == 0) {
        break
      } else {
        f_r <- apply(sapply(1:nrow(f), function(x) {
          search_grid_sensitivity_dep[[pilot]][[t]][array(c(f[x,], (1:9)) %>% unlist,
                                 dim = c(9,2))]
        }), 2, prod)
        s <- f[which.min(f_r),] %>% array
        r[[t]][[i]] <- s
        b[[t]][[i]] <- min(f_r)
      }
    }
    pb$tick()
  }
  
  lapply(2:length(r), function(x) do.call("rbind",r[[x]])) %>% 
    map(as.data.frame) %>% 
    bind_rows(.id = "t") %>% 
    mutate_all(as.numeric) %>% 
    mutate(resource = rowSums(.) - t - length(s_0)) %>% 
    group_by(t) %>% 
    group_split() -> allocation
  
  length(allocation)
  
  lapply(1:length(allocation), function(t){
    c(rep(1,2), 
      sapply(1:(allocation[[t]] %>% nrow - 1), 
             function(x) which((allocation[[t]][x,paste0("V",1:length(s_0))] == allocation[[t]][x+1,paste0("V",1:length(s_0))]) == F))) 
  }) -> r_unfold
save(r, b, r_unfold, file = "code/results/recursive_dependent.rdata")
}

b_to_plot <- b %>% 
  map(unlist) %>% 
  do.call("cbind",.) %>% 
  t %>% 
  data.frame() %>% 
  rowid_to_column(var = "t") %>% 
  pivot_longer(cols = starts_with("X"),
               names_to = "r") %>% 
  mutate(r = parse_number(r)*200) %>% 
  #filter(t <= 30) %>% 
  left_join(incidence_sensitivity[[pilot]], by = "t")

b_ends <- b_to_plot %>%
  filter(r %in% range(r), inc > 0 , inc < 40000) %>%
  group_by(inc) %>%
  mutate(r_ = ifelse(inc > 1e3, min(r), max(r))) %>%
  filter(r == r_)  %>%
  ungroup

b_to_plot %>% 
  pivot_wider(values_from = value,
              names_from = r) %>% 
  right_join(data.frame(inc = seq(0,10000,1)), by = "inc") %>% 
  mutate_at(vars(as.character(b_to_plot$r)), imputeTS::na_interpolation) %>% 
  pivot_longer(cols = as.character(b_to_plot$r)) %>% 
  filter(inc %in% c(10, 100, 1000, 10000)) %>% 
  rename(r = name) %>% 
  mutate(r = parse_number(r)) -> b_to_plot_filtered
 
b_ends_filtered <- b_to_plot_filtered %>%
  filter(r %in% range(r), inc > 0 , inc < 40000) %>%
  group_by(inc) %>%
  mutate(r_ = ifelse(inc > 1e3, min(r), max(r))) %>%
  filter(r == r_)  %>%
  ungroup


b_to_plot_filtered %>%
  mutate(inc = factor(inc)) %>%
  group_by(inc) -> b_to_plot_filtered_inc

b_to_plot_filtered_inc %>%
  distinct(inc) %>%
  ungroup %>%
  mutate(r = 1:n(),
         q = seq(0,1,length.out = n() + 2)[r + 1]) %>%
  rowwise %>%
  group_split() %>%
  map_df(~filter(b_to_plot_filtered_inc,
              inc == .x$inc, value >= .x$q) %>%
        head(1)) %>%
  dplyr::select(inc, value, r) -> label_pos

efficiency_frontier_v3 <- b_to_plot_filtered %>% 
  dplyr::select(inc, value, r) %>%
  mutate(inc = factor(inc),
         r = r*1000/pop_tmp$tot) %>% 
  ggplot(., aes(color = inc,
                y = 1-value,
                group = inc,
                x = r))+
  geom_line(size = 2) +
  scale_color_manual(values = RColorBrewer::brewer.pal(n=5, "YlGnBu")[-c(1)],
                     labels = c("10","100","1K","10K")) +
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x = "Daily Testing Rate (unit: t/k/day)",
       y = "Probability of COVID-19 Detection",
       color = "Cumulative Incidence")+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size = 3)))

efficiency_frontier_v3
# ggsave(plot = efficiency_frontier_v3,
#        filename = "figs/efficiency_frontier_v4.png",
#        height = 7,
#        width = 10)

r %>% 
  .[-1] %>% 
  map(unlist) %>% 
  map(matrix, ncol = 9, byrow = T) %>% 
  map(data.frame) %>% 
  map(rownames_to_column, var = "r") %>% 
  map(mutate, r = as.numeric(r)+1) %>% 
  map(bind_rows, 
      c(1,2,rep(1,8)) %>% setNames(c("r",paste0("X",1:9)))) %>% 
  map(arrange, r) %>% 
  bind_rows(.id = "t") %>% 
  setNames(c("t","r",colnames(n_count))) %>%
  mutate_at(.vars = colnames(n_count),
            .funs = function(x){x-1}) %>% 
  mutate(all = fc_patient+rd_patient+od_patient+
           fc_hcw+rd_hcw+od_hcw+
           child+adult+elderly,
         r = r*200) %>% 
  filter(r <= ceiling((4.15/1000)*pop_tmp$tot/200)*200) %>% 
  mutate_at(vars(colnames(n_count)), funs(./all)) %>% 
  pivot_longer(cols = colnames(n_count)) %>% 
  mutate(t = parse_number(t)) %>% 
  left_join(., incidence[,c("t","inc")], by = "t") %>% 
  filter(inc <= 10000) %>% 
  mutate(category = cut(inc, breaks = c(0, 10^seq(2,4,by=1), Inf),
                        include.lowest = FALSE),
         tags = factor(name,
                       levels = names(r_levs),
                       labels = r_levs),
         tags = factor(tags, levels = names(r_palette), ordered = T)) %>%
  filter(!is.na(category)) -> r_to_plot

r_categories <- r_to_plot %>% distinct(category) %>%
  mutate(category2 = relabel_categories(category, label = "Cumulative incidence"))

r_to_plot <- left_join(r_to_plot, r_categories, by = "category")  %>%
  left_join(r_groups, by = "tags")

r_plot <- r_to_plot %>% 
  group_by(r, category2, tags, name, group, group2) %>%
  summarise(value = sum(value)) %>%
  group_by(r, category2) %>%
  mutate(p = value/sum(value)) %>%
  ungroup %>% 
  mutate(r = r*1000/pop_tmp$tot) %>% 
  ggplot(., aes(x = r,
                y = p,
                fill = tags))+
  geom_area(position = position_stack(reverse = T))+
  theme_bw()+
  facet_wrap(~category2, ncol =1 ) +
  scale_fill_manual(values = r_palette, name="",
                    breaks = c("Fever Clinic - Patients",
                               "Respiratory Dept - Patients",
                               "Other Dept - Patients",
                               
                               "Fever Clinic - HCW",
                               "Respiratory Dept - HCW",
                               "Other Dept - HCW",
                               
                               "Community - Children",
                               "Community - Younger Adults",
                               "Community - Older Adults"))  +
  labs(x = "Daily Testing Rate (unit: t/k/day)",
       y = "Proportion Tests Allocated",
       color = "") +
  theme(axis.text        = element_text(size = 20),
        axis.title       = element_text(size = 20),
        legend.text      = element_text(size = 14),
        legend.title     = element_text(size = 20),
        strip.text       = element_text(size = 20),
        strip.background = element_rect(NA),
        panel.grid       = element_blank(),
        legend.position  = "bottom") +
  guides(fill = guide_legend(ncol = 3, byrow=F))

r_plot

# ggsave(plot = r_plot,
#        "figs/strategies_selected_v3.png",
#        width = 10,
#        height = 10)

r_unfold %>% 
  do.call("rbind",.) %>% 
  data.frame %>% 
  rownames_to_column() %>% 
  pivot_longer(cols = starts_with("X")) %>% 
  mutate(name = parse_number(name),
         rowname = as.numeric(rowname)) %>% 
  rename(r = name,
         t = rowname) %>% 
  left_join(incidence_sensitivity[[41]], by = "t") %>%
  mutate(value = factor(value, 
                        levels = 1:9,
                        labels = c("Fever Clinic - Patients",
                                   "Respiratory Dept - Patients",
                                   "Other Dept - Patients",
                                   
                                   "Fever Clinic - HCW",
                                   "Respiratory Dept - HCW",
                                   "Other Dept - HCW",
                                   
                                   "Community - Children",
                                   "Community - Younger Adults",
                                   "Community - Older Adults")),
         cat = cut(inc, breaks = c(0, 10^seq(2,4,by=1)),
                   include.lowest = FALSE),
         r = r*200) %>% 
  filter(!is.na(cat)) %>% 
  ggplot(., aes(x = r,
                y = cat,
                color = value))+
  geom_jitter()+
  ggsci::scale_color_lancet(drop = F)+
  theme_cowplot()+
  labs(y = " ",
       x = "Daily Testing Capacity") + 
  scale_x_continuous(breaks = 1000*seq(0,80,by=20),
                     labels = label_K) +
  theme(axis.text        = element_text(size = 20),
        axis.title       = element_text(size = 20),
        legend.text      = element_text(size = 14),
        legend.title     = element_text(size = 20),
        strip.text       = element_text(size = 20),
        strip.background = element_rect(NA),
        panel.grid       = element_blank()) +
  guides(fill = guide_legend(nrow = 3, byrow=T))

# ggsave("figs/strategies_selected_dot.png",
#        width = 20,
#        height = 10)
