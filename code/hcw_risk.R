# @title: risk_hcw.r
# @objective: estimate the representativeness of healthcare worker compare to baseline
# @author: Yang Liu
# @created: 5/5/2020
# @created: 15/6/2020

library(fitdistrplus)
# order of settings: house, other work
# order of population: adult, hcw

"data/contact" %>% 
  list.files(pattern = "SG") %>% 
  paste0("data/contact/",.) %>% 
  map(read.table, sep = ",") %>% 
  setNames("data/contact" %>% list.files(pattern = "SG")) %>% 
  #map(mutate, V1 = parse_number(as.character(V1))) %>% 
  bind_rows(., .id = "filename") %>%
  mutate(V1 = round(V1, 2)) %>% 
  separate(filename, sep = "_", into = c("country",
                                         "population",
                                         "setting")) %>% 
  mutate(setting = gsub(".txt", "", setting),
         V2 = V2/100) %>% 
  group_by(country, population, setting) %>%
  nest() -> import

nn <- 10000

# fix x axis
import
import$data[[1]]$V1 <- c(0:10) #adult, household
import$data[[2]]$V1 <- c(0:10) #adult, other
import$data[[3]]$V1 <- c(1:20) #adult, work
import$data[[4]]$V1 <- c(0:9, 12,14) # hcw, household
import$data[[5]]$V1 <- c(0:11) # hcw, other
import$data[[6]]$V1 <- floor(import$data[[6]]$V1) # hcw, work

# generate distributions
n <- 10000
import <- mutate(import, 
                 samples = map(.x =  data,
                               .f = ~rep(x     = .x$V1,
                                         times = round(.x$V2*n))))

# fit workplace distribution
dist <- "pois"
sapply(1:6, function(x) fitdist(data = import$samples[x][[1]],distr =dist)$estimate) -> lambda

n_settings <- c("house", "other", "work")
n_community <- c("adult", "hcw")

names(lambda) <- expand.grid(n_settings, n_community) %>%
  mutate(V3 = paste(Var2, Var1,sep = "-")) %>%
  pull(V3)


# alternative way of calculating the above
import <- mutate(import,
                 lambda = map_dbl(.x =  samples,
                                  .f = ~fitdist(.x, distr = dist)$estimate)) %>%
  tidyr::unite("label", population, setting, remove=FALSE)

import %>%
  ungroup %>%
  dplyr::select(setting, population, lambda) %>%
  tidyr::spread(population, lambda) %>%
  mutate(approx_1 = 1 - (adult/hcw)/(1 - exp(-adult)))

# approximation 1
1 - (lambda[3]/lambda[6])/(1-exp(-lambda[3]))

# approximation 2
1 - cbind(rpois(n, lambda[3]),
          rpois(n, lambda[6])) %>% 
  {mean(.[,1]/.[,2])} -> prop_patient
prop_patient

# compare healthcare workers and general public
lapply(1:3, function(x) sample(x = import$samples[[x]], size = 1000, replace = T)) %>% 
  do.call("cbind", .) %>% 
  as_tibble() %>% 
  setNames(c("house", "other", "work")) %>% 
  mutate(all = house + other + work,
         population = "adult") -> adult_community

lapply(4:6, function(x) sample(x = import$samples[[x]], size = 1000, replace = T)) %>% 
  do.call("cbind", .) %>% 
  as_tibble() %>% 
  setNames(c("house", "other", "work")) %>% 
  mutate(all = house + other + work,
         population = "hcw",
         patient = work*0.65,
         non_patient = work*0.35 + house +other) -> hcw_community

#### contacts by hcw ####
#### @fever clinic ####
t_max = 100
c_e_patient <- vector("list", 3)

pb <- progress::progress_bar$new(
  format = "Sampling Fever Clinic: [:bar] :current/:total (:percent in :elapsed) eta: :eta",
  total = nrow(hcw_community)*t_max,
  width = 80)


lapply(1:nrow(hcw_community), function(p){
  lapply(1:t_max, function(t){
    ans <- rhyper(nn = nn,
                  m = (param_outbreak$Io[t]) %>% round,
                  n = (param_outbreak$n_denom_fc[t] -
                         (param_outbreak$Io[t])) %>% round,
                  k = round(hcw_community$patient[p]))# number of contacts with patients
    pb$tick()
    return(ans)
  }) %>% 
    do.call("rbind",.) %>% 
    t %>% 
    colMeans()
}) %>% 
  do.call("rbind",.) -> c_e_patient[[1]]

#### @ respiratory department ####
pb <- progress::progress_bar$new(
  format = "Sampling Resp. Dept.: [:bar] :current/:total (:percent in :elapsed) eta: :eta",
  total = nrow(hcw_community)*t_max,
  width = 80)


lapply(1:nrow(hcw_community), function(p){
  lapply(1:t_max, function(t){
    ans <- rhyper(nn = nn,
                  m = (param_outbreak$Io[t]) %>% round,
                  n = (param_outbreak$n_denom_rd[t] -
                         (param_outbreak$Io[t])) %>% round,
                  k = round(hcw_community$patient[p]))# number of contacts with patients
    pb$tick()
    return(ans)
  }) %>% 
    do.call("rbind",.) %>% 
    t %>% 
    colMeans()
}) %>% 
  do.call("rbind",.) -> c_e_patient[[2]]

#### @ other hospital departments ####
pb <- progress::progress_bar$new(
  format = "Sampling Other Depts.: [:bar] :current/:total (:percent in :elapsed) eta: :eta",
  total = nrow(hcw_community)*t_max,
  width = 80)

lapply(1:nrow(hcw_community), function(p){
  lapply(1:t_max, function(t){
    ans <- rhyper(nn = nn,
                  m = round(param_outbreak$Ip[t] + 
                              param_outbreak$Iu[t]),
                  n = round(param_outbreak$n_denom_baseline[t] - 
                              (param_outbreak$Ip[t] + 
                                 param_outbreak$Iu[t])),
                  k = round(hcw_community$patient[p])) # number of contacts with patients
    pb$tick()
    return(ans)
  }) %>% 
    do.call("rbind",.) %>% 
    t %>% 
    colMeans()
}) %>% 
  do.call("rbind",.) -> c_e_patient[[3]]

#### contacts with non-patient, including household and other ####
pb <- progress::progress_bar$new(
  format = "Sampling Non-patients: [:bar] :current/:total (:percent in :elapsed) eta: :eta",
  total = nrow(hcw_community)*t_max,
  width = 80)

lapply(1:nrow(hcw_community), function(p){
  lapply(1:t_max, function(t){
    ans <- rhyper(nn = nn,
                  m = round(param_outbreak$I[t]),
                  n = round(param_outbreak$n_denom_baseline[t] -
                              param_outbreak$I[t]),
                  k = round(hcw_community$non_patient[p]))
    pb$tick()
    return(ans)
  })  %>% 
    do.call("rbind",.) %>% 
    t %>% 
    colMeans()
}) %>% 
  do.call("rbind",.) -> c_e_nonpatient

# contacts by adults
pb <- progress::progress_bar$new(
  format = "Sampling c_e_adults: [:bar] :current/:total (:percent in :elapsed) eta: :eta",
  total = nrow(adult_community)*t_max,
  width = 80)

lapply(1:nrow(adult_community), function(p){
  lapply(1:t_max, function(t){
    ans <- rhyper(nn = nn,
                  m = round(param_outbreak$I[t]),
                  n = round(param_outbreak$n_denom_baseline[t] -
                              param_outbreak$I[t]),
                  k = round(adult_community$all[p]))
    pb$tick()
    return(ans)
  })  %>% 
    do.call("rbind",.) %>% 
    t %>% 
    colMeans()
}) %>% 
  do.call("rbind",.) -> c_e_adults

c_e_RR <- purrr::map(.x =  c_e_patient,
                     .f = ~colMeans(.x + c_e_nonpatient)/(colMeans(c_e_adults)))

c_e_RR %>% 
  do.call("cbind", .) %>% 
  as_tibble %>% 
  setNames(c("fc_hcw",
             "rd_hcw",
             "od_hcw")) %>% 
  rownames_to_column(var = "t") %>% 
  mutate(t = as.numeric(t)) %>% 
  left_join(incidence[,c("t","inc")], by = "t") -> hcw_RR

write_rds(hcw_RR, "code/results/hcw_RR.rds")

#  hcw_RR <- read_rds("code/results/hcw_RR.rds")

cbind(colMeans(c_e_patient[[1]] + c_e_nonpatient),
      colMeans(c_e_patient[[2]] + c_e_nonpatient),
      colMeans(c_e_patient[[3]] + c_e_nonpatient),
      colMeans(c_e_adults)) %>% 
  as_tibble %>% 
  setNames(c("fc_hcw",
             "rd_hcw",
             "od_hcw",
             "adult")) %>% 
  rownames_to_column(var = "t") %>% 
  gather(name, value, -t) %>%
  mutate(name = factor(name,
                       levels = names(r_levs),
                       labels = r_levs),
         name = factor(name, levels = names(r_palette), ordered = T),
         name = fct_drop(f = name),
         t = as.numeric(t)) %>% 
  left_join(dplyr::filter(incidence,Rt == 2)[,c("t","inc")], by = "t") -> 
  expected_encounter_to_plot

expected_encounter <- 
  ggplot(expected_encounter_to_plot %>% filter(inc <= 100000), aes(x = inc,
                                                                   y = value,
                                                                   group = name,
                                                                   color = name))+
  geom_line(size = 1) +
  scale_color_manual(values = r_palette) +
  labs(t) +
  labs(x = "Cumulative Incidence in the community",
       y = "Expected Number of Infectious Contacts",
       color = "Surveillance Subgroups") +
  theme_bw() +
  #facet_wrap(~t) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  annotation_logticks(sides = "bl") +
  guides(color = guide_legend(ncol = 2, byrow=F, title.position = "top")) +
  scale_x_log10(labels = scientific_10) +
  scale_y_log10(labels = scientific_10) +
  coord_equal()

ggsave("figs/expected_encounter.png",
       plot = expected_encounter,
       width = 7,
       height = 7)


hcw_RR %>%  
  group_by(t) %>%
  mutate(tt = 1:n()) %>%
  filter(tt == median(tt)) %>%
  select(-tt) %>%
  gather(name, value, -t, -inc) %>%
  mutate(name = factor(name,
                       levels = names(r_levs),
                       labels = r_levs),
         name = factor(name, levels = names(r_palette), ordered = T),
         name = fct_drop(f = name),
         t = as.numeric(t)) -> relative_encounter_to_plot

relative_encounter <- 
  relative_encounter_to_plot %>% 
  filter(inc > 1,
         inc <= 100000) %>%
  ggplot(., aes(x = inc,
                y = value,
                group = name,
                color = name))+
  #geom_point() +
  geom_line(size = 1) +
  #geom_smooth(method = "gam", se=F, formula = y ~ s(x)) +
  geom_hline(yintercept = 1) +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  annotation_logticks(sides = "bl") +
  scale_color_manual(values = r_palette) +
  guides(color = guide_legend(ncol = 1, byrow=F, title.position = "top")) +
  scale_x_log10(labels = scientific_10) +
  scale_y_log10() +
  #labs(t) +
  #scale_x_log10() +
  labs(x = "Cumulative Incidence",
       y = "Probability Ratio",
       color = "Surveillance Subgroups")

ggsave("figs/relative_encounter.png",
       plot = relative_encounter,
       width = 7,
       height = 7)


