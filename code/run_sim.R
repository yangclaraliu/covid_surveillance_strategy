# @title: run_sim.r
# @objective: run simulation. parameters can be found here as well.
# @author: Yang Liu, Petra Klepac

# basic set up of model
# Ip - pre-clinical
# Io - clinical
# Iu - subclinical
# Ro_pos - PCR+ Removed following Io
# Ru_pos - PCR+ Removed following Iu

stages <- c("S", "E", "Ip","Io", "Iu", "Ro_pos", "Ru_pos","R_neg")
nepi <- length(stages) # S E Ip Io Iu Ro_pos Ru_pos R_neg
age_groups <- paste(seq(0, 75, 5),seq(0, 75, 5)+4, sep="-"); age_groups[16] <- "75+"
nage <- length(age_groups) # 16 age groups - 16*16 contact matrices
# parameters
source('code/utils/read_data.R')
params = data.table(dlat = 2.6, # length of latent period
                    dprogress = 2.4, # length of incubation period in days 
                    dinf = 3.6,      # length of clinically infectious period in days,
                    dinf_u = 6, # length of subclinical infectious period in days
                    dneg = 6,
                    R0 = 2)
source("code/utils/get_sen.R")

#load data and functions

paste0("./code/utils/", 
       list.files(path = "./code/utils/",
                  pattern = "*\\.(R|r)")) %>%
  grep(pattern = "(read).*", invert = T, value=T) %>%
  map(source)

params[, `:=` (p.latent = 1 - exp(-1/dlat),
               p.progress = 1 - exp(-1/dprogress),
               p.recover = 1 - exp(-1/dinf),
               p.recover_u = 1-exp(-1/dinf_u),
               p.neg = 1 - exp(-1/dneg),
               theta = 0.14, # transmissibility of subclinicals
               rho = list(cfrac_age))] # probability of clinical

t0 <- 1
tmax <- 300

expand.grid(dinf_u = c(5,6,8),
            dincu = c(4,5,6),
            theta = c(0.14, 0.5)) %>% 
  mutate(dprogress = dinf_u*0.4,
         dinf = dinf_u-dprogress,
         dneg = 10-dinf_u,
         R0 = 2,
         dlat = dincu - dprogress,
         p.latent = 1 - exp(-1/dlat),
         p.progress = 1 - exp(-1/dprogress),
         p.recover = 1 - exp(-1/dinf),
         p.recover_u = 1-exp(-1/dinf_u),
         p.neg = 1 - exp(-1/dneg),
         rho = list(cfrac_age)) %>% 
  data.table -> sensitivity_table

R0 <- 2.7
R0sample <- c(1.4, 2, 2.7)
nruns <- length(R0sample)

m <- array(0, dim = c(nepi, nage))
m[1,] <- rep(1, nage)
seed_agegroup <- 6 #sample
print(paste("The infected individual is from the", age_groups[seed_agegroup], "age group."))
#e0 <- 1e-06
m[1,seed_agegroup] <- m[1,seed_agegroup] - 1/age_dist_tmp[seed_agegroup]
m[2,seed_agegroup] <- 1/age_dist_tmp[seed_agegroup]
n0 <- vec(m)
nout <- array(NA_real_, dim = c(length(n0), tmax, nruns))

source("code/utils/run_epi.R")
if(file.exists("code/results/run_sim_6.rdata")){
  load("code/results/run_sim_6.rdata")
} else{
  run_epi(n0     = n0,
          C      = contact$all,
          p      = params,
          runs   = nruns,
          R0post = R0sample,
          tmax   = tmax,
          t0     = t0) -> sim
  
  sim_sensitivity <- vector("list", nrow(sensitivity_table))
  
  for(i in 1:nrow(sensitivity_table)){
    print(sprintf("Sensitivity analysis: running %d of %d", i, length(sim_sensitivity)))
    run_epi(n0     = n0,
            C      = contact$all,
            p      = sensitivity_table[i],
            runs   = nruns,
            R0post = R0sample,
            tmax   = tmax,
            t0     = t0) -> sim_sensitivity[[i]]
  }
}

c("Ip", "Io", "Iu", "S", "E", "Ro_pos", "Ru_pos", "R_neg") %>%
  {purrr::set_names(as.list(.), .)} %>%
  map_df(~get_comp(.x, sim),
         .id = "comp") %>%
  mutate(comp = forcats::fct_inorder(comp)) -> comps_to_plot

lapply(1:nrow(sensitivity_table), function(x) {
  c("Ip", "Io", "Iu", "S", "E", "Ro_pos", "Ru_pos", "R_neg") %>%
    {purrr::set_names(as.list(.), .)} %>%
    map_df(~get_comp(.x, sim_sensitivity[[x]]),
           .id = "comp") %>%
    mutate(comp = forcats::fct_inorder(comp))
}) -> comps_sensitivity

c("Ip", "Io", "Iu", "Ro_pos", "Ru_pos") %>%
  {purrr::set_names(as.list(.), .)} %>%
  map_df(~get_comp(.x, sim, byAge = T),
         .id = "comp") %>%
  mutate(comp = forcats::fct_inorder(comp)) -> comps_to_plot_age

lapply(1:nrow(sensitivity_table), function(x){
  c("S","E","Ip", "Io", "Iu", "Ro_pos", "Ru_pos","R_neg") %>%
    {purrr::set_names(as.list(.), .)} %>%
    map_df(~get_comp(.x, sim_sensitivity[[x]], byAge = T),
           .id = "comp") %>%
    mutate(comp = forcats::fct_inorder(comp))
}) -> comps_age_sensitivity

comps_to_plot %>%
  pivot_wider(names_from = comp,
              values_from = value) %>%
  mutate(I = Ip + Io + Iu,
         R = Ro_pos + Ru_pos) %>%
  #  mutate_at(vars(starts_with("I")), function(x) x/pop_tmp$tot) %>%
  group_by(t, Rt) -> prevalence

comps_sensitivity %>%
  map(pivot_wider, names_from = comp, values_from = value) %>%
  map(mutate, I = Ip + Io + Iu) %>%
  map(mutate, R = Ro_pos + Ru_pos) %>%
  map(group_by, t, Rt) %>%
  bind_rows(.id = "tag") %>%
  mutate(lab = paste0("S",tag,"_",Rt)) %>%
  group_by(lab) %>%
  group_split() -> prevalence_sensitivity

prevalence_sensitivity %>%
  setNames(prevalence_sensitivity %>%
             map(pull, lab) %>%
             map(unique) %>%
             unlist()) -> prevalence_sensitivity

comps_age_sensitivity %>% map(get_prev_age) -> prevalence_age_sensitivity

prevalence_age_sensitivity %>%
  map(group_by, t, Rt) %>%
  bind_rows(.id = "tag") %>%
  mutate(lab = paste0("S",tag,"_",Rt)) %>%
  group_by(lab) %>%
  group_split() -> prevalence_age_sensitivity

get_prev_age(comps_to_plot_age) -> prevalence_age

prevalence %>%
  dplyr::select(t, Rt, S) %>%
  mutate(inc = round(pop_tmp$tot - S) - 1) -> incidence

prevalence_sensitivity %>%
  map(dplyr::select, t, Rt, S, lab) %>%
  map(mutate, inc = round(pop_tmp$tot - S) - 1) -> incidence_sensitivity

SEIR_plot <- comps_to_plot %>%
  filter(!comp %in% c("S", "R_neg","E"),
         t <= 150) %>%
  ggplot(., aes(x = t, y = value, group = comp, color = comp))+
  geom_line() +
  facet_wrap(~Rt, ncol = 1, 
             labeller = label_bquote(R[t] == .(Rt))) +
  scale_y_continuous(labels = gigamonagi::scientific_10) +
  theme_bw() +
  ggsci::scale_color_lancet(labels = c("Pre-clinical",
                                       "Clinical",
                                       "Subclinical",
                                       "Post-Clinical\nPCR+\nRemoved",
                                       "Post-Subclinical\nPCR+\nRemoved")) +
  labs(x = "Time since first case (days)",
       y = "Incidence",
       color = "Compartment") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 16,
                                   vjust = 0.5,
                                   margin = margin(b = 0.5, t= 0.5, unit = 'cm')),
        legend.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        strip.background = element_rect(NA),
        panel.grid = element_blank()) +
  guides(col = guide_legend(override.aes = list(size = 3)))

# ggsave(filename = "figs/SEIR_baseline.png", plot = SEIR_plot,
#        width = 10,
#        height = 10)

comps_to_plot %>%
  mutate(Rt = factor(Rt)) %>%
  ggplot(., aes(x = t, y = value, group = Rt, color = Rt)) +
  geom_line() + facet_wrap(~comp, ncol = 2,
                           scales = "free_y") +
  scale_x_continuous(limits = c(0, 150)) +
  scale_color_brewer(palette = "Reds") +
  theme_bw() +
  theme(legend.position = "bottom")

# if(!file.exists("code/results/run_sim.rdata")){
#   save(sim, sim_sensitivity,
#        prevalence, prevalence_sensitivity,
#        prevalence_age, prevalence_age_sensitivity,
#        incidence, incidence_sensitivity,
#        file = "code/results/run_sim_14.rdata")
# }

