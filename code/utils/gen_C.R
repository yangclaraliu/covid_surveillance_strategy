# @title: gen_C.r
# @objective: generate contact matrices and load clinical fraction
# @author: Yang Liu, Petra Klepac
# Data used in this file is from
# (1) Davies et al. (Nature Medicine)
# (2) Prem et al.

# load contact matrices
contacts_china_2017 <- readRDS("data/contact/contacts_china.RDS")

Prem <- new.env()

list.files(path = "data/contact", 
           pattern = "Prem2020.*\\.rdata",
           full.names = T) %>%
  purrr::map(~load(.x, envir = Prem)) 

Prem %>%
  as.list %>%
  purrr::map("CHN") %>%
  set_names(., nm = sub(x = names(.), 
                        pattern = "contact_", 
                        fixed = T, 
                        replacement = ""))  -> contacts_china_2020

rm(Prem)

# age-specific susceptibility
covid_uy <- read_rds("data/R0/covid_uy.rds")

covid_uy %>% 
  #pull(mult) %>% mean
  dplyr::select(starts_with("u_")) %>% 
  apply(., 2, median) %>% 
  #colMeans() %>% 
  rep(., each = 2) %>% 
  setNames(unique(age_dist$age)) -> sus_age

covid_uy %>% 
  #pull(mult) %>% mean
  dplyr::select(starts_with("y_")) %>% 
  apply(., 2, median) %>% 
  #colMeans() %>% 
  rep(., each = 2) %>% 
  setNames(unique(age_dist$age)) -> cfrac_age

