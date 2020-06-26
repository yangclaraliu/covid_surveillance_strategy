require(Matrix)
require(matrixcalc)
require(data.table)
require(tidyverse)
require(matrixcalc)
require(cowplot)
require(ggpubr)
require(viridis)
require(fitdistrplus)
require(magrittr)

source("code/utils/norm_C.R")

user_name <- Sys.info()["user"]

if (user_name == "samclifford"){
  epic_path_win <- paste0("")
  dropbox_path <- "~/Dropbox/nCov-2019/data_sources/"
} else {
  epic_path_win <- paste0("C:/Users/",
                          user_name,
                          "/Documents/EPIC/")
  code_path_win <- paste0("C:/Users/",
                          user_name,
                          "/Documents/GitHub/Local_2019-nCoV/")
  dropbox_path <- paste0("C:/Users/",
                         user_name,
                         "/Dropbox/nCov-2019/data_sources")
}

# plotting stuff
al_p <- 0.5
al_w <- 1

pal <- RColorBrewer::brewer.pal(3, "Dark2")

co_f <- pal[1]
co_r <- pal[2]
co_o <- pal[3]

r_levs <- c(
  child      = "Community - Children",
  adult      = "Community - Younger Adults",
  elderly    = "Community - Older Adults",
  
  od_hcw     = "Other Dept - HCW",
  od_patient = "Other Dept - Patients",
  
  fc_hcw     = "Fever Clinic - HCW",
  fc_patient = "Fever Clinic - Patients",
  
  rd_hcw     = "Respiratory Dept - HCW",
  rd_patient = "Respiratory Dept - Patients"
  )

r_palette <- 
  c("Community - Children"        = "grey90",
    "Community - Younger Adults"  = "grey60",
    "Community - Older Adults"    = "grey10",
    
    "Other Dept - HCW"            = alpha(co_o, al_w),
    "Other Dept - Patients"       = alpha(co_o, al_p),
    
    "Respiratory Dept - HCW"      = alpha(co_r, al_w),
    "Respiratory Dept - Patients" = alpha(co_r, al_p),
    
    "Fever Clinic - HCW"          = alpha(co_f, al_w),
    "Fever Clinic - Patients"     = alpha(co_f, al_p))

r_groups <- data.frame(tags = r_levs) %>%
  rownames_to_column() %>%
  separate(., tags, into = c("group", "group2"), sep = "-", remove = F) %>%
  mutate(tags = factor(tags, levels = names(r_palette), ordered = T))
# age distribution
read_rds(paste0(dropbox_path, "/demographic_data/agedist_prf.rds")) %>% 
  .[["Y2016"]] %>%
  ungroup %>%
  mutate(age = if_else(age %in% c("80-",
                                  "75-",
                                  "85+"),
                       "75+",
                       age),
         levels = parse_number(age)) %>%
  arrange(levels) %>% 
  mutate(levels = factor(levels,
                         levels = seq(0, 75, 5),
                         labels = unique(.$age))) %>%
  dplyr::select(-age) %>%
  rename(age = levels) %>% 
  group_by(age, CNTY_CODE) %>% 
  summarise(tot = sum(tot)) -> age_dist

# population
age_dist %>% 
  group_by(CNTY_CODE) %>% 
  summarise(tot = sum(tot)) -> pop

cnty <- "110100" # code for shanghai
age_dist_tmp <- age_dist %>% filter(CNTY_CODE == cnty) %>% pull(tot)
pop_tmp <- pop %>% filter(CNTY_CODE == cnty)
length(age_dist_tmp)
settings <- c("home", "work", "school", "others")

# load contact matrices
source("code/utils/gen_C.R")
contact <- norm_C(C        = contacts_china_2020, 
                  #contacts_china_2017 or contacts_china_2020
                  popv     = age_dist_tmp,
                  sus_age  = sus_age,
                  make.sym = TRUE )