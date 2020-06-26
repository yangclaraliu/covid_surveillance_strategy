# targets <- colnames(n_test)[1:6]
# dim(n_test)
# param_outbreak_index <- grep("prob",colnames(param_outbreak))
# #check consistent column names
# targets == gsub("prob_","",colnames(param_outbreak)[param_outbreak_index])
# 
# get_p_s <- function(scenario){
#   tictoc::tic()
#   lapply(2:t_max, function(t) {
#     map2(.x = n_test[scenario,1:length(targets)],
#          .y = param_outbreak[t, param_outbreak_index],
#          ~length(which(rbinom(n = n_obs, size = .x, prob = .y)>=1))/n_obs)
#   }) %>% setNames(2:t_max) %>% 
#     enframe() %>% 
#     unnest(value) %>% unnest(value) %>% 
#     rename(t = name) %>% 
#     mutate(name = rep(targets, t_max-1)) %>% 
#     pivot_wider(names_from = name,
#                 values_from = value) -> tmp
# 
#   bind_cols(lapply(2:7, function(y) {1-sapply(1:nrow(tmp), function(x) prod(1-tmp[1:x,y]))})) %>%
#     setNames(targets) %>%
#     rownames_to_column(var = "t") %>%
#     mutate(scenario = scenario) -> tmp2
#   print(scenario)
#   tictoc::toc()  
#   return(tmp2)
# }
# 
# plan(multiprocess)
# map(.x = c(1:3), .f = ~get_p_s(.x), .progress = TRUE) -> effects
# 
# 
# write_rds(effects, path = "code/results/effects.rds")
# 
# 
# 
# 
# 
# # get_qual <- function(p_test, param_outbreak){
# 
# # for(s in 1:S){
# #     p_detect <- list()
# #     n_test <- round(n_count*p_test[s,])
# 
# # p_detect[["fc_patient"]] <- sapply(2:t_max, function(t) {
# #   which(rbinom(n = n_obs,
# #                size = n_test$fc_patient,
# #                prob = param_outbreak$n_clinicals[t]/param_outbreak$n_denom_fc[t]) >= 1) %>% length/n_obs
# # })
# # 
# # p_detect[["rd_patient"]] <- sapply(2:t_max, function(t) {
# #   which(rbinom(n = n_obs,
# #                size = n_test$rd_patient,
# #                prob = param_outbreak$n_clinicals[t]/param_outbreak$n_denom_rd[t]) >= 1) %>% length/n_obs
# # })
# # 
# # p_detect[["od_patient"]] <- sapply(2:t_max, function(t) {
# #   which(rbinom(n = n_obs,
# #                size = n_test$od_patient,
# #                prob = param_outbreak$n_subclinicals[t]/param_outbreak$n_denom_baseline[t]) >= 1) %>% length/n_obs
# # })
# # 
# # p_detect[["fc_hcw"]] <- sapply(2:t_max, function(t) {
# #   which(rbinom(n = n_obs,
# #                size = n_test$fc_hcw,
# #                prob = unlist(hcw_RR[t, "IR3"])*param_outbreak$n_COVID19[t]/param_outbreak$n_denom_baseline[t]) >= 1) %>% length/n_obs
# # })
# # 
# # p_detect[["rd_hcw"]] <- sapply(2:t_max, function(t) {
# #   which(rbinom(n = n_obs,
# #                size = n_test$rd_hcw,
# #                prob = unlist(hcw_RR[t, "IR2"])*param_outbreak$n_subclinicals[t]/param_outbreak$n_denom_baseline[t]) >= 1) %>% length/n_obs
# # })
# # 
# # p_detect[["od_hcw"]] <- sapply(2:t_max, function(t) {
# #   which(rbinom(n = n_obs,
# #                size = n_test$od_hcw,
# #                prob = unlist(hcw_RR[t, "IR1"])*param_outbreak$n_subclinicals[t]/param_outbreak$n_denom_baseline[t]) >= 1) %>% length/n_obs
# # })
# # 
# p_detect %>% 
#   bind_cols() %>% 
#   -1 %>% 
#   abs -> p_undetect
# 
# p_undetect_ts <- matrix(NA, 
#                         ncol = ncol(p_undetect),
#                         nrow = nrow(p_undetect)) %>% data.frame %>% setNames(colnames(p_undetect))
# for(i in 1:ncol(p_undetect)) p_undetect_ts[,i] <- sapply(2:t_max, function(t) {prod(p_undetect[1:(t-1), i]) %>% unlist) 
# effects[[s]] <- sapply(1:nrow(p_undetect_ts), function(x) prod(p_undetect_ts[x,])) %>% data.frame %>% setNames("p_undetected") %>% mutate(s = s)
# }
# 
# #}
