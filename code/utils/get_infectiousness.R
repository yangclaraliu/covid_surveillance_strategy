# @title: get_infectiousness.r
# @objective: calculate likely duration of infectiousness
# @author: Yang Liu
# @created: 15/6/2020
# @updated:

read.table("data/infectiousness/HeEtAl_Infectiousness.txt",
           stringsAsFactors = FALSE) %>% 
  as_tibble() %>% 
  setNames(c("t","value")) %>% 
  mutate(t = round(parse_number(t),1)) %>% 
  group_by(t) %>% 
  summarise(value = mean(value)) %>% 
  full_join(data.frame(t = seq(-2.3,7.7,0.1)), by = "t") %>% 
  arrange(t) %>% 
  mutate(value = imputeTS::na_interpolation(value)/100,
         max = max(value),
         scaled = value/max) -> g

lapply(1:nrow(g), function(x) rbinom(n = 1000, size = 1, g$scaled[x]*0.1))%>% 
  do.call("rbind",.) %>% 
  colSums() %>% 
  #fitdist(., "gamma")
  quantile(., c(0.25,0.75))

sapply(1:1000, function(x) lapply(1:nrow(g), function(x) rbinom(n = 88, size = 1, g$scaled[x]*0.1))%>% 
  do.call("rbind",.) %>% 
  colSums() %>% 
  mean) -> f

fitdist(f, "gamma")

