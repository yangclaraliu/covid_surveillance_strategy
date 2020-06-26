# @title: interpolate_data.r
# @objective: interpolate time series of proportions
# @author: Yang Liu
# @contributor: Sam Clifford
# @created: 20/4/2020
# @updated: 22/5/2020

library(tidyverse)
require(DescTools)
require(imputeTS)

interpolate <- function(x, trange, dt = 1){
  read.table(x, sep = ",") %>% 
    setNames(c("t","p")) %>% 
    as_tibble %>% 
    mutate(t = round(t, 1), # why are we rounding t here?
           comp = "Io") %>% 
    right_join(enframe(seq(trange[1], trange[2], dt)), by = c("t" = "value")) %>% 
    mutate(p = imputeTS::na_interpolation(p)) 
  
  # is there a reason we are skipping "Io" in `comp`?
}
