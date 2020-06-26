# @title: get_comp.r
# @objective: extract one compartment's trajectory from results while summing
# over all available age group
# @author: Petra Klepac
# @created: 20/4/2020
# @updated: 20/4/2020

get_comp <- function(compartment, sim, byAge = FALSE){
  compartments <- stages
  compartment_no <- which(compartments == compartment)
  
  if(byAge == T) {
    lapply(1:nruns, function(x)   as.matrix(sim[,,x] * rep(age_dist_tmp, each = nepi)) %>% 
             .[seq(compartment_no,nepi*nage,nepi),] %>% 
             data.frame %>% 
             rownames_to_column(., var = "Age") %>% 
             mutate(Rt = R0sample[x]) %>% 
             pivot_longer(., 
                          cols = starts_with("X"),
                          names_to = "t") %>% 
             mutate(t = parse_number(t))) %>%
      bind_rows() -> tmp
  } else {
    #tmp <-  
    sapply(1:nruns, function(x)   as.matrix(sim[,,x] * rep(age_dist_tmp, each = nepi)) %>% 
                     .[seq(compartment_no,nepi*nage,nepi),] %>%
                     colSums()) %>%
      data.frame() %>% 
      rownames_to_column(var = "t") %>% 
      #setNames(c("t","X")) %>% 
      pivot_longer(cols = starts_with("X"),
                   names_to = "Rt") %>% 
      mutate(Rt = parse_number(Rt),
             Rt = R0sample[Rt],
             t = as.numeric(t)) -> tmp
  }
  return(tmp)
}
