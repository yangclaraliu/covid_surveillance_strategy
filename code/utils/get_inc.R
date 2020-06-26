# @title: get_inc.R
# @objective: calculate the number of incidence exist in the community based on
# different surveillance scenarios.
# @author: Yang Liu
# @created: 20/4/2020
# @updated: 20/4/2020

# params
# psi = under-estimation rate. It is a compound indicator of overall magnitude
#       of under-reporting.
# s = scaling factor. It is a measure of representativeness of the surveillance
#     system.

get_inc_dist <- function(params){
  inc_dist_raw <- rpois(n = params$n, 
                        lambda = params$inc_obs)
  inc_dist_exp <- inc_dist_raw/(params$psi*params$s)
  return(inc_dist_exp)
}

