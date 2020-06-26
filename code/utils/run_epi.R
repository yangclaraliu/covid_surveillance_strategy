# @title: read_data.r
# @objective: run epidemic simulation
# @author: Petra Klepac
# @created: 9/2/2020
# @updated: 20/4/2020

library(progress)

run_epi <- function(n0, # initial conditions
                    C, 
                    p, # disease parameters
                    popv, 
                    t0, 
                    tmax, 
                    runs,
                    R0post){
  n <- array(NA_real_, c(length(n0), tmax, runs)) 
  n[,1,] <- matrix(rep(n0, runs), nrow = length(n0), ncol = runs, byrow = F) 
  
  
  pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent in :elapsed) eta: :eta",
    total = nruns*(tmax - t0),
    width = 80)
  
  for (run in 1:runs){
    p$R0 <- R0post[run]
    for (t in t0:(tmax-1)){
      temp <- n[,t,run] # nt vector
      n[,(t+1),run] <- as.array(gen_transmission_mat(temp, C, p, popv) %*%  temp)
      pb$tick()
    }
  }
  return(n)
}
