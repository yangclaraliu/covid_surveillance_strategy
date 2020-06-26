# @title: main.R
# @objective: 
# @author: Yang Liu, Sam Clifford, Petra Klepac

# simulate a hypothetical epidemic processes
source("code/run_sim.R")
load("code/results/run_sim_6.rdata")
# load parameters of healthcare infrastructure
hcw_RR <- read_rds("code/results/hcw_RR.rds")
source("code/utils/healthcare_param.R")
# calculate the increased risks among healthcare workers
# source("code/hcw_risk.R")
# calculate surveillance sensitivity, see Fig 2
source("code/surveillance_sensitivities.R")
# comparing fever clinic and respiratory, see Fig 3 
source("code/resp_leakage.R")
# efficiency frontier and selected strategies
source("code/run_recursive.R"))