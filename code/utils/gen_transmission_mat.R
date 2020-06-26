# @title: read_data.r
# @objective: generate transmission matrices
# @author: Petra Klepac
# @created: 9/2/2020
# @updated: 20/4/2020

gen_transmission_mat <- function(nt,  # stacked epidemic categories
                                 C,   # contact matrix
                                 p,   # parameters
                                 popv # pouplaiton vector
                                 ){
  # FUN to create a block diagonal matrix blockA with age-specific 
  # transmission matrices on diagonal this is transmission matrix for the 
  # entire population (n(t+1) = blockA n(t))
  
  # nt is vector of stacked epidemic categories by age, n(t) 
  # convert it to matrix form m using  t(matrix(n, nrow = nepi, byrow = F))
  # mt then has SEIR values in cols, and ages in rows
  # the third row is all the infecteds by age, for time t
  
  mt <- t(matrix(nt, nrow = nepi, byrow = F))
  Ip_t <- mt[,3] # a vector of all infecteds by age for time t
  Io_t <- mt[,4]
  Iu_t <- mt[,5]

    
  trans.matrix <- function(i, It, C, p, pop){
    # FUN to get age-specific transmission matrix A_i for stage i
    lambda <- p$R0*C[i,]%*%(Io_t + Ip_t) + p$theta*p$R0*C[i,]%*%(Iu_t)
    # It is in proportions, multiply with pop to get numbers if needed
    phi <- 1- exp(-lambda)
    A = matrix(c(1-phi,    0,                             0,                 0,                0,                0,         0,         0,        
                 phi,      1 - p$p.latent,                0,                 0,                0,                0,         0,         0,
                 0,        p$rho[[1]][i]*p$p.latent,      (1-p$p.progress),  0,                0,                0,         0,         0,
                 0,        0,                             p$p.progress,      1-p$p.recover,    0,                0,         0,         0,
                 0,        (1-p$rho[[1]][i])*p$p.latent,  0,                 0,                1-p$p.recover_u,    0,         0,         0,
                 0,        0,                             0,                 p$p.recover,      0,                1-p$p.neg, 0,         0,
                 0,        0,                             0,                 0,                p$p.recover_u,      0,         1-p$p.neg, 0,
                 0,        0,                             0,                 0,                0,                p$p.neg,   p$p.neg,   1), 
               nrow = nepi, ncol= nepi, byrow = T)
    return(A)
  }
  
  # find A_i for all stages and put them in the list
  Ai <- vector("list", nage)
  for (i in 1:nage){
    Ai[[i]] <- trans.matrix(i,It,C,p,pop)
  }
  blockA <- bdiag(Ai) 
  return(blockA)
}
