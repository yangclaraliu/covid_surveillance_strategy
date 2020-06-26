# @title: mer_act_con.r
# @objective: modify contact matrices based on their activity levels
# @author: Yang Liu
# @created: 23/4/2020
# @updated: 23/4/2020

mer_act_con <- function(act, con){
  if((length(con)-1) != nrow(act)) stop("The dimensions of your activity and contact matrices are not compatible.")
  c_tmp <- list()
  for(i in 1:nrow(act)) {
    c_tmp[[i]] <- contact[[i]] %*% diag(act[i,])
  }
  names(c_tmp) <- settings[1:nrow(act)]
  c_tmp[["all"]] <- Reduce("+", c_tmp)
  c_tmp_2 <- norm_C(c_tmp, age_dist_tmp)
  eigen <- eigen(c_tmp_2$all)
  print(paste0("Largest eigen value is ", eigen$values[1]))
  return(c_tmp_2)
}



