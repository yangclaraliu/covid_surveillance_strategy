# @title: gen_act_lvl.r
# @objective: generate matrices for summarising activity levels
# @author: Yang Liu
# @created: 23/4/2020
# @updated: 23/4/2020

gen_act_lvl <- function(act = NULL){
  tmp <-  array(1,dim = c(length(settings),nage))
  # check if parameters are properly defined
  if(is.null(act)) {
    print("Generated baseline activity matrix.")
    return(tmp)
    }
  if(any(is.na(act$row))) stop("Missing row index(es).")
  if(length(act$row) != length(act$val)) stop("Missing `val`s.")
  
  # modify activity levels
  if(any(is.na(act$col))){
    tmp[act[is.na(act$col),"row"],] <- act[is.na(act$col),"val"]
    paste0("The activity level in the settings of ", 
           settings[act[is.na(act$col),"row"]], 
           " has been reduced to ", 
           act[is.na(act$col),"val"],
           ".") -> message_1
    print(message_1)
  }
  
  if(any(!is.na(act$col))){
    tmp2 <- act[!is.na(act$col),]
    for(i in 1:nrow(tmp2)) tmp[tmp2$row[i], tmp2$col[i]] <- tmp2$val[i]
    paste0("The activity level in the settings of ",
           settings[act[!is.na(act$col),"row"]],
           " and specifically for age group ",
           age_groups[act[!is.na(act$col),"col"]],
           " has been changed to ",
           act[!is.na(act$col),"val"],
           ".") -> message_2
    print(message_2)
  }
  return(tmp)
}