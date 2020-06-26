# @title: norm_C.r
# @objective: normalise contact matrices
# @author: Petra Klepac, Yang Liu
# @created: 9/2/2020
# @updated: 18/5/2020

norm_C <- function(C, # raw contact matrix 
                   popv, # age distribution of the target population
                   sus_age  = 1, # relative susceptibility by age
                   make.sym = TRUE){
  
  if (any(sapply(C, function(x){diff(dim(x)) != 0}))){
    stop("At least one element of `C` is not a square matrix")
  }
  
  dims <- sapply(C, nrow)
  
  if (sd(dims) > 0 & length(dims) > 1){
    stop("At least one element of `C` is of a different size to the others")
  }
  
  if (!is.null(dim(sus_age))){
    warning("`sus_age` has dimensions. Check you haven't passed it as a matrix")
  }
  
  if ("matrix" %in% class(sus_age)){
    warning("`sus_age` was a matrix on entry. Making it a vector")
    sus_age <- unlist(sus_age)
  }
  
  if (length(sus_age) != 1L){
    if (any(dims != length(sus_age))){
      stop("At least one element of `C` does not conform with `sus_age`")
    }
  } else {
    if (sus_age > 1 | sus_age <= 0){
      stop("Age-related susceptibility must be greater than 0 and less than or equal to 1")
    }
  }
  
  if (make.sym){
    # make sure contacts are reciprocal
    Csym <- lapply(C, function(x, popv) (x + t(x) * ((popv)%*%t(1/popv)))/2, popv) 
  } else {
    Csym <- C # if make.sym = F leave it as is
  }
  
  
  # Vector recylcing helps us out here.
  # If sus_age == 1 then this is a scalar by a matrix
  # If sus_age is a vector (but *not* a matrix) then it gets recycled as columns
  # if worried, use the replicate below, as it gives identical results
  Csym <- lapply(Csym, function(x){x * sus_age})
  # Csym <- lapply(Csym, function(x){x * replicate(length(sus_age), sus_age)})
  
  eig1 <- Re(eigen(Csym["all"]$all)$value[1])
  Cnorm <- lapply(Csym, function(x,eig1) x/eig1, eig1)
  return(Cnorm)
}
