get_normalized = function(x, normalization){
  sum_x = sum(x)
  if (sum_x == 0){
    return(rep(0, length(x)))
  } else if (normalization == "pdf"){
    return(x/sum_x)
  }
}
