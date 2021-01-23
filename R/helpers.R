distance2 = function(P, Q, method, p = NULL, test.na = TRUE, unit, est.prob = NULL){
  x = rbind(P, Q)
  suppressMessages(philentropy::distance(x,
                        method = method,
                        p = p,
                        test.na = test.na,
                        unit = unit,
                        est.prob = est.prob))
}

normalize_signature = function(x, normalization){
  if (normalization == "pdf"){
    return(x / sum(x))
  }
}
