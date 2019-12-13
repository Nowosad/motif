distance2 = function(P, Q, method, p = NULL, test.na = TRUE, unit, est.prob = NULL){
  x = rbind(P, Q)
  message("Metric: '", method, "' using unit: '", unit, "'.")
  suppressMessages(philentropy::distance(x,
                        method = method,
                        p = p,
                        test.na = test.na,
                        unit = unit,
                        est.prob = est.prob))
}
