# Anthropogenic noise disturbance on harbor seals

###########################################################################
# AICc function
###########################################################################
AICc <- function(x){
  ## Calculate AICc with glm of models
  n = length(x[[1]]$fitted)
  AIC.table = c() # Make a place for summary table
  for(i in 1:4) {
    AIC.table<-
      rbind(AIC.table, c(n, x[[i]]$rank + 1, x[[i]]$aic + (2*K*(K+1))/(n-K-1)))
  }
  colnames(AIC.table)<- c("N","df","AICc")
  rownames(AIC.table)<- c("seals ~ 1",
                          "seals ~ site*noise + month + tide + time",
                          "seals ~ site*noise + month + time",
                          "seals ~ site*noise + month")
  return(AIC.table)
}
