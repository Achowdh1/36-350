generate_data = function(n, p){
  cov= matrix(rnorm(n*p), nrow = n)
  responses = rnorm(n)
  return(list(covariates = cov, responses = responses))
} 

model_fit = function(covariates, responses, cutoff){
  lm_1 = lm(responses ~ covariates)
  inds = (summary(lm_1)$coefficients)[-1,"Pr(>|t|)"] < cutoff
  if(all(inds == FALSE)){return(c())}
  lm_2 = lm(responses~covariates[,inds])
  return(lm_2)
}

library(ggplot2)
run_simulation = function(n_trials, n, p, cutoff){
  results = c()
  for(i in 1:n_trials){
    rnorm_data = generate_data(n, p)
    lm_fit = model_fit(rnorm_data$covariates, rnorm_data$responses, cutoff)
    if(!is.null(lm_fit)){
    pvals = (summary(lm_fit)$coefficients)[-1,"Pr(>|t|)"]
    results = c(results, pvals) }
  }
  p = ggplot(data.frame(pvals = results)) + geom_histogram(aes(x = pvals, y = ..density..))
  print(p)
}


#run_simulation(1000, 100, 10, 0.1)

#n <- c(100, 1000, 10000) ; p <- c(10, 20, 50)

#figure code, commented out
#for(nval in n){
#  for(pval in p){
#    run_simulation(1000, nval, pval, 0.05)
#  }
#}
