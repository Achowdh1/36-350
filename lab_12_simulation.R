generate_data = function(n, p){
  cov= matrix(rnorm(n*p), nrow = n)
  responses = rnorm(n)
  return(list(covariates = cov, responses = responses))
} 

model_fit = function(covariates, responses, cutoff){
  lm_1 = lm(responses ~ covariates)
  inds = (summary(lm_1)$coefficients)[-1,"Pr(>|t|)"] < cutoff
  print(summary(lm_1))
  print(inds)
  if(all(inds == FALSE)){return(c())}
  lm_2 = lm(responses~covariates[,inds])
  return(lm_2)
}
a = generate_data(10,5)
model_fit(a$covariates, a$responses, 0.7)
