generate_data = function(n, p){
  cov= matrix(rnorm(n*p), nrow = n)
  responses = rnorm(n)
  return(list(covariates = cov, responses = responses))
}


