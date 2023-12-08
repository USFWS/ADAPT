#function for analysis

#global controls whether we're running just the global model or
# if the model should run each covariate and combinations so that 
# we can do model comparisons

inla_func <- function(vars,pts_pt,matern,global){
  if(global == FALSE){ 
    cov_vec <- vars
    n_cov <- length(cov_vec)
    list_covs <- as.list(NA)
    n_forms <- 0
  
  #calculate number of formulas to run
    for(i in 1:n_cov){
    n_forms <- sum(n_forms,choose(n_cov,i))
    }
  
    if(n_forms > 10) {warning(paste0("wow that's ",n_forms," models!"))}
  
    list_formula <- as.list(NA)
    list_formula_tmp <- as.list(NA)
  
    for(i in 1:n_cov){
    #each combo is an element of the list
      list_formula_tmp <- as.list(NA)
        for(j in 1:choose(n_cov,i)){
      # list_formula_tmp[j] <- paste(combn(cov_vec,i)[,j],
      #            "(st_coordinates,weights=",
      #            combn(cov_vec,i)[,j],
      #            ",model=matern)",sep="",collapse="+")
        list_formula_tmp[j] <- paste(combn(cov_vec,i)[,j],collapse = "+")
      }
      list_formula[[i]] <- list_formula_tmp
    }
  
    list_formula <- unlist(list_formula)
    list_formula2 <- as.list(NA)
    for(i in 1:n_forms){
      list_formula2[[i]] <- list_formula[i]
      }
  
      output <- as.list(NA)
      pc_prec <- list(prior = "pcprec", param = c(1, 0.1))
      
      for(i in 1:n_forms){
      inla_formula = as.formula(paste('obs ~ -1 +', list_formula2[i]))
     
      output[[i]] <- bru(components = inla_formula,
                       family = "binomial",data = pts_pt, 
                       options = list(control.compute = list(waic = TRUE, cpo = TRUE),
                                      control.inla = list(strategy = "simplified.laplace",
                                                          int.strategy = "eb"),
                                      control.predictor = list(compute=TRUE,link=1),
                                      verbose = FALSE))
    
      if(sum(output[[i]]$cpo$failure) > 0){
      warning("Model ",n_forms," has convergence issues (sum(CPO$Failure > 0))")
      }
    
      }
  } else {
    
    output <- as.list(NA)
    pc_prec <- list(prior = "pcprec", param = c(1, 0.1))
    #just run global model
    inla_comp <- as.formula(paste('~ -1 +', vars))
    
    inla_formula <- obs ~ .
    
    
    output <- bru(
      components = inla_comp,
      like(formula = inla_formula,
          family = "binomial",
          data = pts_pt), 
      options = list(control.compute = list(waic = TRUE, cpo = TRUE),
                        control.inla = list(#strategy = "simplified.laplace",
                                          int.strategy = "eb"),
                        control.predictor = list(compute=TRUE,link=1),
                        verbose = FALSE))
    
  }
  
      
  return(output=output)
}
