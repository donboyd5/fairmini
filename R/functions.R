
rho <- function(residuals){
  # Correlation between residuals and their lag
  cor(residuals[-length(residuals)], residuals[-1])  
}

iv_formula_combine <- function(formula, instruments){
  as.formula(paste(
    deparse(formula[[2]]), "~", deparse(formula[[3]], width.cutoff = 500), "|", deparse(instruments[[2]], width.cutoff = 500)
  ))
}


ar1_correction <- function(data, formula, instruments, max_iter = 100, tol = 1e-5, damp_factor = 0.5) {
  # ar1 correction via iterative Cochrane-Orcutt with damping
  
  # Load required package
  if (!requireNamespace("AER", quietly = TRUE)) {
    stop("Package 'AER' needed for this function to work. Please install it.")
  }
  
  # Combine formula and instruments into a single formula for ivreg
  iv_formula <- as.formula(paste(
    deparse(formula[[2]]), "~", deparse(formula[[3]], width.cutoff = 500), "|", deparse(instruments[[2]], width.cutoff = 500)
  ))
  
  # Initial 2SLS fit ignoring serial correlation
  fit_iv <- AER::ivreg(iv_formula, data = data)
  residuals_iv <- residuals(fit_iv)
  
  # Estimate initial AR(1) coefficient using arima
  fit_rho <- arima(residuals_iv, order = c(1, 0, 0), include.mean = FALSE)
  rho_initial <- coef(fit_rho)["ar1"]
  print(paste0("Initial value of rho: ", rho_initial))
  
  rho_old <- rho_initial
  
  # Iterative Cochrane-Orcutt procedure
  for (i in seq_len(max_iter)) {
    # Transform data to remove AR(1) correlation
    data_transformed <- data
    for (col in names(data)) {
      if (is.numeric(data[[col]])) {
        data_transformed[[col]][-1] <- data[[col]][-1] - rho_old * data[[col]][-nrow(data)]
      }
    }
    data_transformed <- data_transformed[-1, ]  # Drop the first observation
    
    # Re-fit 2SLS on the transformed data
    fit_iv_new <- AER::ivreg(iv_formula, data = data_transformed)
    residuals_new <- residuals(fit_iv_new)
    
    # Estimate new AR(1) coefficient
    fit_rho_new <- arima(residuals_new, order = c(1, 0, 0), include.mean = FALSE)
    rho_new <- coef(fit_rho_new)["ar1"]
    
    # Apply damping to stabilize rho
    rho_new <- damp_factor * rho_new + (1 - damp_factor) * rho_old
    
    # Print iteration details
    cat("Iteration:", i, "Rho:", rho_new, "\n")
    
    # Check for convergence
    if (abs(rho_new - rho_old) < tol) {
      cat("Convergence achieved at iteration", i, "\n")
      break
    }
    
    # Update for next iteration
    fit_iv <- fit_iv_new
    rho_old <- rho_new
  }
  
  # Return results
  list(
    fit = fit_iv,               # Final 2SLS model
    rho_initial = rho_initial,  # Initial AR(1) coefficient
    rho = rho_old,              # Final AR(1) coefficient
    iterations = i              # Number of iterations
  )
}
