#' Create modelled gross flows consistent with target net
#'
#' Given a set of prior values for gross flows - base_in and base_out - and a
#' target net flow, return the maximum likelihood combination of gross flows 
#' that satisfy the target.
#' Flow probabilities are modelled as Poisson distributions, with lambda values
#' set as the base flows.
#' The jump_scale parameter affects the maximum size of the adjustment steps 
#' as the algorithm tries to converge on an optimum solution.
#' Lower values of jump_scale may be faster, but potentially less reliable.
#' Always use a value greater than 1.
#' The modelled gross flows are returned as a named vector. These can be 
#' split using unnest_wider(col = model_flows).
#' 
#' @param base_in Numeric. Prior value for the inflow. 
#' @param base_out Numeric Prior value for the outflow.
#' @param target_net Numeric. The net flow that the gross flows must match
#' @param jump_scale An integer. Determines the size of adjustment in each 
#' iterative step. Large values give smaller adjustments. 
#' @return named vector of modelled gross flows.
##' 
#' 
#' @export
#' 
#' 

optimise_gross_flows <- function(base_in, base_out, target_net, jump_scale = 10) {
  
  # as flows are modelled as Poisson distributions, values must be positive 
  # integers for the main part of the modelling process. Here values are 
  # rounded and set to a minimum of 1. This has the potentially to inflate the 
  # total gross flows in cases where base values are small.
  base_out <- abs(base_out)
  base_in <- abs(base_in)
  base_net <- round(base_in - base_out, 0)
  change_net = target_net - base_net
  
  max_iterations <- ceiling(2 * abs(change_net))
  
  new_in <- round(max(base_in, 1), 0)
  new_out <- round(max(base_out, 1), 0)
  
  #starting from the base flows, make adjustments to the gross flows until target net flow is reached
  j <- 1
  while((abs(target_net - (new_in - new_out)) > 0.5) & (j <= max_iterations)){
    
    #total adjustment remaining to reach target net flow
    distance_from_target <- target_net - (new_in - new_out)
    direction_to_target <- distance_from_target/abs(distance_from_target)
    
    #adjustment to be made in this loop
    int_adjust <- direction_to_target * ceiling(abs(distance_from_target/jump_scale))
    
    #test whether making adjustment to inflow or outflow has bigger impact on combined likelihood
    #make adjustment to flow that gives smallest decrease
    # if(base_in >100 & base_out >100) {
    #   p_in_adjust <- dnorm(new_in + int_adjust, mean = base_in, sd = base_in, log = TRUE) + dnorm(new_out,  mean = base_out, sd = base_out, log = TRUE)
    #   p_out_adjust <- dnorm(new_in, mean = base_in, sd = base_in, log = TRUE) + dnorm(new_out - int_adjust,  mean = base_out, sd = base_out, log = TRUE)
    # } else {
      p_in_adjust <- dpois(new_in + int_adjust, base_in, log = TRUE) + dpois(new_out, base_out, log = TRUE)
      p_out_adjust <- dpois(new_in, base_in, log = TRUE) + dpois(new_out - int_adjust, base_out, log = TRUE)
    # }
    
    if(p_in_adjust > p_out_adjust) {
      new_in <- new_in + int_adjust
    } else {
      new_out <- new_out - int_adjust
    }
    
    j <- j + 1
  }
  
  #allocate any remaining (probably fractional) difference from target net to individual flows
  #in a way that avoids possibility of negative gross glows
  remainder = target_net - (new_in - new_out)
  
  if(abs(remainder) < min(1, abs(new_in), abs(new_out))) {
    
    if(new_in >= new_out) {
      new_in = new_in + remainder
    } else {
      new_out = new_out - remainder
    }
    
  } else {
    if(remainder > 0) {
      new_in = new_in + remainder
    } else {
      new_out = new_out - remainder
    }
  }
  
  c_out <- list(c("inflow" = new_in, "outflow" = new_out))
  
  return(c_out)
}

#function is not naturally vectorised
optimise_gross_flows = Vectorize(optimise_gross_flows, SIMPLIFY = TRUE)