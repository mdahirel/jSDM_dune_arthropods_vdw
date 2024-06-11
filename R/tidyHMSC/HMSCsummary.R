library(coda)
library(Hmsc)
library(posterior)
library(tidyverse)

HMSCsummary <- function(Hmsc) {
  
  if(!("Hmsc" %in% class(Hmsc))){stop("input must be a model fitted with the Hmsc package")}
  if(is_empty(Hmsc$postList)){stop("input is an initialised-only model; posterior has not been sampled yet, use sampleMcmc() to sample")}
  
  HMSCcoda <- convertToCodaObject(Hmsc)
  if ("Rho" %in% names(HMSCcoda)) {
    varnames(HMSCcoda$Rho) <- "Rho"
  }
  ## tiny patch around the fact that since Rho contains only one variable, Hmsc doesn't name that variable
  ## that seems to lead to issues for posterior_draws() if not patched
  
 params<-enframe(HMSCcoda,name = "parameter_type")
 
 if(any(params$parameter_type %in% c("Eta","Lambda","Omega","Alpha","Psi","Delta"))){
   LF_plusSigma_params<-params |> 
     filter(parameter_type %in% c("Beta","Gamma","V","Rho","Sigma"))
   
   LR_params <- params |> 
     filter(parameter_type %in% c("Eta","Lambda","Omega","Alpha","Psi","Delta"))|> 
     unnest(value)
   
   params <- rbind(LF_plusSigma_params,LR_params) 
 }
 
 params <- params |> 
   group_by(parameter_type) |> 
   mutate(index=row_number(value)) |> 
   ungroup()
 
 summarised <-params|>
   mutate(posterior_draws = map(
     .x = value,
     .f = function(.x) {
       as_draws_df((.x))
     }
   )) |>
   mutate(diagnostics = map(
     .x = posterior_draws,
     .f = function(x = .x) {
       summarise_draws(
         x,
         mean,
         ~ quantile2(.x, probs = c(0.025, 0.975)), # set a function argument to allow these to be changed?
         default_convergence_measures()
       )
     }
   ))
 
 output <- summarised |>
   select(parameter_type,index, diagnostics) |>
   unnest(cols = c(diagnostics))
 
 return(output)
 
}
