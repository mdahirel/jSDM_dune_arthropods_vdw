library(coda)
library(Hmsc)
library(phytools)
library(posterior)
library(tidyverse)

HMSCgrouped_summary <- function(Hmsc,groups) {
  
  if(!("Hmsc" %in% class(Hmsc))){stop("input must be a model fitted with the Hmsc package")}
  if(is_empty(Hmsc$postList)){stop("input is an initialised-only model; posterior has not been sampled yet, use sampleMcmc() to sample")}
  
  HMSCcoda <- convertToCodaObject(Hmsc)
  
  params<- as_draws_df(HMSCcoda$Beta) |> 
    pivot_longer(-c(.iteration,.chain,.draw))|> 
    mutate(variable = str_remove_all(name, "\\([:alpha:][:digit:]+\\)")) |> 
    mutate(coef = str_split_fixed(variable," , ", 2)[,1],
           species = str_split_fixed(variable," , ",2)[,2],.after=1) |> 
    mutate(species = str_remove_all(species, " \\]")) |> 
    mutate(coef = str_remove_all(coef,"B\\["))
  
  overall_avg <- params |> 
    select(-c(name,variable)) |> 
    group_by(coef,.draw,.iteration,.chain) |> 
    summarise(value=mean(value)) |> 
    group_by(coef) |> 
    mean_qi(value) |> 
    mutate(group="overall") |> 
    select(group,coef,value,.lower:.interval)
  
  groups_avg <- params |> 
    left_join(groups) |> 
    select(-c(name,variable)) |> 
    group_by(coef,.draw,.iteration,.chain,group) |> 
    summarise(value=mean(value)) |> 
    group_by(coef,group) |> 
    mean_qi(value)|> 
    select(group,coef,value,.lower:.interval)
  
  output <- rbind(overall_avg,groups_avg)
  
  return(output)
  
}
