library(ggdist)
library(tidyverse)

HMSCoccurrence <- function(HMSCfitted){
  
  if(is_empty(attr(HMSCfitted,"HMSCfitted"))){stop("input should be the result of an HMSCfitted() call")}
  
  occ<-HMSCfitted |> 
    group_by(.draws,species) |> 
    summarise(occurrence_pred=mean(fit),occurrence_obs=mean(observed)) |> 
    group_by(species,occurrence_obs) |> 
    mean_qi(occurrence_pred) |> 
    ungroup() |> 
    arrange(-occurrence_obs)
  
  occ_avg<-HMSCfitted |> 
    group_by(.draws,species) |> 
    summarise(occurrence_pred=mean(fit),occurrence_obs=mean(observed)) |> 
    group_by(.draws) |> 
    summarise(avg_pred=mean(occurrence_pred),avg_obs=mean(occurrence_obs)) |> 
    group_by(avg_obs) |> 
    mean_qi(avg_pred) |> 
    ungroup()
  
  return(list(occ=occ,occ_avg=occ_avg))
}