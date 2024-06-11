library(ggdist)
library(tidyverse)

HMSCtjurD <- function(HMSCfitted){
  
  if(is_empty(attr(HMSCfitted,"HMSCfitted"))){stop("input should be the result of an HMSCfitted() call")}
  
  tjur <- HMSCfitted |> 
    group_by(.draws,species) |> 
    mutate(occ_obs=mean(observed)) |> 
    group_by(.draws,species,observed) |> 
    summarise(meanfit=mean(fit),occ_obs=mean(occ_obs)) |> 
    ungroup() |> 
    pivot_wider(names_from=observed,names_prefix="obs",values_from=meanfit) |> 
    mutate(TjurD=obs1-obs0) 
  
  tjur_species <- tjur |> 
    group_by(species) |> 
    mean_qi(TjurD) |> 
    ungroup()
  
  tjur_avg <- tjur |>
    group_by(.draws) |> 
    summarise(mean_D=mean(TjurD),cor_D_occurrence=cor(TjurD,occ_obs)) |> 
    ungroup() |> 
    mean_qi(mean_D,cor_D_occurrence)
  
  return(list(tjurD_per_species=tjur_species,
              tjurD_avg=tjur_avg))
  
}