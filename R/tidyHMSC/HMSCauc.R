library(ggdist)
library(tidyverse)
library(yardstick)

HMSCauc <- function(HMSCfitted,auc_type=c("PR","ROC")){
  
  if(is_empty(attr(HMSCfitted,"HMSCfitted"))){stop("input should be the result of an HMSCfitted() call")}
  if(!(auc_type %in% c("PR","ROC"))){stop("auc_type argument should be either 'PR' or 'ROC'")}
  
  auc.by.draws <- HMSCfitted |> 
    group_by(.draws,species) |> 
    nest() |>
    mutate(AUC=map(.x=data,
                   .f=function(.x){## this should be the right AUC, to check
                     if(auc_type=="PR"){
                     tibble(AUC=pr_auc_vec(truth=factor(.x$observed),event_level="second",estimate=.x$fit))
                     }else{
                     tibble(AUC=roc_auc_vec(truth=factor(.x$observed),event_level="second",estimate=.x$fit))  
                     }
                   })) |> 
    unnest(cols=AUC) |> 
    ungroup() 
  
  auc_species <- auc.by.draws |> 
    group_by(species) |>
    mean_qi(AUC)
  
  auc_avg <- auc.by.draws |>
    group_by(.draws) |> 
    summarise(mean_AUC=mean(AUC)) |> 
    ungroup() |> 
    mean_qi(mean_AUC)
  
  return(list(AUC_per_species=auc_species,
              AUC_avg=auc_avg,
              AUC_type = auc_type))
}
