library(coda)
library(Hmsc)
library(tidyverse)

HMSCfitted <- function(Hmsc){
  
  if(!("Hmsc" %in% class(Hmsc))){stop("input must be a model fitted with the Hmsc package")}
  if(is_empty(Hmsc$postList)){stop("input is an initialised-only model; posterior has not been sampled yet, use sampleMcmc() to sample")}
  
  p <- computePredictedValues(Hmsc)
  Y <- Hmsc$Y
  sample_names <- rownames(Y)
  
  tab <- tibble(preds=array_tree(p,margin=3)) |> 
    mutate(.draws=1:length(preds)) |> 
    mutate(fit = map(.x=preds,
                     .f=function(.x){
                       x <- as.data.frame(.x) 
                       x$sample_ID=sample_names
                       x |> 
                         pivot_longer(cols=!sample_ID,
                                      names_to="species",values_to="fit"
                         )
                     })) |> 
    select(c(.draws,fit)) |> 
    unnest(cols=c(fit))
  
  y=as.data.frame(Y)
  y$sample_ID = row.names(y)
  y<-y|> 
    pivot_longer(cols=!sample_ID,
                 names_to="species",values_to="observed"
    )
  
  tab <- left_join(tab,y)
  
  attr(tab,"HMSCfitted") <- TRUE
  
  return(tab)
}


HMSCfitted_CV <- function(Hmsc,CVpreds){
  
  if(!("Hmsc" %in% class(Hmsc))){stop("input must be a model fitted with the Hmsc package")}
  if(is_empty(Hmsc$postList)){stop("input is an initialised-only model; posterior has not been sampled yet, use sampleMcmc() to sample")}
  if(is_empty(attr(CVpreds,"HMSCcrossval"))){stop("CVpreds should be the results of a computePredictedValues() call with partitions")}
  
  p <- CVpreds
  Y <- Hmsc$Y
  sample_names <- rownames(Y)
  
  tab <- tibble(preds=array_tree(p,margin=3)) |> 
    mutate(.draws=1:length(preds)) |> 
    mutate(fit = map(.x=preds,
                     .f=function(.x){
                       x <- as.data.frame(.x) 
                       x$sample_ID=sample_names
                       x |> 
                         pivot_longer(cols=!sample_ID,
                                      names_to="species",values_to="fit"
                         )
                     })) |> 
    select(c(.draws,fit)) |> 
    unnest(cols=c(fit))
  
  y=as.data.frame(Y)
  y$sample_ID = row.names(y)
  y<-y|> 
    pivot_longer(cols=!sample_ID,
                 names_to="species",values_to="observed"
    )
  
  tab$species=colnames(Y)[as.numeric(str_extract(tab$species,"[:digit:]+"))]
  ## needed because cross-validated predictions don't save species names, but V1,V2...
  
  tab <- left_join(tab,y)
  
  attr(tab,"HMSCfitted") <- TRUE
  
  return(tab)
}