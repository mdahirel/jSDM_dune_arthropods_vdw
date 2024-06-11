library(coda)
library(Hmsc)
library(phytools)
library(posterior)
library(tidyverse)

HMSCphylosig <- function(Hmsc) {
  
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
  
  params <- params |> 
    select(-c(name,variable)) |> 
    group_by(coef,.draw,.iteration,.chain) |> 
    nest() 
  
  params<-cbind(params,tibble(tree=rep(list(mfull$phyloTree),length(params$data))))
  
  output <- params|> 
    mutate(lambda= map2(.x=data,.y=tree,.f=function(x=.x,tree=.y){
     trait=x$value
     names(trait)=x$species
     lambda=phytools::phylosig(x=trait,tree=tree,method="lambda")$lambda
       
    }
      )) |> 
    select(coef,lambda) |> 
    unnest(lambda) |> 
    group_by(coef) |> 
    mean_qi()
  
  return(output)
  
}
