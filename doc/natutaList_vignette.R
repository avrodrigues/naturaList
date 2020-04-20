## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#"
)

## ----install, eval=F, echo = T------------------------------------------------
#  library(devtools)
#  install_github("avrodrigues/naturaList")

## ----setup, eval=FALSE--------------------------------------------------------
#  # Load package and data
#  library(naturaList)
#  
#  data("A.setosa")
#  data("speciaLists")
#  
#  # see the size of datasets
#  dim(A.setosa) # see ?A.setosa for details
#  dim(speciaLists) # see ?speciaLists for details

## ----classify, eval=F, echo = T-----------------------------------------------
#  # classification
#  occ.class <- classify_occ(A.setosa, speciaLists)
#  y
#  y
#  y
#  y
#  
#  dim(occ.class)

## ----levels, eval=F, echo = T-------------------------------------------------
#  table(occ.class$naturaList_levels)

## ----create_spec_df, eval=F, echo = T-----------------------------------------
#  # create a specialist table example
#  br.musicians <- c("Caetano Veloso", "Antônio Carlos Tom Jobim",
#                    "Gilberto Gil", "Vinícius de Morais")
#  
#  spec_df <- create_spec_df(br.musicians)
#  spec_df

## ----get_det_names, eval=F, echo = T------------------------------------------
#  # check out if there are strings which are not taxonomists
#  get_det_names(A.setosa)
#  
#  # include these strings in a object
#  ig.names <- c("Sem Informação" , "Anonymous")
#  
#  # use 'ignore.det.names' to ignore those strings in classify_occ()
#  occ.class <- classify_occ(A.setosa, speciaLists, ignore.det.names = ig.names)
#  y
#  y
#  y
#  y
#  
#  table(occ.class$naturaList_levels)
#  

## ----grid_filter, eval=F------------------------------------------------------
#  # grid filtering
#  occ.grid <- grid_filter(occ.class)
#  dim(occ.grid)
#  table(occ.grid$naturaList_levels)

## ----map_module, eval=F-------------------------------------------------------
#  # map module
#  occ.selected <- map_module(occ.grid)
#  

