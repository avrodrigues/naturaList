---
title: "Introduction to naturaList R Package"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{natutaList_vignette}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#"
)
```

The aim of `naturaList` package is to implement a classification of occurrence records based on the confiability in the species identification. The classification could be done up to six levels of confidence. Additionally, `naturaList` provides tools to filter the occurrence data based on these classification levels.

## Installation
Install the package from github:

```{r install, eval=F, echo = T}
library(devtools)
install_github("avrodrigues/naturaList")
```

## Classify occurrence records based on confidence in the species identification

The rationale of the classification is that the most reliable identification of a specimen is made by a specialist in the taxa. To classify an occurence at this level of confidence, the `classify_occ()` function needs of an occurrence and a specialist dataset. The other levels are derived from information contained in the occurence dataset. Thus, the default order of confidence levels is:

 * Level 1 - species is identified by a specialist, if not;
 * Level 2 - species is identified by a taxonomist, if not;
 * Level 3 - occurrence record has an image associated, if not;
 * Level 4 - the specimen is preserved in a scientific colection, if not;
 * Level 5 - the identification was done in filed observation, if not;
 * Level 6 - no criteria was met.
 
The user can alter this order, depending on his/her objetives, except for the Level 1 that is always a species determined by a specialist. 

As exemple, we will use the datasets in `naturaList`: `A.setosa`, as the occurrence dataset, and `speciaLists`, as the specialist dataset. In the `A.setosa` there are occurrence records for *Alsophila setosa*, a tree fern of the Brazilian Atlantic Forest. This dataset were downloaded from [Global Biodiversity Information Facilility (GBIF)]( https://doi.org/10.15468/dl.6jesg0). The `speciaLists` is a dataset with specialists of ferns and licophytes of Brazil, which we gathered from the authors of this [paper](http://dx.doi.org/10.1590/2175-7860201566410). 

```{r setup, eval=FALSE}
# Load package and data
library(naturaList)
data("A.setosa")
data("speciaLists")
# see the size of datasets
dim(A.setosa) # see ?A.setosa for details
dim(speciaLists) # see ?speciaLists for details
```

Classification using the default order of confidence levels
```{r classify, eval=F, echo = T}
# classification
occ.class <- classify_occ(A.setosa, speciaLists)
y
y
y
y
dim(occ.class)
```
 
When the name of the taxonomist that identified the species do not perfect match with specialists names, the function asks whether the taxonomist name represents a specialist. Then the user must type `y` or `n` to answer yes or no for each umbiguos name. In our example, this checking procedure is needed in 4 occurrences. All of them were identified by specialists. Because this the code have 4 lines with `y`. 


You can check how many occurrences was classified in each level:
```{r levels, eval=F, echo = T}
table(occ.class$naturaList_levels)
```

## Create a specialist dataset

You can easily create a specialist dataset using `create_spec_df()`. You just need to provide a character vector with the names of specialists, and the output is a dataset formated be used in `classify_occ()`. 

In this example, we use the names of four famous brazilians musicians. Note that the Latin accent mark is provided, and even a nickname (e.g. Tom Jobim). 

```{r create_spec_df, eval=F, echo = T}
# create a specialist table example
br.musicians <- c("Caetano Veloso", "Antônio Carlos Tom Jobim", 
                  "Gilberto Gil", "Vinícius de Morais")
spec_df <- create_spec_df(br.musicians)
spec_df
```

## Verify if the strings in the 'determined.by' column of occurrence dataset is a name 

It might occur that some strings in the 'determined.by' column of the occurrence dataset do not correspond to a taxonomist name. Strings as such `"Unknown"` often is included in the 'determined.by' data field. It is important then that such strings be ignored by the `classify_occ()`, if not this function could flag an occurrence record as determined by a taxonomist when it was not. 

To cope with this issue, `get_det_names()` can be used to verify which strings are not taxonomists names. This function returns all unique strings in the 'determined.by' column of the dataset. Based on this list of names, you could create a character vector with the strings to be ignored by `classify_occ()`, providing it to the `ignore.det.names` argument. See also the `?classify_occ` for more details. 

```{r get_det_names, eval=F, echo = T}
# check out if there are strings which are not taxonomists
get_det_names(A.setosa)
# include these strings in a object
ig.names <- c("Sem Informação" , "Anonymous")
# use 'ignore.det.names' to ignore those strings in classify_occ()
occ.class <- classify_occ(A.setosa, speciaLists, ignore.det.names = ig.names)
y
y
y
y
table(occ.class$naturaList_levels)
```

## Filter occurrence records within grid cells

With `grid_filter()` you may filter occurrence records inside grid cells in order to keep only the record with the highest confidence level in each grid cell. 

![Fig. 1 - The procedure of `grid_filter()` to select the record with the highest confidence level in each grid cell](../vignettes/grid_filter.png)

As example, we will filter the `occ.class` in square grid cells of 0.5 decimal
degree of side, which is the default grid cell size of `grid_filter()`. You can 
provide the height and width of the grid cell in `grid.resolution` argument, or 
provide a raster layer in argument `r` from which the cell is used to filter 
occurrences. 

```{r grid_filter, eval=F}
# grid filtering
occ.grid <- grid_filter(occ.class)
dim(occ.grid)
table(occ.grid$naturaList_levels)
```

## Check out for records in a interactive map module

The `map_module()` function allow the user to visualize the occurrence
records in a interactive map. In this interactive map, the user can click over an
occurence point to see basic information about the record, such as the instituition
that holds the record, who identified the species and when it was identified. 
Also, if some error is identified, the user could allow that clicking works to 
delete points. 


```{r map_module, eval=F}
# map module
occ.selected <- map_module(occ.grid)
```


![Fig. 2 - Features of `map_module()` function used in the example with Alsophila setosa occurrence data. A) the initial display of occurrence records data; B) highlights the features of map_module. I – Select levels to be maintained in the output dataset; II – enable to delete point with click over occurrences; III – show how many points have been selected from the initial dataset; IV – display in the map only occurrences that belong to the selected levels, and turn visualization of the map between OpenStreetMap and Satelite; V – options to draw and edit polygons to select occurrence records that fall within polygon; X – highlights the deleted occurrence points](../vignettes/fig_map_module.png)

In the figure 2, you see two screen shots from `map_module()`. In figure 2A, it is showed the initial display of occurence records. In figure 2B it is highlighted the features inside `map_module()`. 

In this example, we selected the levels 1 and 2 (Fig 2B - I). Then we deleted three records, by setting on the button *delete points with click* (Fig 2B - II) and clicking on them. At the end of selection we click on 'Done !' button. 

Other features are avalible and can be seen on the figure 2. Try out with this example dataset. 
