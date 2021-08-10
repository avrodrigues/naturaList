## Test environments
* local Windows 10, R 4.0.3
* macOS-latest (on github-actions 21-06-2021), R 4.1.0 

## R CMD check results
## local Windows 10, R 4.0.3
checking package vignettes in 'inst/doc' ... WARNING

dir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUE
Package vignettes without corresponding single PDF/HTML:
'naturaList_grid_filter_vignette.Rmd'
'naturaList_map_module_vignette.Rmd'
'natutaList_clean_eval_vignette.Rmd'
'natutaList_vignette.Rmd'

0 errors v | 3 warnings x | 0 notes v


## R CMD check results
## macOS-latest (on github-actions 21-06-2021), R 4.1.0 

── R CMD check results ─────────────────────────────────── naturaList 0.4.0 ────
Duration: 52.5s

❯ checking files in ‘vignettes’ ... WARNING
  Files in the 'vignettes' directory but no files in 'inst/doc':
    ‘draw_polygon.png’, ‘fig_map_module.png’, ‘grid_filter.png’,
      ‘naturaList_grid_filter_vignette.Rmd’,
      ‘naturaList_map_module_vignette.Rmd’,
      ‘natutaList_clean_eval_vignette.Rmd’, ‘natutaList_vignette.Rmd’

❯ checking package vignettes in ‘inst/doc’ ... WARNING
  dir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUE
  Package vignettes without corresponding single PDF/HTML:
     ‘naturaList_grid_filter_vignette.Rmd’
     ‘naturaList_map_module_vignette.Rmd’
     ‘natutaList_clean_eval_vignette.Rmd’
     ‘natutaList_vignette.Rmd’

❯ checking re-building of vignette outputs ... WARNING
  Error(s) in re-building vignettes:
  --- re-building ‘naturaList_grid_filter_vignette.Rmd’ using rmarkdown
  Warning in engine$weave(file, quiet = quiet, encoding = enc) :
    Pandoc (>= 1.12.3) not available. Falling back to R Markdown v1.
  --- finished re-building ‘naturaList_grid_filter_vignette.Rmd’
  
  --- re-building ‘naturaList_map_module_vignette.Rmd’ using rmarkdown
  Warning in engine$weave(file, quiet = quiet, encoding = enc) :
    Pandoc (>= 1.12.3) not available. Falling back to R Markdown v1.
  --- finished re-building ‘naturaList_map_module_vignette.Rmd’
  
  --- re-building ‘natutaList_clean_eval_vignette.Rmd’ using rmarkdown
  Warning in engine$weave(file, quiet = quiet, encoding = enc) :
    Pandoc (>= 1.12.3) not available. Falling back to R Markdown v1.
  Loading required package: sp
  
  Attaching package: 'dplyr'
  
  The following objects are masked from 'package:raster':
  
      intersect, select, union
  
  The following objects are masked from 'package:stats':
  
      filter, lag
  
  The following objects are masked from 'package:base':
  
      intersect, setdiff, setequal, union
  
  trying URL 'https://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio_10m_bil.zip'
  Content type 'application/zip' length 10735619 bytes (10.2 MB)
  ==================================================
  downloaded 10.2 MB
  
  The rnaturalearthhires package needs to be installed.
  Installing the rnaturalearthhires package.
  Installing package into '/private/var/folders/24/8k48jl6d249_n_qfxwsl6xvm0000gn/T/Rtmp43AFZM/file7b536e6aa91/naturaList.Rcheck'
  (as 'lib' is unspecified)
  Quitting from lines 114-119 (natutaList_clean_eval_vignette.Rmd) 
  Error: processing vignette 'natutaList_clean_eval_vignette.Rmd' failed with diagnostics:
  Failed to install the rnaturalearthhires package.
    Please try installing the package for yourself using the following command: 
       install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
  --- failed re-building ‘natutaList_clean_eval_vignette.Rmd’
  
  --- re-building ‘natutaList_vignette.Rmd’ using rmarkdown
  Warning in engine$weave(file, quiet = quiet, encoding = enc) :
    Pandoc (>= 1.12.3) not available. Falling back to R Markdown v1.
  --- finished re-building ‘natutaList_vignette.Rmd’
  
  SUMMARY: processing the following file failed:
    ‘natutaList_clean_eval_vignette.Rmd’
  
  Error: Vignette re-building failed.
  Execution halted

❯ checking data for non-ASCII characters ... NOTE
    Note: found 7187 marked UTF-8 strings

0 errors ✔ | 3 warnings ✖ | 1 note ✖


