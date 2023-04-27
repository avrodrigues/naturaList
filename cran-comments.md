## Test environments
* local Windows 10, R 4.2.2
* macOS  12.6.4 21G526 (github-actions), R 4.2.3  
* Windows Server 2022, R-devel, 64 bit (r-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-hub)
* Fedora Linux, R-devel, clang, gfortran (r-hub)
* win-builder

## R CMD check results
## local Windows 10, R 4.2.2

Ok

## macOS  12.6.4 21G526 (github-actions), R 4.2.3 

there were 2 Warnings and 2 notes:

 ❯ checking files in ‘vignettes’ ... WARNING
  Files in the 'vignettes' directory but no files in 'inst/doc':
    ‘draw_polygon.png’, ‘fig_map_module.png’, ‘grid_filter.png’,
      ‘naturaList_grid_filter_vignette.Rmd’,
      ‘naturaList_map_module_vignette.Rmd’,
      ‘natutaList_clean_eval_vignette.Rmd’, ‘natutaList_vignette.Rmd’

❯ checking package vignettes in ‘inst/doc’ ... WARNING
  Directory 'inst/doc' does not exist.
  Package vignettes without corresponding single PDF/HTML:
     ‘naturaList_grid_filter_vignette.Rmd’
     ‘naturaList_map_module_vignette.Rmd’
     ‘natutaList_clean_eval_vignette.Rmd’
     ‘natutaList_vignette.Rmd’

❯ checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: ‘tm’
    All declared Imports should be used.

❯ checking data for non-ASCII characters ... NOTE
    Note: found 5 marked Latin-1 strings
    Note: found 7187 marked UTF-8 strings 

## macOS Ventura 13.3.1, R 4.3.0 beta (check_mac_release)

Ok
  
## Windows Server 2022, R-devel, 64 bit (r-hub)

Note:
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

## Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-hub)

OK

## Fedora Linux, R-devel, clang, gfortran (r-hub)

OK

## win-builder
ok
