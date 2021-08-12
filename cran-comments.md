## Test environments
* local Windows 10, R 4.0.3
* macOS-latest (on github-actions 10-08-2021), R 4.1.0 
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-rub)
* Fedora Linux, R-devel, clang, gfortran (r-hub)

## R CMD check results
## local Windows 10, R 4.0.3 and macOS-latest (on github-actions 10-08-2021), R 4.1.0 

there were 2 Warnings:

* Files in the 'vignettes' directory but no files in 'inst/doc'

* dir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUE
  Package vignettes without corresponding single PDF/HTML  

## Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-rub)

There were 1 note:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Arthur  Vinicius Rodrigues <rodrigues.arthur.v@gmail.com>’

## Fedora Linux, R-devel, clang, gfortran (r-hub)

Didn't pass:

Error in install package dependencies
