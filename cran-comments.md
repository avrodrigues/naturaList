## Test environments
* local Windows 10, R 4.0.3
* macOS-latest (on github-actions 14-10-2021), R 4.1.0 
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-rub)
* Fedora Linux, R-devel, clang, gfortran (r-hub)

## R CMD check results
## local Windows 10, R 4.0.3 

there was 1 Warning:
'qpdf' is needed for checks on size reduction of PDFs

## macOS-latest (on github-actions 14-10-2021), R 4.1.0 

there were 2 Warnings:

* Files in the 'vignettes' directory but no files in 'inst/doc'

* dir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUE
  Package vignettes without corresponding single PDF/HTML  

## Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-rub)

OK

## Fedora Linux, R-devel, clang, gfortran (r-hub)

OK
