## Test environments
* local Windows 10, R 4.0.3
* macOS-latest (github-actions), R 4.1.2 
* Windows Server 2022, R-devel, 64 bit (r-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-rub)
* Fedora Linux, R-devel, clang, gfortran (r-hub)

## R CMD check results
## local Windows 10, R 4.0.3 

there was 1 Warning:
'qpdf' is needed for checks on size reduction of PDFs

## macOS-latest (at github-actions 02-03-2022), R 4.1.2 

there were 2 Warnings:

* Files in the 'vignettes' directory but no files in 'inst/doc'

* dir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUE
  Package vignettes without corresponding single PDF/HTML  
  
and 1 Note:

*      Note: found 5 marked Latin-1 strings
       Note: found 7187 marked UTF-8 strings

## Windows Server 2022, R-devel, 64 bit (r-hub)

Notes:
* checking examples ... NOTE
Examples with CPU (user + system) or elapsed time > 5s
            user system elapsed
grid_filter 5.84   0.14    5.98

This is the only server that the example did not pass. 


## Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-rub)

OK

## Fedora Linux, R-devel, clang, gfortran (r-hub)

OK
