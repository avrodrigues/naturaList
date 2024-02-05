## Test environments
* local Windows 10, R 4.1.3
* macOS  11.6.5 20G527 (github-actions), R 4.1.3  
* Windows Server 2022, R-devel, 64 bit (r-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-hub)
* Fedora Linux, R-devel, clang, gfortran (r-hub)

## R CMD check results
## local Windows 10, R 4.1.3 (date 02-02-2024)

everything ok

## macOS-latest (at github-actions 02-03-2022), R 4.1.2 

there were 2 Warnings:

* Files in the 'vignettes' directory but no files in 'inst/doc'

* dir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUE
  Package vignettes without corresponding single PDF/HTML  
  
## Windows Server 2022, R-devel, 64 bit (r-hub)

Note:
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

## Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-hub)

OK

## Fedora Linux, R-devel, clang, gfortran (r-hub)

OK
