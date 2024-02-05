## Test environments
* local Windows 10, R 4.1.3
* macOS  11.6.5 20G527 (github-actions), R 4.1.3  
* Windows Server 2022, R-devel, 64 bit (r-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-hub)
* Fedora Linux, R-devel, clang, gfortran (r-hub)

## R CMD check results
## local Windows 10, R 4.1.3 (date 05-02-2024)

everything ok

## macOS-latest (at github-actions 02-03-2022), R 4.1.2 

there were 3 notes:

* Files in the 'vignettes' directory but no files in 'inst/doc'

* dir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUEdir.exists(dir) is not TRUE
  Package vignettes without corresponding single PDF/HTML  
  
## Windows Server 2022, R-devel, 64 bit (r-hub) (05-02-24)
3 Notes:

❯ checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

## Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-hub) (05-02-24)

OK

## Fedora Linux, R-devel, clang, gfortran (r-hub) (05-02-24)

OK
