# naturaList 0.4.0
* bugs solved in `create_spec_df()`:
    * warning message included for specialists names without abbreviation letters
* bug solved in `classify_occ()`:
    * it is possible to use a list of specialists with only 1 row
* some arguments for column names of the occurrence dataset was changed. They are 
still working, with warning message informing they are deprecated and what is the
current argument accepted. 

# naturaList 0.3.1

* bugs solved in `classify_occ()`. Inclusion of a new dataset `cyathea.br` in the package.

# naturaList 0.3.0

* Added functions `clean_eval()` and `define_env_space`. This functions implements evaluation of cleaning procedures based in classification made by `classify_occ()`. The evaluation is based on the change of convex hull area of species occurrences between cleaned and raw occurrences dataset.
