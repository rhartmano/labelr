# labelr News

# labelr 0.1.3
* functions `add_lab_dummies()` and `add_lab_dumm1()` (with aliases `ald()` and `ald1()`) added. These generate a dummy variable for each label of a value-labeled variable and return the supplied data.frame with these dummy variables added.

* functions `factor_to_lab_int()` (with alias `f2int()`) and `lab_int_to_factor()` (with alias `int2f()`) added to allow easy conversion of labeled integer variables to factors and of factors to labeled integer variables.

* function `axis_lab()` (with alias `alb()`) added - helper that allows one to easily use variable name labels as y or x axis labels with Base R plots or ggplot2.

* function `all_quant_labs()` (with alias `allq()`) added: automatically add quantile-based numerical range value labels for all numeric variables that meet specifications.

* function `tab_labs()` (with alias `tabl2()`) added: alternative implementation of `tabl()` functionality that always displays tabulations in terms of value labels (where they exist) and that automatically and temporarily converts any non-value-labeled, many-valued numerical variables to quantile category variables.

* function `use_val_lab1()` (with alias `uvl1()`) added. This provides the same functionality as `use_val_labs()`, but allows the user to supply only one unquoted variable, whereas `use_val_labs()` requires quoted variable name arguments but allows the user to pass a vector of multiple variable names in one call. 

* function `add_lab_col1()` (with alias `alc1()`) added. This provides the same functionality as `add_lab_cols()`, but allows the user to supply only one unquoted variable, whereas `add_lab_cols()` requires quoted variable name arguments but allows the user to pass a vector of multiple variable names in one call. 

* updated `add_name_labs()`, `drop_name_labs()`, `convert_labs()`, and `clean_data_atts()` functions, so that changes (add or drop) of name.labs attributes are applied to native labels() attribute and, in case of `convert_labs()`, so that any incoming variable/column level labels() attributes (e.g., column name labels from a haven-imported tibble) are converted to labelr name.labs. Primary benefit of this is that RStudio View() will now show name labels underneath column names.

* `use_val_labs()`, `add_lab_cols()`, and `val_labs_vec()` fixed to convert any irregular character values to NA labels (was converting to "NA").

* updated `ssort()` to preserve rownames.

* updated `gremlr()`, `greml()`, `slab()`, `sbrac()`, `somel()`, `taill()`, `sfilter()`, and `ssort()` to handle situations involving (or producing) one-column data.frames.

* Defensive programming improvements made to various value-label-related functions, so that they respond more gracefully and informatively to non-valid variable name arguments. 

* split up original one vignette into two respective vignettes: Introduction and Special Topics.

* light copy-editing updates to README, vignette, and documentation. 
