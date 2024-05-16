# labelr News

# labelr 0.1.6
* added two convenience functions: `fact2char()` converts all factor data.frame variables to character variables, and `irregular2()` converts all irregular (see `check_irregular()` and `irregular2v()`) values in a data.frame to some other single value (NA, by default). `irregular2()`. 

* `ssort()` code is streamlined (and no longer issues a warning when it recycles the descending argument). 

* added fact.to.char (defaults to FALSE) and irreg.to.na (defaults to FALSE) arguments to `as_base_data_frame()`.

* removed extraneous code from `transfer_labs()`.

* Corrected argument specification error in example `flab()` and `slab()` calls in the Introduction vignette (call was supplying argument "Female" when it should have supplied "F") . Vignette call syntax error was introduced in v1.0.5 and was limited to the syntax of the specific example calls in the vignette. The functions themselves were not at fault and are, thus, unchanged.

# labelr 0.1.5
* Added function `v()` to allow passing of an "unquoted character vector" (of labels or column names). 

* Updated `make_demo_data()` and associated gender identity label examples to be more inclusive.

* Fixed some typos and copy-editing errors in documentation. 

# labelr 0.1.4
* Functions `has_val_labs()`, `has_avl_labs()`, `has_m1_labs()`, and `has_quant_labs()` added to facilitate logical check of a single data.frame variable (column) for the presence of, respectively: (1) any sort of value labels, (2) `add_val_labs()` -style value labels specifically, (3) `add_m1_lab()`-style value labels specifically, or (4) `add_quant_labs()`-style value labels specifically.

* Function `all_uniquev()` (alias `all_univ()`) added. This checks where the length of a vector is equal to the length of the unique values in the vector (is equivalent to `length(x)` == `length(unique(x))`. It is anticipated that this will be used internally in future iterations of labelr.

* Improved `add_val_labs()` (and `add_val1()`) to better detect and prevent their use on variables that already have `add_m1_lab()` -style many-to-one values labels. Previously, it was possible in some cases to modify select `add_m1_lab()`-style value labels using `add_val_labs()`. Now, `add_val_labs()` (and `add_val1()`) will detect and prohibit this behavior, redirecting the user to use `add_m1_lab()` for such variables. This reinforces the distinction between `add_val_labs()`-style (one-to-one) and `add_m1_lab()`-style (many-values-to-one-label) value labels, whereas prior behavior blurred this line.

* Modified select `*1()` functions (e.g., `add_lab_col1()`, `use_val_lab1()`) so that they explicitly dis-allow indirection (e.g., passing an arbitrarily named character vector containing column names) and instead require that the supplied variable name (the var argument) be the literal name of a single variable present in the supplied data.frame. The functions permit users to pass that var name unquoted or quoted, although the documentation advises the user to supply unquoted variable names.

* Two shared, vestigial code chunks removed from the following: `use_val_labs()`, `use_val_lab1()`, `add_lab_cols()`, `add_lab_col1()`, and `val_labs_vec()` documentation. Code involved checking for all values NA from an earlier iteration of `use_val_labs()`. Code is more concise; functions, arguments, and outputs are unchanged.

* `val_labs_vec()` now strips any lingering attributes from the returned character vector. 

* Improved handling of errant and varying var arg specifications in `val_labs_vec()`, `add_lab_col1()`, and `use_val_lab1()` to be more flexible and informative if a non-existent var is supplied by the user.

* Move internal functions to the beginning of the the body of `add_val_labs()`, `add_val1()`, `add_m1_lab()`, and `add1m1()`. 

* Copy edit fixes and improved argument explanations in documentation items (e.g., an outdated comment in README, an infelicitous example in `use_val1()` and `add_lab_col1()` documentation).

* function `get_val_lab1()` (with alias `gvl1()`) added, providing a variant of `get_val_labs()` that follows the conventions of other `*1()` functions (i.e., allowing only a single variable, whose name may be passed unquoted to the var argument). In addition, `get_val_lab1()` includes a simplify argument (FALSE by default) that allows the user to return the value-label mapping as a named vector.

# labelr 0.1.3
* functions `add_lab_dummies()` and `add_lab_dumm1()` (with aliases `ald()` and `ald1()`) added. These generate a dummy variable for each label of a value-labeled variable and return the supplied data.frame with these dummy variables added.

* functions `factor_to_lab_int()` (with alias `f2int()`) and `lab_int_to_factor()` (with alias `int2f()`) added to allow easy conversion of labeled integer variables to factors and of factors to labeled integer variables.

* function `axis_lab()` (with alias `alb()`) added - helper that allows one to easily use variable name labels as y or x axis labels with Base R plots or ggplot2.

* function `all_quant_labs()` (with alias `allq()`) added: automatically add quantile-based numerical range value labels for all numeric variables that meet specifications.

* function `tabl()` updated. labs.on = TRUE is now the default. qtiles argument added that allows for non-value-labeled, many-valued numeric variables to be converted temporarily and on the fly to quantile category variables, so that they can be included in returned table results. Tabulation "engine" now relies on base `table()`, which makes select tabulations much faster.

* function `use_val_lab1()` (with alias `uvl1()`) added. This provides the same functionality as `use_val_labs()`, but allows the user to supply only one unquoted variable, whereas `use_val_labs()` requires quoted variable name arguments but allows the user to pass a vector of multiple variable names in one call. 

* function `add_lab_col1()` (with alias `alc1()`) added. This provides the same functionality as `add_lab_cols()`, but allows the user to supply only one unquoted variable, whereas `add_lab_cols()` requires quoted variable name arguments but allows the user to pass a vector of multiple variable names in one call. 

* updated `add_name_labs()`, `drop_name_labs()`, `convert_labs()`, and `clean_data_atts()` functions, so that changes (add or drop) of name.labs attributes are applied to native labels() attribute and, in case of `convert_labs()`, so that any incoming variable/column level labels() attributes (e.g., column name labels from a haven-imported tibble) are converted to labelr name.labs. Primary benefit of this is that RStudio View() will now show name labels underneath column names.

* `use_val_labs()`, `add_lab_cols()`, and `val_labs_vec()` fixed to convert any irregular character values to NA labels (was converting to "NA").

* `tabl()` wide.cols (pivot, cross-tab) functionality improved to show 0 instead of NA for empty cross-tab cell counts.

* `add_quant_labs()` and `add_quant1()` now round non-integer auto-generated quantile value labels to avoid long, repeating-decimal value labels (e.g., "q067" instead of "q066.66666...").

* updated `ssort()` to preserve rownames.

* updated `gremlr()`, `greml()`, `slab()`, `sbrac()`, `somel()`, `taill()`, `sfilter()`, and `ssort()` to handle situations involving (or producing) one-column data.frames.

* Defensive programming improvements made to various value-label-related functions, so that they respond more gracefully and informatively to non-valid variable name arguments. 

* split up original one vignette into two respective vignettes: Introduction and Special Topics.

* light copy-editing updates to README, vignette, and documentation. 
