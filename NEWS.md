# labelr (development version)

# labelr 1.0.3
* updated `slab()` logic to better handle operations that would result in one-column data.frames.

* updated `gremlr()` and `greml()` to handle situations involving one-column data.frames

* updated `add_name_labs()`, `drop_name_labs()`, `convert_labs()`, and `clean_data_atts()` functions, so that changes (add or drop) of name.labs attributes are applied to native labels() attribute and, in case of `convert_labs()`, so that any incoming variable/column level labels() attributes (e.g., column name labels from a haven-imported tibble) are converted to labelr name.labs. Primary benefit of this is that RStudio View() will now show name labels underneath column names.

* functions `factor_to_lab_int()` (with alias `f2int()`) and `lab_int_to_factor()` (with alias `int2f()`) added to allow easy conversion of labeled integer variables to factors and of factors to labeled integer variables.

* function `axis_lab()` (with alias `alb()`) added - helper that allows one to easily use variable name labels as y or x axis labels with Base R plots or ggplot2.

* `use_val_labs()`, `add_lab_cols()`, and `val_labs_vec()` fixed to convert any irregular character values to NA labels (was converting to "NA").

* `as_base_data_frame()` title corrected (was a duplicate of `as_num()`).

* light copy-editing updates to README, vignette, and documentation. 
