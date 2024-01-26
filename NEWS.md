# labelr (development version)

# labelr 1.0.3
* functions `factor_to_lab_int()` (with alias `f2int()`) and `lab_int_to_factor()` (with alias `int2f()`) added to allow easy conversion of labeled integer variables to factors and of factors to labeled integer variables.

* function `axis_lab()` (with alias `alb()`) added - helper that allows one to easily use variable name labels as y or x axis labels with Base R plots or ggplot2

* `use_val_labs()`, `add_lab_cols()`, and `val_labs_vec()` fixed to convert any irregular character values to NA labels (was converting to "NA")

* `as_base_data_frame()` title corrected (was a duplicate of `as_num()`)

* light copy-editing updates to README, vignette, and documentation 
