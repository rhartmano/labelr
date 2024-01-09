## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "### >"
)

## -----------------------------------------------------------------------------
library(labelr)

## -----------------------------------------------------------------------------
mt2 <- mtcars

## -----------------------------------------------------------------------------
mt2 <- add_frame_lab(mt2, frame.lab = "Data extracted from the 1974 Motor
Trend US magazine, comprising fuel consumption and 10 aspects of automobile
design and performance for 32 automobiles (1973–74 models). Source: Henderson
and Velleman (1981), Building multiple regression models interactively.
 Biometrics, 37, 391–411.")

## -----------------------------------------------------------------------------
mt2 <- add_name_labs(mt2,
  name.labs = c(
    "mpg" = "Miles/(US) gallon",
    "cyl" = "Number of cylinders",
    "disp" = "Displacement (cu.in.)",
    "hp" = "Gross horsepower",
    "drat" = "Rear axle ratio",
    "wt" = "Weight (1000 lbs)",
    "qsec" = "1/4 mile time",
    "vs" = "Engine (0 = V-shaped, 1 = straight)",
    "am" = "Transmission (0 = automatic, 1 = manual)",
    "gear" = "Number of forward gears",
    "carb" = "Number of carburetors"
  )
)

## -----------------------------------------------------------------------------
mt2 <- add_val_labs(
  data = mt2,
  vars = "am",
  vals = c(0, 1),
  labs = c("automatic", "manual")
)

mt2 <- add_val_labs(
  data = mt2,
  vars = "carb",
  vals = c(1, 2, 3, 4, 6, 8),
  labs = c(
    "1-carb", "2-carbs",
    "3-carbs", "4-carbs",
    "6-carbs", "8-carbs"
  )
)

# var arg can be unquoted if using add_val1()
# note that this is not add_val_labs(); add_val1() has "var" arg instead of "vars
mt2 <- add_val1(
  data = mt2,
  var = cyl, # note, "var," not "vars" arg
  vals = c(4, 6, 8),
  labs = c(
    "four-cyl",
    "six-cyl",
    "eight-cyl"
  )
)

## -----------------------------------------------------------------------------
mt2 <- add_m1_lab(
  data = mt2,
  vars = "gear",
  vals = 3,
  lab = "3"
)

mt2 <- add_m1_lab(
  data = mt2,
  vars = "gear",
  vals = c(4, 5),
  lab = "4+"
)

## -----------------------------------------------------------------------------
mt2 <- add_quant_labs(
  data = mt2,
  vars = "disp",
  qtiles = 3
) # we'll label "disp" values using three quantiles

# get "pretty" value thresholds for "mpg"
mpg_bins <- pretty(c(min(mt2$mpg), max(mt2$mpg)))
mpg_bins

mt2 <- add_quant_labs(
  data = mt2,
  vars = "mpg",
  vals = mpg_bins
) # assign labels using our "pretty" thresholds

## -----------------------------------------------------------------------------
get_frame_lab(mt2)

get_name_labs(mt2)

get_val_labs(mt2)

## -----------------------------------------------------------------------------
get_all_lab_atts(mt2)

## -----------------------------------------------------------------------------
zlab.mt2 <- get_all_lab_atts(mt2)

## -----------------------------------------------------------------------------
mt2 <- strip_labs(mt2)

## -----------------------------------------------------------------------------
get_all_lab_atts(mt2)

get_val_labs(mt2)

get_name_labs(mt2)

get_frame_lab(mt2)

## -----------------------------------------------------------------------------
mt2 <- add_lab_atts(mt2, zlab.mt2)

get_all_lab_atts(mt2)

get_val_labs(mt2)

get_name_labs(mt2)

get_frame_lab(mt2)

## -----------------------------------------------------------------------------
mtgone <- subset(mt2, am == 0 & gear <= 5)

mtlater <- mt2[, c("am", "gear", "cyl")]

sapply(list(mtgone, mtlater), get_all_lab_atts)

## -----------------------------------------------------------------------------
mtback <- add_lab_atts(mtgone, zlab.mt2)
mtnotsofast <- add_lab_atts(mtlater, zlab.mt2)

head(use_val_labs(mtback), 3)
head(use_val_labs(mtnotsofast), 3)

