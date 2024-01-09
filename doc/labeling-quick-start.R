## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "### >",
  fig.width = 7,
  fig.height = 4.7
)

## ----setup--------------------------------------------------------------------
library(labelr)

## -----------------------------------------------------------------------------
df <- make_demo_data(n = 1000, seed = 555)

## -----------------------------------------------------------------------------
df <- add_frame_lab(df, frame.lab = "Demographic and reaction time test score
                    records collected by Royal Statistical Agency of
                    Fictionaslaviaca. Data fictionally collected in the year
                    1987. As published in A. Smithee (1988). Some Fictional Data
                    for Your Amusement. Mad Magazine, 10(1), 1-24.")


get_frame_lab(df)

## -----------------------------------------------------------------------------
df <- add_name_labs(df, name.labs = c(
  "age" = "Age in years",
  "raceth" = "Racial/ethnic identity group category",
  "gender" = "Gender identity",
  "edu" = "Highest education level attained",
  "x1" = "Space Invaders reaction time test scores",
  "x2" = "Galaga reaction time test scores"
))

## -----------------------------------------------------------------------------
get_name_labs(df)

## -----------------------------------------------------------------------------
df <- add_val_labs(df,
  vars = "raceth", vals = c(1:7),
  labs = c(
    "White", "Black", "Hispanic",
    "Asian", "AIAN", "Multi", "Other"
  ),
  max.unique.vals = 10
)

## -----------------------------------------------------------------------------
df <- add_val1(
  data = df, gender, vals = c(0, 1, 2),
  labs = c("Male", "Female", "Other"), max.unique.vals = 10
)

## -----------------------------------------------------------------------------
get_val_labs(df)

## -----------------------------------------------------------------------------
df_temp <- add_quant_labs(data = df, vars = "x", qtiles = 5, partial = TRUE)

## -----------------------------------------------------------------------------
df_temp <- add_quant_labs(
  data = df_temp, vars = "x", vals = c(100, 150),
  partial = TRUE
)

## -----------------------------------------------------------------------------
df <- add_quant1(df, x1, qtiles = 5)

## -----------------------------------------------------------------------------
df <- add_m1_lab(df, "edu", vals = c(3:5), lab = "Some College+")

df <- add_m1_lab(df, "edu", vals = 1, lab = "Not HS Grad")

df <- add_m1_lab(df, "edu", vals = 2, lab = "HSG, No College")

get_val_labs(df)

## -----------------------------------------------------------------------------
head(df)

## -----------------------------------------------------------------------------
head(use_val_labs(df), 5)

## -----------------------------------------------------------------------------
df_labd <- use_val_labs(df)

## -----------------------------------------------------------------------------
with(df, table(gender, raceth))

with_val_labs(df, table(gender, raceth))

wvl(df, table(gender, raceth)) # wvl() is an more compact alias

with(use_val_labs(df), table(gender, raceth))

## -----------------------------------------------------------------------------
head(df, 5) # Base R function utils::head()

headl(df, 5) # labelr function headl() (note the "l")

tail(df, 5) # Base R function utils::tail()

taill(df, 5) # labelr function taill() (note the "l")

car::some(df, 5) # car package function car::some()

somel(df, 5, seed = 1837) # labelr function somel() (note the "l")

## -----------------------------------------------------------------------------
df_plus_labs <- add_lab_cols(df)

head(df_plus_labs)

head(df_plus_labs[c("gender", "gender_lab", "raceth", "raceth_lab")])

## -----------------------------------------------------------------------------
head(df)

df1 <- flab(df, raceth == "Asian" & gender == "Female")

head(df1, 5)

head(use_val_labs(df1), 5)

## -----------------------------------------------------------------------------
head(slab(df, raceth == "Black" & gender == "Male", gender, raceth), 10)

## -----------------------------------------------------------------------------
head(tabl(df), 20)

## -----------------------------------------------------------------------------
head(tabl(df, labs.on = TRUE), 20)

## -----------------------------------------------------------------------------
head(tabl(df, labs.on = TRUE, prop.digits = 3), 20)

## -----------------------------------------------------------------------------
head(tabl(df, labs.on = TRUE, wide.col = "gender"), 20)

## -----------------------------------------------------------------------------
# `collapse::qsu()`
# with labels "off" (i.e., using regular values of "raceth" as by var)
by_demog_val <- collapse::qsu(df, cols = c("x2"), by = ~raceth)
by_demog_val

# with labels "on" (i.e., using labels, thanks to `uvl()`)
by_demog_lab <- collapse::qsu(uvl(df), cols = c("x2"), by = ~raceth)
by_demog_lab

## -----------------------------------------------------------------------------
dflik <- make_likert_data(scale = 1:7, seed = 272)
head(dflik)

## -----------------------------------------------------------------------------
vals2label <- 1:7
labs2use <- c(
  "VSD",
  "SD",
  "D",
  "N",
  "A",
  "SA",
  "VSA"
)

## -----------------------------------------------------------------------------
dflik <- add_val_labs(
  data = dflik, vars = c("x", "y3"), ###  note the vars args
  vals = vals2label,
  labs = labs2use,
  partial = TRUE
)

## -----------------------------------------------------------------------------
head(dflik)

## -----------------------------------------------------------------------------
lik1 <- use_val_labs(dflik)
head(lik1)

## -----------------------------------------------------------------------------
dfdrop <- drop_val_labs(dflik,
  c("x2", "y3"),
  partial = FALSE
)

## -----------------------------------------------------------------------------
get_val_labs(dfdrop, "x2")

## -----------------------------------------------------------------------------
get_val_labs(dfdrop, "x1")

## -----------------------------------------------------------------------------
dfxgone <- drop_val_labs(dflik,
  c("x"),
  partial = TRUE
)

## -----------------------------------------------------------------------------
get_val_labs(dfxgone)

## -----------------------------------------------------------------------------
mt2 <- mtcars

## -----------------------------------------------------------------------------
mt2[1, 1:11] <- NA
rownames(mt2)[1] <- "Missing Car"

## -----------------------------------------------------------------------------
mt2 <- add_frame_lab(mt2, frame.lab = "Data extracted from the 1974 Motor
Trend US magazine, comprising fuel consumption and 10 aspects of automobile
design and performance for 32 automobiles (1973–74 models). Source: Henderson
and Velleman (1981), Building multiple regression models interactively.
 Biometrics, 37, 391–411.")

## -----------------------------------------------------------------------------
attr(mt2, "frame.lab") # check for attribute

get_frame_lab(mt2) # return frame.lab alongside data.frame name as a data.frame

get_frame_lab(mt2)$frame.lab

## -----------------------------------------------------------------------------
names_labs_vec <- c(
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

## -----------------------------------------------------------------------------
mt2 <- add_name_labs(mt2,
  vars = names(names_labs_vec),
  labs = names_labs_vec
)

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
mt2 <- add_name_labs(mt2,
  name.labs = c(
    "disp" = toupper("UH OH!"),
    "mpg" = toupper("i, said, uh oh!!")
  )
)

## -----------------------------------------------------------------------------
get_name_labs(mt2)

get_name_labs(mt2, var = c("disp", "mpg"))

## -----------------------------------------------------------------------------
mt2 <- add_name_labs(mt2,
  vars = names(names_labs_vec),
  labs = names_labs_vec
)

get_name_labs(mt2)

## -----------------------------------------------------------------------------
mt2 <- use_name_labs(mt2)

## -----------------------------------------------------------------------------
head(mt2[c(1, 2)])

## -----------------------------------------------------------------------------
mt2 <- use_var_names(mt2)
head(mt2[c(1, 2)])

## -----------------------------------------------------------------------------
mt2 <- use_name_labs(mt2)
lm(`Miles/(US) gallon` ~ `Number of cylinders`, data = mt2)
lm(mpg ~ cyl, data = use_var_names(mt2))

## -----------------------------------------------------------------------------
mt2 <- use_var_names(mt2)

## -----------------------------------------------------------------------------
# verify that we still have name labels
get_name_labs(mt2)

# demo wnl() (note that with_name_labs() will achieve same result)
wnl(mt2, t.test(mpg ~ am))

wnl(mt2, lm(mpg ~ am))

wnl(mt2, summary(mt2))

wnl(mt2, xtabs(~gear))

xtabs(~ mt2$gear) # compare to directly above

## -----------------------------------------------------------------------------
invisible(wnl(mt2, hist(mpg))) # name label, not colname as title

invisible(wnl(mt2, plot(mpg, carb))) # name labels, not colnames, on axes

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

## -----------------------------------------------------------------------------
mt2 <- add_val1(
  data = mt2,
  var = cyl, ### note, "var," not "vars" arg
  vals = c(4, 6, 8),
  labs = c(
    "four-cyl",
    "six-cyl",
    "eight-cyl"
  )
)

## -----------------------------------------------------------------------------
mt2 <- add_val_labs(
  data = mt2,
  vars = "am",
  vals = c(99),
  labs = c("friend")
)

get_name_labs(mt2)

get_val_labs(mt2)

## -----------------------------------------------------------------------------
# for "disp", add numerical range labels based on quantiles (here, quintiles)
mt2 <- add_quant_labs(data = mt2, vars = "disp", qtiles = 5)

# for "mpg", add numerical range labels based on "pretty" cuts
# bound bins using range of "mpg" vals
mpg_bins <- pretty(range(mt2$mpg, na.rm = TRUE))
mpg_bins


mt2 <- add_quant_labs(
  data = mt2,
  vars = "mpg",
  vals = mpg_bins
) # assign labels using our "pretty" thresholds

head(use_val_labs(mt2), 4)

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
  vals = 4:5,
  lab = "4+"
)

## -----------------------------------------------------------------------------
zlab.mt2 <- get_all_lab_atts(mt2)

## -----------------------------------------------------------------------------
mt2 <- strip_labs(mt2)
get_all_lab_atts(mt2)

## -----------------------------------------------------------------------------
mt2 <- add_lab_atts(mt2, zlab.mt2)
get_all_lab_atts(mt2)

## -----------------------------------------------------------------------------
mt_pipe <- mtcars |>
  add_val1(am,
    vals = c(0, 1),
    labs = c("automatic", "manual")
  ) |>
  add_val1(cyl,
    vals = c(4, 6, 8),
    labs = c("four-cyl", "six-cyl", "eight-cyl")
  ) |>
  add1m1(gear,
    vals = 3,
    lab = "3"
  ) |>
  add1m1(gear,
    vals = c(4, 5),
    lab = c("4+")
  ) |>
  add_val1(carb,
    vals = c(1, 2, 3, 4, 6, 8),
    labs = c(
      "1-carb", "2-carbs", "3-carbs",
      "4-carbs", "6-carbs", "8-carbs"
    )
  ) |>
  add_quant1(mpg,
    qtiles = 4
  )

## -----------------------------------------------------------------------------
head(mt_pipe, 5) # Base R function utils::head()

headl(mt_pipe, 5) # labelr function headl() (note the "l")

tail(mt_pipe, 5) # Base R function utils::tail()

taill(mt_pipe, 5) # labelr function taill() (note the "l")

car::some(mt_pipe, 5) # car package function car::some()

somel(mt_pipe, 5, seed = 1837) # labelr function somel() (note the "l")

