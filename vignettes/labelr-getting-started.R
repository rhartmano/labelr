## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  error = TRUE,
  comment = "### >"
)

## ----setup--------------------------------------------------------------------
# install.packages("devtools")
# devtools::install_github("rhartmano/labelr")
library(labelr)

## -----------------------------------------------------------------------------
set.seed(555) # for reproducibility
df <- make_demo_data(n = 1000) # you can specify the number of fictional obs.

# make a backup for later comparison
df_copy <- df

## -----------------------------------------------------------------------------
df <- add_frame_lab(df, frame.lab = "Demographic and reaction time test score
                    records collected by Royal Statistical Agency of
                    Fictionaslavica. Data fictionally collected in the year
                    1987. As published in A. Smithee (1988). Some Fictional Data
                    for Your Amusement. Mad Magazine, 10(1), 1-24.")


get_frame_lab(df)

## -----------------------------------------------------------------------------
df <- add_name_labs(df, name.labs = c(
  "age" = "Age in years",
  "raceth" = "Racial/ethnic identity group category",
  "gender" = "Gender identity category",
  "edu" = "Highest education level attained",
  "x1" = "Space Invaders reaction time test scores",
  "x2" = "Galaga reaction time test scores"
))

## -----------------------------------------------------------------------------
get_name_labs(df)

## -----------------------------------------------------------------------------
df <- add_val_labs(df, # data.frame with to-be-value-labeled column
  vars = "raceth", # quoted variable name of to-be-labeled variable/column
  vals = c(1:7), # label values 1 through 7, inclusive
  labs = c(
    "White", "Black", "Hispanic", # apply these labels in this order to vals 1-7
    "Asian", "AIAN", "Multi", "Other"
  ),
  max.unique.vals = 10 # maximum number of unique values permitted
)

## -----------------------------------------------------------------------------
df <- add_val1(
  data = df,
  var = gender, # contrast this var argument to the vars argument demo'd above
  vals = c(0, 1, 2), # the values to be labeled
  labs = c("Male", "Female", "Other"), # the labels, applied in order to the vals
  max.unique.vals = 10
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
df <- add_quant1(df, # data.frame
  x1, # variable to value-label
  qtiles = 5
) # number quintiles to define numerical range labels

## -----------------------------------------------------------------------------
df <- add_m1_lab(df, "edu", vals = c(3:5), lab = "Some College+")

df <- add_m1_lab(df, "edu", vals = 1, lab = "Not HS Grad")

df <- add_m1_lab(df, "edu", vals = 2, lab = "HSG, No College")

get_val_labs(df)

## -----------------------------------------------------------------------------
head(df_copy, 3) # our pre-labeling copy of the data.frame

head(df, 3) # our latest, post-labeling version of same data.frame

## -----------------------------------------------------------------------------
labs.df <- get_all_lab_atts(df)

## -----------------------------------------------------------------------------
df <- strip_labs(df) # remove our labels
get_all_lab_atts(df) # show that they're gone

## -----------------------------------------------------------------------------
df <- add_lab_atts(df, labs.df)

get_all_lab_atts(df)

## -----------------------------------------------------------------------------
head(df, 5) # Base R function utils::head()

headl(df, 5) # labelr function headl() (note the "l")

tail(df, 5) # Base R function utils::tail()

taill(df, 5) # labelr function taill() (note the extra "l")

set.seed(293)
car::some(df, 5) # car package function car::some()

set.seed(293)
somel(df, 5) # labelr function somel() (note the "l")

## -----------------------------------------------------------------------------
use_val_labs(df)[1:20, ] # headl() is just a more compact shortcut for this

## -----------------------------------------------------------------------------
# `collapse::qsu()`
# with labels "off" (i.e., using regular values of "raceth" as by var)
(by_demog_val <- collapse::qsu(df, cols = c("x2"), by = ~raceth))

# with labels "on" (i.e., using labels, thanks to `uvl()`)
(by_demog_lab <- collapse::qsu(uvl(df), cols = c("x2"), by = ~raceth))

## -----------------------------------------------------------------------------
with(df, table(gender, raceth)) # base::with()

with_val_labs(df, table(gender, raceth)) # labelr::with_val_labs()

wvl(df, table(gender, raceth)) # labelr::wvl is a more compact alias

with(use_val_labs(df), table(gender, raceth)) # this does same thing

## -----------------------------------------------------------------------------
df_labd <- use_val_labs(df)
head(df_labd) # note, this is utils::head(), not labelr::headl()

## -----------------------------------------------------------------------------
df_plus_labs <- add_lab_cols(df)
head(df_plus_labs[c("gender", "gender_lab", "raceth", "raceth_lab")])

## -----------------------------------------------------------------------------
head(df)

df1 <- flab(df, raceth == "Asian" & gender == "Female")

head(df1, 5) # returned df1 is in terms of values, just like df

headl(df1, 5) # note use of labelr::headl; labels are there

## -----------------------------------------------------------------------------
head(slab(df, raceth == "Black" & gender == "Male", gender, raceth), 10)

## -----------------------------------------------------------------------------
head(tabl(df), 20) # labs.on = FALSE is default

## -----------------------------------------------------------------------------
head(tabl(df, labs.on = TRUE), 20) # labs.on = TRUE is not the default

## -----------------------------------------------------------------------------
head(tabl(df, labs.on = TRUE, prop.digits = 3), 20)

## -----------------------------------------------------------------------------
head(tabl(df, labs.on = TRUE, wide.col = "gender"), 20)

## -----------------------------------------------------------------------------
tabl(iris, "Species") # explicit vars arg with one-var ("Species")

tabl(mtcars, zero.rm = TRUE) # vars arg null

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
mt2 <- add_name_labs(mtcars,
  vars = names(names_labs_vec),
  labs = names_labs_vec
)

## -----------------------------------------------------------------------------
mt2 <- add_name_labs(mtcars,
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
mt2 <- use_name_labs(mt2)

head(mt2[c(1, 2)])

## -----------------------------------------------------------------------------
lm(`Miles/(US) gallon` ~ `Number of cylinders`, data = mt2) # pasting in var names
lm(mpg ~ cyl, data = use_var_names(mt2)) # same result if name labels are "off"

## -----------------------------------------------------------------------------
sapply(mt2, median) # get the median for every name-labeled variable

collapse::qsu(mt2) # use an external package for more informative descriptives

## -----------------------------------------------------------------------------
mt2 <- use_var_names(mt2)
head(mt2[c(1, 2)])

## -----------------------------------------------------------------------------
# first, show mt2 with name labels off but verify that we still have name labels
head(mt2)
get_name_labs(mt2)

## -----------------------------------------------------------------------------
# demo wnl() (note that with_name_labs() will achieve same result)
wnl(mt2, t.test(mpg ~ am))

wnl(mt2, lm(mpg ~ am))

wnl(mt2, summary(mt2))

wnl(mt2, xtabs(~gear))

with(mt2, xtabs(~gear)) # compare to directly above

## -----------------------------------------------------------------------------
mtbad <- mtcars

## -----------------------------------------------------------------------------
mtbad[1, 1:11] <- NA
rownames(mtbad)[1] <- "Missing Car"
mtbad[2, "am"] <- Inf
mtbad[3, "gear"] <- -Inf
mtbad[5, "carb"] <- NaN
mtbad[2, "mpg"] <- Inf
mtbad[3, "mpg"] <- NaN

# add a character variable, for demonstration purposes
# if it makes you feel better, you can pretend these are Consumer Reports or
# ...JD Power ratings or something
set.seed(9202) # for reproducibility
mtbad$grade <- sample(c("A", "B", "C"), nrow(mtbad), replace = TRUE)
mtbad[4, "grade"] <- NA
mtbad[5, "grade"] <- "NA"
mtbad[6, "grade"] <- "Inf"

# see where this leaves us
head(mtbad)

sapply(mtbad, class)

## -----------------------------------------------------------------------------
mtlabs <- mtbad |>
  add_val1(grade,
    vals = c("A", "B", "C"),
    labs = c("Gold", "Silver", "Bronze")
  ) |>
  add_val1(am,
    vals = c(0, 1),
    labs = c("auto", "stick")
  ) |>
  add_val1(carb,
    vals = c(1, 2, 3, 4, 6, 8), # not the most inspired use of labels
    labs = c(
      "1c", "2c", "3c",
      "4c", "6c", "8c"
    )
  ) |>
  add_val1(gear,
    vals = 3:5, # again, not the most compelling use case
    labs = c(
      "3-speed",
      "4-speed",
      "5-speed"
    )
  ) |>
  add_quant1(mpg, qtiles = 4) # add quartile-based value labels

## -----------------------------------------------------------------------------
get_val_labs(mtlabs, "am") # NA values were detected and dealt with

## -----------------------------------------------------------------------------
mtless <- sselect(mtlabs, mpg, cyl, am, gear, carb, grade) # safely select

head(mtless, 5) # note that the irregular values are still here

## -----------------------------------------------------------------------------
head(use_val_labs(mtless), 5) # but they all go to NA if we `use_val_labs`

## -----------------------------------------------------------------------------
mtlabs_plus <- add_lab_cols(mtlabs, c("mpg", "am")) # this creates and adds "am_lab" col
mtlabs_plus <- sselect(mtlabs_plus, mpg, mpg_lab, am, am_lab) # let's select down to these two

head(mtlabs_plus) # here's where we landed

## -----------------------------------------------------------------------------
# Trying to Label an Irregular Value (-Inf)
mtbad <- add_val1(
  data = mtcars,
  var = gear,
  vals = -Inf,
  labs = c("neg.inf")
)

# Trying to Label an Irregular Value (NA)
mtbad <- add_val_labs(
  data = mtbad,
  vars = "grade",
  vals = NA,
  labs = c("miss")
)

# Trying to Label an Irregular Value (NaN)
mtbad <- add_val_labs(
  data = mtbad,
  vars = "carb",
  vals = NaN,
  labs = c("nan-v")
)

# labelr also treats "character variants" of irregular values as irregular values.
mtbad <- add_val1(
  data = mtbad,
  var = carb,
  vals = "NAN",
  labs = c("nan-v")
)

## -----------------------------------------------------------------------------
unique(iris$Species)

sapply(iris, class) # nothing up our sleeve -- "Species" is a factor

## -----------------------------------------------------------------------------
irlab <- add_val_labs(iris,
  vars = "Species",
  vals = c("setosa", "versicolor", "virginica"),
  labs = c("se", "ve", "vi")
)

# this also would've worked
# irlab_dos <- add_val1(iris, Species,
#   vals = c("setosa", "versicolor", "virginica"),
#   labs = c("se", "ve", "vi")
# )

## -----------------------------------------------------------------------------
summary(iris)

summary(irlab)

head(iris, 4)

head(irlab, 4)

lm(Sepal.Length ~ Sepal.Width + Species, data = iris)

lm(Sepal.Length ~ Sepal.Width + Species, data = irlab) # values are same

## -----------------------------------------------------------------------------
sapply(irlab, class)

levels(irlab$Species)

## -----------------------------------------------------------------------------
get_val_labs(irlab, "Species")

## -----------------------------------------------------------------------------
head(use_val_labs(irlab))
ir_v <- flab(irlab, Species == "vi")
head(ir_v, 5)

## -----------------------------------------------------------------------------
irlab_aug <- add_lab_cols(irlab, vars = "Species")

## -----------------------------------------------------------------------------
set.seed(231)
sample_rows <- sample(seq_len(nrow(irlab)), 10, replace = FALSE)

irlab_aug[sample_rows, ]

sapply(irlab_aug, class)

with(irlab_aug, table(Species, Species_lab))

## -----------------------------------------------------------------------------
ir_char <- use_val_labs(irlab) # we assign this to a new data.frame
sapply(ir_char, class)

head(ir_char, 3)

class(ir_char$Species) # it's character

## -----------------------------------------------------------------------------
ir_fact <- use_val_labs(irlab)

ir_fact$Species <- factor(ir_char$Species,
  levels = c("se", "ve", "vi"),
  labels = c("se", "ve", "vi")
)
head(ir_fact, 3)

class(ir_fact$Species) # it's a factor

levels(ir_fact$Species) # it's a factor

## -----------------------------------------------------------------------------
with(ir_fact, tapply(Sepal.Width, Species, mean))
with(irlab, tapply(Sepal.Width, Species, mean))
with(iris, tapply(Sepal.Width, Species, mean))

## -----------------------------------------------------------------------------
ir_ord <- iris

set.seed(293)
qrating <- c("AAA", "AA", "A", "BBB", "AA", "BBB", "A")

ir_ord$qrat <- sample(qrating, 150, replace = TRUE)

ir_ord$qrat <- factor(ir_ord$qrat,
  ordered = TRUE,
  levels = c("AAA", "AA", "A", "BBB")
)

## -----------------------------------------------------------------------------
levels(ir_ord$qrat)

class(ir_ord$qrat)

## -----------------------------------------------------------------------------
ir_ord <- add_val_labs(ir_ord,
  vars = "qrat",
  vals = c("AAA", "AA", "A", "BBB"),
  labs = c(
    "unimpeachable",
    "excellent",
    "very good",
    "meh"
  )
)

## -----------------------------------------------------------------------------
ir_ord <- add_lab_cols(ir_ord, vars = "qrat")

head(ir_ord, 10)

with(ir_ord, table(qrat_lab, qrat))

class(ir_ord$qrat)

levels(ir_ord$qrat)

class(ir_ord$qrat_lab)

get_val_labs(ir_ord, "qrat") # labs are still there for qrat

get_val_labs(ir_ord, "qrat_lab") # no labs here; this is just a character var

## -----------------------------------------------------------------------------
opening_ding <- Sys.time() # to time labelr

library(nycflights13)

## -----------------------------------------------------------------------------
df <- flights

nrow(df)

## -----------------------------------------------------------------------------
df <- add_frame_lab(df, frame.lab = "On-time data for all flights that
                    departed NYC (i.e. JFK, LGA or EWR) in 2013.")

## -----------------------------------------------------------------------------
attr(df, "frame.lab") # check for attribute

get_frame_lab(df) # return frame.lab alongside data.frame name as a data.frame

get_frame_lab(df)$frame.lab

## -----------------------------------------------------------------------------
names_labs_vec <- c(
  "year" = "Year of departure",
  "month" = "Month of departure",
  "year" = "Day of departure",
  "dep_time" = "Actual departure time (format HHMM or HMM), local tz",
  "arr_time" = "Actual arrival time (format HHMM or HMM), local tz",
  "sched_dep_time" = "Scheduled departure times (format HHMM or HMM)",
  "sched_arr_time" = "Scheduled arrival time (format HHMM or HMM)",
  "dep_delay" = "Departure delays, in minutes",
  "arr_delay" = "Arrival delays, in minutes",
  "carrier" = "Two letter airline carrier abbreviation",
  "flight" = "Flight number",
  "tailnum" = "Plane tail number",
  "origin" = "Flight origin airport code",
  "dest" = "Flight destination airport code",
  "air_time" = "Minutes spent in the air",
  "distance" = "Miles between airports",
  "hour" = "Hour of scheduled departure time",
  "minute" = "Minutes component of scheduled departure time",
  "time_hour" = "Scheduled date and hour of the flight as a POSIXct date"
)

df <- add_name_labs(df, name.labs = names_labs_vec)

get_name_labs(df) # show that they've been added

## -----------------------------------------------------------------------------
airlines <- nycflights13::airlines

head(airlines)

## -----------------------------------------------------------------------------
ny_val <- airlines$carrier

## -----------------------------------------------------------------------------
ny_lab <- airlines$name

## -----------------------------------------------------------------------------
ny_month_vals <- c(1:12) # values
ny_month_labs <- c(
  "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"
) # labels

## -----------------------------------------------------------------------------
df <- add_val1(df,
  var = carrier, vals = ny_val,
  labs = ny_lab,
  max.unique.vals = 20
)

## -----------------------------------------------------------------------------
df <- add_val_labs(df,
  vars = "month",
  vals = ny_month_vals,
  labs = ny_month_labs,
  max.unique.vals = 20
)

## -----------------------------------------------------------------------------
df <- add_quant_labs(df, "dep_time", qtiles = 5)

## -----------------------------------------------------------------------------
get_val_labs(df)

## -----------------------------------------------------------------------------
head(df[c("origin", "dep_time", "dest", "year", "month", "carrier")])

## -----------------------------------------------------------------------------
df_swapd <- use_val_labs(df)

head(df_swapd[c("origin", "dep_time", "dest", "year", "month", "carrier")])

## -----------------------------------------------------------------------------
df_plus <- add_lab_cols(df, vars = c("carrier", "month", "dep_time"))

head(df_plus[c(
  "origin", "dest", "year",
  "month", "month_lab",
  "dep_time", "dep_time_lab",
  "carrier", "carrier_lab"
)])

## -----------------------------------------------------------------------------
# labels are not visible (they exist only as attributes() meta-data)
head(df[c("carrier", "arr_delay")])

# we still can use them to filter (note: we're filtering on "JetBlue Airways",
# ...NOT its obscure code "B6")
df_fl <- flab(df, carrier == "JetBlue Airways" & arr_delay > 20)

# here's what's returned when we filtered on "JetBlue Airways" using flab()
head(df_fl[c("carrier", "arr_delay")])

# double-check that this is JetBlue
head(use_val_labs(df_fl)[c("carrier", "arr_delay")])

## -----------------------------------------------------------------------------
the_buzzer <- Sys.time()
the_buzzer - opening_ding

## -----------------------------------------------------------------------------
set.seed(272) # for reproducibility
dflik <- make_likert_data(scale = 1:7) # another labelr function
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
  partial = TRUE # applying to all cols with "x" or "y3" substring in names
)

## -----------------------------------------------------------------------------
head(dflik)

## -----------------------------------------------------------------------------
lik1 <- uvl(dflik) # assign to new object, since we can't "undo"
head(lik1) # we could have skipped previous call by using labelr::headl(dflik)

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
  partial = TRUE # note
)

## -----------------------------------------------------------------------------
get_val_labs(dfxgone)

