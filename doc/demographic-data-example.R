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

head(df)

## -----------------------------------------------------------------------------
df <- add_val_labs(df,
  vars = "raceth", vals = c(1:7),
  labs = c(
    "White", "Black", "Latino",
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
df_temp <- add_quant_labs(data = df, vars = "x", qtiles = 5, partial = TRUE)

head(use_val_labs(df_temp), 5)

## -----------------------------------------------------------------------------
df_temp <- add_quant_labs(
  data = df_temp, vars = "x", vals = c(100, 150),
  partial = TRUE
)

head(use_val_labs(df_temp), 5)

## -----------------------------------------------------------------------------
df <- add_quant1(df, x1, qtiles = 5)

head(use_val_labs(df), 5)

## -----------------------------------------------------------------------------
df <- add_m1_lab(df, "edu", vals = c(3:5), lab = "Some College+")

df <- add_m1_lab(df, "edu", vals = 1, lab = "Not HS Grad")

df <- add_m1_lab(df, "edu", vals = 2, lab = "HSG, No College")

get_val_labs(df)

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
df <- add_frame_lab(df, frame.lab = "Demographic and reaction time test score
                    records collected by Royal Statistical Agency of
                    Fictionaslaviaca. Data fictionally collected in the year
                    1987. As published in A. Smithee (1988). Some Fictional Data
                    for Your Amusement. Mad Magazine, 10(1), 1-24.")


get_frame_lab(df)

## -----------------------------------------------------------------------------
head(df)

## -----------------------------------------------------------------------------
get_frame_lab(df)

get_name_labs(df)

get_val_labs(df)

## -----------------------------------------------------------------------------
get_name_labs(df, "gender")

get_val_labs(df, "gender")

## -----------------------------------------------------------------------------
zlab.df <- get_all_lab_atts(df)

## -----------------------------------------------------------------------------
df <- strip_labs(df)
get_all_lab_atts(df) # nothing there

## -----------------------------------------------------------------------------
df <- add_lab_atts(df, zlab.df)
get_all_lab_atts(df)

## -----------------------------------------------------------------------------
head(use_val_labs(df), 5)

## -----------------------------------------------------------------------------
with(df, table(gender, raceth))

with_val_labs(df, table(gender, raceth))

with(use_val_labs(df), table(gender, raceth))

## -----------------------------------------------------------------------------
# xtabs typical output
xtabs(~raceth, data = df)

# with df wrapped in `uvl()` or `use_val_labs()`
xtabs(~raceth, data = uvl(df))

## -----------------------------------------------------------------------------
# `collapse::qsu()`
# with labels "off" (i.e., using regular values of "raceth" as by var)
by_demog_val <- collapse::qsu(df, cols = c("x2"), by = ~raceth)
by_demog_val

# with labels "on" (i.e., using labels, thanks to `uvl()` aka `use_val_labs()`)
by_demog_lab <- collapse::qsu(uvl(df), cols = c("x2"), by = ~raceth)
by_demog_lab

## -----------------------------------------------------------------------------
wvl(df, table(gender, raceth)) # compact alias for with_val_labs

## -----------------------------------------------------------------------------
# verify that we still have name labels
get_name_labs(df)

# demo wnl() (note that with_name_labs() will achieve same result)
wnl(df, xtabs(x1 ~ gender)) # data arg always goes first, followed by experession

wnl(df, aov(x1 ~ gender)) # data arg always goes first, followed by experession

wnl(df, lm(x2 ~ gender * raceth + age))

## -----------------------------------------------------------------------------
invisible(with(df, hist(x1))) # using with()

invisible(wnl(df, hist(x1))) # using wnl(); compare to previous histogram

## -----------------------------------------------------------------------------
invisible(with(df, plot(x1, x2))) # using with()

invisible(wnl(df, plot(x1, x2))) # using wnl(); compare to previous scatterplot

## -----------------------------------------------------------------------------
df_swap <- use_val_labs(df)

head(df_swap)

## -----------------------------------------------------------------------------
df_plus <- add_lab_cols(df)

head(df_plus[c("raceth", "raceth_lab", "gender", "gender_lab")])

## -----------------------------------------------------------------------------
head(df, 5) # Base R function utils::head()

headl(df, 5) # labelr function headl() (note the "l")

tail(df, 5) # Base R function utils::tail()

taill(df, 5) # labelr function taill() (note the "l")

car::some(df, 5) # car package function car::some()

somel(df, 5, seed = 1837) # labelr function somel() (note the "l")

## -----------------------------------------------------------------------------
head(df) # value labels can work for use even when we don't see them
df1 <- flab(df, raceth == "Asian" & gender == "Female")
head(df1)

## -----------------------------------------------------------------------------
head(slab(df, raceth == "Black" & gender == "Male", gender, raceth), 10)

## -----------------------------------------------------------------------------
head(tabl(df, vars = "raceth"), 20)

## -----------------------------------------------------------------------------
head(tabl(df, vars = "raceth", labs.on = TRUE), 20)

## -----------------------------------------------------------------------------
head(tabl(df, labs.on = TRUE), 20)

## -----------------------------------------------------------------------------
head(tabl(df, labs.on = TRUE, prop.digits = 3), 20)

## -----------------------------------------------------------------------------
head(tabl(df, labs.on = TRUE, wide.col = "gender"), 20)

