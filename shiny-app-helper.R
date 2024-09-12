# Load packages for data analysis
library(tidyverse)

# Import data
raw_data <- read_csv("data/cps_women_lfp.csv")

# Reorder factors
data <- raw_data |>
  mutate_if(is_character, as_factor) |>
  mutate(
    education = fct_relevel(
      education, "< HS Diploma", "HS Diploma",
      "Some college, no degree", "Associate's Degree",
      "Bachelor's Degree", "Master's or Higher"
    ),
    age = fct_relevel(
      age, "< 25", "25-34", "35-44", "45-54",
      "55-64", "65-74", "75+"
    ),
    wageinc_quantiles = fct_relevel(
      wageinc_quantiles, "0-19.99", "20-39.99",
      "40-59.99", "60-79.99", "80-100"
    ),
    income_quantiles = fct_relevel(
      income_quantiles, "0-19.99", "20-39.99",
      "40-59.99", "60-79.99", "80-100"
    )
  )

# Create new variables and filter dataset
lfp <- data |>
  # Since LFP is the variable of interest, filter out NAs
  filter(!is.na(lfp)) |>
  # Create new variables
  mutate(
    lfp_lgl = lfp == "In labor force",
    college_lgl = college == "Has college degree",
    # This logical is true if we know the individual is self-employed,
    # but false otherwise, including if there is a missing value
    self_employed_lgl = if_else(self_employed == "Self-employed",
      TRUE, FALSE, FALSE
    ),
    lfp_lgl_excl_self = !self_employed_lgl & lfp_lgl,
    employed_lgl = employed == "Employed",
    lfp_lgl = lfp == "In labor force",
    covid_tw_lgl = covid_telework == "Telework from 2021-2022 due to COVID",
    inctot = if_else(inctot == 0, NA, inctot),
    income = if_else(income == 0, NA, income),
    incss = if_else(incss == 0, NA, incss)
  )

# Create filtered data set of women only
women <- lfp |>
  filter(sex == "Female")

# Create filtered data set of women over 25 only
women_over_25 <- women |>
  filter(age != "< 25" & !is.na(age)) |>
  select(!c(
    cpsidp,
    sex,
    wageinc_quantiles,
    employed,
    self_employed,
    covid_unemployed,
    covid_paid,
    covid_telework,
    telework_now,
    telework_before,
    telework_difference,
    self_employed_lgl,
    lfp_lgl_excl_self,
    employed_lgl,
    covid_tw_lgl
  ))

# Summarize the data set by year
women_over_25_by_year <- women_over_25 |>
  summarize(
    .by = c(year, race, age, income_quantiles, education, college),
    across(c(lfp_lgl, inctot, income, incss, college_lgl),
           \(x) sum(wgt[!is.na(x)]),
           .names = "{.col}_wgt"),
    across(c(lfp_lgl, inctot, income, incss, college_lgl),
           \(x) weighted.mean(x, wgt, na.rm = TRUE)),
    wgt = sum(wgt)
  )

# Write the new data set to a new file under the Shiny app folder
write_csv(women_over_25_by_year, "shiny-app/data/women-over-25-by-year.csv")
