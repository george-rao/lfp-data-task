library(shiny)
library(bslib)

# Load packages for data analysis
library(tidyverse)
library(scales)
theme_set(theme_bw(
  base_size = 14
))

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
  filter(age != "< 25" & !is.na(age))

# Create function to streamline code
plot_mean_by_group_over_time <- function(data, var, group) {
  data |>
    summarize(
      .by = c(year, {{ group }}),
      mean = weighted.mean(!!sym(var), wgt, na.rm = TRUE)
    ) |>
    ggplot(aes_string(x = "year", y = "mean", color = {{ group }})) +
    geom_line(alpha = 0.75, linewidth = 0.75) +
    geom_point(alpha = 0.75, size = 2.25) +
    labs(x = "Year")
}

groups <- c(
  "Race" = "race",
  "Age group" = "age",
  "Income quintile" = "income_quantiles",
  "Education level" = "education",
  "College status" = "college"
)

variables <- c(
  "Labor force participation rate" = "lfp_lgl",
  "Average total income" = "inctot",
  "Average earned income" = "income",
  "Average social insurance income" = "incss",
  "Proportion with college degree" = "college_lgl"
)

dollar_vars <- c("inctot", "income", "incss")

ui <- page_sidebar(
  title = "Selected data from the U.S. Current Population Survey",
  sidebar = sidebar(
    selectInput(
      inputId = "var",
      label = "Variable to chart over time",
      choices = variables
    ),
    selectInput(
      inputId = "group",
      label = "Variable to group by",
      choices = groups
    )
  ),
  plotOutput(outputId = "lfp_chart")
)

server <- function(input, output) {
  chart <- reactive({
    chart <- women_over_25 |>
      plot_mean_by_group_over_time(input$var, input$group) +
      labs(
        y = names(which(variables == input$var)),
        color = names(which(groups == input$group))
      )
    if(input$var %in% dollar_vars) {
      chart <- chart + scale_y_continuous(labels = dollar)
    } else {
      chart <- chart + scale_y_continuous(labels = percent)
    }
    chart
  })
  output$lfp_chart <- renderPlot({chart()})
}

shinyApp(ui = ui, server = server)
