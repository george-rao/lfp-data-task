library(shiny)
library(bslib)

# Load packages for data analysis
library(tidyverse)
library(scales)
theme_set(theme_bw(
  base_size = 14
))

# Import data and reorder factors
women_over_25_by_year <- read_csv("data/women-over-25-by-year.csv") |>
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
    income_quantiles = fct_relevel(
      income_quantiles, "0-19.99", "20-39.99",
      "40-59.99", "60-79.99", "80-100"
    )
  )

# Create function to streamline code
plot_mean_by_group_over_time <- function(data, var, group) {
  data |>
    filter(!is.na(!!sym(group))) |>
    summarize(
      .by = c(year, {{ group }}),
      mean = weighted.mean(!!sym(var), !!sym(str_c(var, "_wgt")), na.rm = TRUE)
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
  title = "Data on women over 25 from the U.S. CPS",
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
    ),
    radioButtons(
      inputId = "facet_lgl",
      label = "Type of chart",
      choices = c(
        "Combined" = FALSE,
        "Faceted" = TRUE
      )
    ),
  ),
  plotOutput(outputId = "lfp_chart")
)

server <- function(input, output) {
  chart <- reactive({
    chart <- women_over_25_by_year |>
      plot_mean_by_group_over_time(input$var, input$group) +
      labs(
        y = names(which(variables == input$var)),
        color = names(which(groups == input$group))
      )
    if (input$var %in% dollar_vars) {
      chart <- chart + scale_y_continuous(labels = dollar)
    } else {
      chart <- chart + scale_y_continuous(labels = percent)
    }
    if (input$facet_lgl) {
      chart <- chart +
        facet_wrap(vars(!!sym(input$group)), ncol = 3) +
        theme(legend.position = "none")
    }
    chart
  })
  output$lfp_chart <- renderPlot({
    chart()
  })
}

shinyApp(ui = ui, server = server)
