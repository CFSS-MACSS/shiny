# Load packages -----------------------------------------------------

library(shiny)
library(tidyverse)
library(colorblindr)
library(scales)
library(countrycode)

# Load data ---------------------------------------------------------
manager_survey <- read_csv("data/survey.csv",
  na = c("", "NA"),
  show_col_types = FALSE
)

# perform some basic data cleaning
manager_survey <- manager_survey %>%
  # remove NAs for industry and education
  drop_na(industry, highest_level_of_education_completed) %>%
  # only keep US dollars
  filter(currency == "USD") %>%
  # clean up the industry, country, and education variables
  mutate(
    industry_other = fct_lump_min(industry, min = 100),
    country = countrycode(country, origin = "country.name", destination = "cldr.name.en"),
    highest_level_of_education_completed = fct_relevel(
      highest_level_of_education_completed,
      "High School",
      "Some college",
      "College degree",
      "Master's degree",
      "Professional degree (MD, JD, etc.)",
      "PhD"
    ),
    highest_level_of_education_completed = fct_recode(
      highest_level_of_education_completed,
      "Professional degree" = "Professional degree (MD, JD, etc.)"
    )
  )

# extract all distinct industries as a character vector
industry_choices <- manager_survey %>%
  distinct(industry_other) %>%
  arrange(industry_other) %>%
  pull(industry_other)

# randomly sample 3 starter industries - note we are not using set.seed()
selected_industry_choices <- sample(industry_choices, 3)

# Define UI ---------------------------------------------------------
ui <- fluidPage()

# Define server function --------------------------------------------
server <- function(input, output, session) {

}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
