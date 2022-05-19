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
ui <- fluidPage(
  # add a title panel
  titlePanel(title = "Ask a Manager"),
  # use sidebar layout
  sidebarLayout(
    # create panel for inputs
    sidebarPanel(
      checkboxGroupInput(
        inputId = "industry",
        label = "Select up to 8 industies:",
        choices = industry_choices,
        selected = selected_industry_choices
      ),
    ),
    # create main panel
    mainPanel(
      # use HTML tags to format structure of text
      hr(),
      "Showing only results for those with salaries in USD who have provided
      information on their industry and highest level of education completed.",
      br(), br(),
      # placeholder for selected industries
      textOutput(outputId = "selected_industries"),
      hr(),
      br(),
      # use a tabset for the main content
      tabsetPanel(
        type = "tabs",
        # average salaries plot
        tabPanel(title = "Average salaries", plotOutput(outputId = "avg_salary_plot")),
        # individual salaries tab
        tabPanel(
          title = "Individual salaries",
          # add conditional panel to select observations based on salary
          conditionalPanel(
            condition = "input.industry.length <= 8",
            sliderInput(
              inputId = "ylim",
              label = "Zoom in to salaries between",
              min = 0,
              value = c(0, 1000000),
              max = max(manager_survey$annual_salary),
              width = "100%",
              pre = "$"
            )
          ),
          # plot and table for this panel
          plotOutput(outputId = "indiv_salary_plot", brush = "indiv_salary_brush"),
          tableOutput(outputId = "indiv_salary_table")
        ),
        # show all the data
        tabPanel("Data", DT::dataTableOutput(outputId = "data"))
      )
    )
  )
)

# Define server function --------------------------------------------
server <- function(input, output, session) {

}

# sample code we will convert into reactive plots
indiv_salary_plot <- ggplot(
  data = manager_survey,
  mapping = aes(
    x = highest_level_of_education_completed,
    y = annual_salary,
    color = industry_other
  )
) +
  geom_jitter(size = 2, alpha = 0.6) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom") +
  scale_color_OkabeIto() +
  scale_y_continuous(
    labels = label_dollar()
  ) +
  labs(
    x = "Highest level of education completed",
    y = "Annual salary",
    color = "Industry",
    title = "Individual salaries"
  )

avg_salary_plot <- manager_survey %>%
  group_by(industry_other, highest_level_of_education_completed) %>%
  summarise(
    mean_annual_salary = mean(annual_salary, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # draw the plot
  ggplot(
    mapping = aes(
      x = highest_level_of_education_completed,
      y = mean_annual_salary,
      group = industry_other,
      color = industry_other
    )
  ) +
  geom_line(size = 1) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom") +
  scale_color_OkabeIto() +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    x = "Highest level of education completed",
    y = "Mean annual salary",
    color = "Industry",
    title = "Average salaries"
  )


# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
