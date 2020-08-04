library(shiny)
library(tidyverse)
library(jsonlite)
library(lubridate)

state_covid_cases <- fromJSON("https://covidtracking.com/api/states/daily") %>%
    as_tibble() %>%
    mutate(date = ymd(date)) %>%
    select(-hash, -dateChecked, -fips, -dataQualityGrade, -lastUpdateEt) %>%
    select(-dateModified, -checkTimeEt, -grade) %>%
    pivot_longer(c(-date, -state), names_to="Category", values_to="Count")

us_totals <- state_covid_cases %>%
    group_by(date, Category) %>%
    summarize(state = "US", Count = sum(Count, na.rm=TRUE), .groups="drop")

covid_cases <- state_covid_cases %>% bind_rows(us_totals)
us_states <- sort(unique(covid_cases$state))
categories <- unique(covid_cases$Category)

ui <- fluidPage(
    titlePanel("US Covid-19 Counts"),
    a("Data from covidtracking.com", href="https://covidtracking.com/api/states/daily"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput("category", "Category", categories, "deathIncrease"),
            selectizeInput("state", "State",
                           us_states,
                           c("US"),
                           multiple=TRUE),
            width=3
            ),
        mainPanel(plotOutput("covid_timeline_graph", height=470),
                  br(),
                  plotOutput("covid_bar_graph", height=470),
                  br(),
                  tableOutput("covid_table"))
    )
)
server <- function(input, output) {
    output$covid_timeline_graph <- renderPlot({
        covid_cases %>%
            filter(state %in% input$state) %>%
            filter(Category == input$category) %>%
            ggplot(aes(x=date, y=Count, color=state)) +
            geom_line() +
            scale_y_continuous(labels=function(x) format(x, scientific=FALSE)) +
            theme_grey(base_size=15)
    })
    output$covid_bar_graph <- renderPlot({
        covid_cases %>%
            filter(Category == input$category) %>%
            filter(date == max(date)) %>%
            ggplot(aes(x=reorder(state, -Count), y=Count, fill=state, color=state)) +
            geom_col(show.legend=FALSE) +
            xlab("") +
            scale_y_continuous(labels=function(x) format(x, scientific=FALSE)) +
            theme_grey(base_size=12) +
            theme(axis.text.x = element_text(angle = 90))
    })
    output$covid_table <- renderTable({
        covid_cases %>%
            filter(state %in% input$state) %>%
            filter(Category == input$category) %>%
            pivot_wider(names_from="state", values_from="Count") %>%
            arrange(desc(date)) %>%
            mutate(date = as.character(date))
    })
}
shinyApp(ui = ui, server = server)
