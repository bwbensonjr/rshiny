library(shiny)
library(tidyverse)
POLLS_URL <- "https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv"

CANDIDATE_LIST_ALL <- c(
    "Biden",
    "Warren",
    "Sanders",
    "Buttigieg",
    "Klobuchar",
    "Bloomberg",
    "Booker",
    "Yang",
    "Castro",
    "Gabbard",
    "Steyer",
    "Bennet",
    "Delaney",
    "Williamson",
    "Patrick"
)

CANDIDATE_LIST_SMALL <- c(
    "Biden",
    "Warren",
    "Sanders",
    "Buttigieg",
    "Klobuchar",
    "Bloomberg"
)

polls <- read_csv(POLLS_URL,
                  col_types = cols(
                      partisan = col_character(),
                      notes = col_character(),
                      start_date = col_date("%m/%d/%y"),
                      end_date = col_date("%m/%d/%y"))) %>%
    mutate(state = replace_na(state, "National")) %>%
    filter(party == "DEM") %>%
    filter(answer %in% CANDIDATE_LIST_ALL)

states <- sort(unique(polls$state))

ui <- fluidPage(
    titlePanel("Democratic Primary Polls 2020"),
    a("Data from fivethirtyeight.com", href=POLLS_URL, target="_blank"),
    sidebarLayout(
        sidebarPanel(
            dateInput("start_date", "Start Date", "2019-11-01"),
            checkboxGroupInput("candidates", "Candidates", CANDIDATE_LIST_ALL, CANDIDATE_LIST_SMALL),
            selectInput("state", "State", states, "National"),
            width=3
            ),
        mainPanel(plotOutput("poll_graph", height=470),
                  br(),
                  br(),
                  tableOutput("poll_table"))
    )
)
server <- function(input, output) {
    output$poll_graph <- renderPlot({
        polls %>%
            filter(end_date > input$start_date) %>%
            filter(state == input$state) %>%
            filter(answer %in% input$candidates) %>%
            ggplot(aes(x=end_date, y=pct, color=answer)) +
            geom_point() +
            geom_smooth(method="loess") +
            ggtitle(input$state) +
            theme_grey(base_size=15)
    })
    output$poll_table <- renderTable({
        filtered <- polls %>%
            filter(end_date > input$start_date) %>%
            filter(state == input$state) %>%
            group_by(poll_id) %>%
            slice(poll_id, 1:1) %>%
            ungroup() %>%
            select(pollster,
                   url,
                   fte_grade,
                   state,
                   start_date,
                   end_date,
                   methodology,
                   population,
                   sample_size) %>%
            mutate(pollster = paste0("<a href='", url, "' target='_blank'>", pollster, "</a>"),
                   start_date = as.character(start_date),
                   end_date = as.character(end_date)) %>% 
            select(-url) %>%
            arrange(desc(end_date))
        filtered
        }, sanitize.text.function = function(x) x)
}
shinyApp(ui = ui, server = server)
