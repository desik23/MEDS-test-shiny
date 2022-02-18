# load packages ----
library(shiny)
library(palmerpenguins)
library(tidyverse)
library(shinyWidgets)
library(bslib)

# import data ----
temp_summary <- readRDS("data/temp_month_summary.rds")

# user interface ----
ui <- fluidPage(theme = bslib::bs_theme(bootswatch = "yeti"),
  navbarPage(
    "Exploring Antarctic Penguins & Weather",
    tabPanel("Background",
             em("some background information here")),
    tabPanel("Antarctic Penguins",
             tabsetPanel(
               tabPanel("Scatterplot",
                        em(  # body mass slider input ----
                             sliderInput(inputId = "body_mass",
                                         label =  "Select a range of body masses (g):",
                                         min = 2700, max = 6300, value = c(3000, 4000)),
                             
                             # body mass output ----
                             plotOutput(outputId = "bodyMass_scatterPlot"))),
               tabPanel("Histogram",
                        em( # island input ----
                        pickerInput(inputId = "island", label = "Select an island:",
                        choices = c("Torgersen", "Dream", "Biscoe"),
                        selected = c("Torgersen", "Dream", "Biscoe"),
                        multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE)),
                        # bin number input ---- 
                        sliderInput(inputId = "bin_num", label = "Select number of bins:",
                                    min = 1, max = 100, value = 25), 
  # flipper length plot output ----
  plotOutput(outputId = "flipperLength_hist"))))),
    tabPanel("Antarctic Weather",
             em("some widget to explore weather data here")),
    tabPanel("Explore the Data",
             tabsetPanel(
               tabPanel("Penguin Data",
                        em(  # penguin data table output
                          DT::dataTableOutput(outputId = "penguin_data"))),
               tabPanel("Palmer Station Weather Data",
                        em(# weather checkboxGroupInput ----
                           checkboxGroupInput(
                             inputId = "month", label = "Choose a month(s):",
                             choices = c("January", "February", "March", "April",
                                         "May", "June", "July", "August",
                                         "September", "October", "November", "December"),
                             selected = c("January", "February")
                           ),
                           # weather table output----
                           DT::dataTableOutput(outputId = "temp_table")))))
  )
)


# server instructions ----
server <- function(input, output) {
  
  # filter body masses ----  
  body_mass_df <- reactive({ 
    penguins %>% 
      filter(body_mass_g %in% input$body_mass[1]:input$body_mass[2])
  })
  # 
  # render scatterplot ----
  output$bodyMass_scatterPlot <- renderPlot({ 

  # code to generate scatterplot here
    ggplot(na.omit(body_mass_df()), 
           aes(x = flipper_length_mm, y = bill_length_mm,
               color = species, shape = species)) +
      geom_point(size = 4) +
      scale_color_manual(values = c("Adelie" = "#FEA346", "Chinstrap" = "#B251F1", "Gentoo"= "#4BA4A4")) +
      scale_shape_manual(values = c("Adelie" = 19,
                                    "Chinstrap" = 17,
                                    "Gentoo" = 15)) +
      labs(x = "Flipper length (mm)", y = "Bill length (mm)",
           color = "Penguin species", shape = "Penguin species") +
      theme_minimal() + 
      theme(legend.position = c(0.85, 0.2), 
            legend.background = element_rect(color = "white"))

})
  # render the penguins data table ----
  output$penguin_data <- DT::renderDataTable({
    DT::datatable(penguins,
                  options = list(pageLength = 5),
                  caption = tags$caption(
                    style = 'caption-side: top; text-align: left;',
                    'Table 1: ', tags$em('Size measurements for adult foraging penguins near Palmer Station, Antarctica')))
  })
  # filter island data ----
  island_df <- reactive({
    validate( # validate() tests a condition, returns error if test fails-
      # need() takes an exp that returns T or F + chr string to return if exp is FALSE 
      need(length(input$island) > 0, "Please select at least one island to visualize.") 
    ) 
    
    penguins %>% 
      filter(island == input$island)
  })
  # render the flipper length histogram ----
  output$flipperLength_hist <- renderPlot({
    ggplot(na.omit(island_df()), aes(x = flipper_length_mm, fill = species)) +
      geom_histogram(alpha = 0.6, bins = input$bin_num) +
      scale_fill_manual(values = c("Adelie" = "#FEA346", "Chinstrap" = "#B251F1", "Gentoo" = "#4BA4A4")) +
      labs(x = "Flipper length (mm)", y = "Frequency", 
           fill = "Penguin species") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.background = element_rect(color = "white"))
  })
  # ~ previous code omitted for brevity ~
  # filter weather data by month ----
  month_df <- reactive({
    temp_summary %>% 
      filter(month_name %in% input$month)
  })
  # render the temperature data table ----
  output$temp_table <- DT::renderDataTable({
    DT::datatable(month_df(),
                  class = 'cell-border stripe',
                  colnames = c('Year', "Month", 'Mean Air Temp.', 'Max. Air Temp.', 'Min. Air Temp.'),
                  caption = tags$caption(
                    style = 'caption-side: top; text-align: left;',
                    'Table 2: ', tags$em('Mean, maximum, and minimum monthly air temperatures (°C) recorded at Palmer Station, Antarctica from 1989 - 2019.'))) 
  })

}
  

# combine UI & server into an app ----
shinyApp(ui = ui, server = server)
